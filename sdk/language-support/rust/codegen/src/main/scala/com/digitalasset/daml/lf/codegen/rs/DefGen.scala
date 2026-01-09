// Copyright (c) 2025 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.daml.lf.codegen.rs

import com.digitalasset.daml.lf.data.Ref._
import com.digitalasset.daml.lf.language.Ast

/** Base trait for Rust definition generators.
  * Unlike TS/JS, we only have one render phase per definition.
  */
private[codegen] sealed trait DefGen {
  def renderRust(b: CodeBuilder): Unit
}

/** Generates a Rust module (mod).
  */
private[codegen] final case class NamespaceGen(name: Name, definitions: Seq[DefGen])
    extends DefGen {
  override def renderRust(b: CodeBuilder): Unit = {
    // Rust modules are snake_case.
    // We assume 'name' is the Daml module name (CamelCase), so we lower-case it.
    // Note: You might need a more robust camelToSnake utility function here.
    val modName = name.toLowerCase

    b.addEmptyLine()
    b.addBlock(s"pub mod $modName {", "}") {
      // Import common types from the parent/root
      b.addLine("use super::*;")
      b.addLine("use daml_types::* ;")
      b.addLine("use serde::{Serialize, Deserialize};")

      definitions.foreach(_.renderRust(b))
    }
  }
}

/** Generates the `impl Template` block.
  * Note: The actual struct definition for the Template payload is handled by TypeConGen.
  */
private[codegen] final case class TemplateGen(
    moduleId: ModuleId,
    packageName: PackageName,
    name: Name,
    // Decoders/Encoders removed - Serde handles this.
    keyTypeOpt: Option[Ast.Type], // We need the Type, not the Decoder
    choices: Seq[ChoiceGen],
    implements: Seq[TypeConId],
    pkgIdToName: Map[PackageId, String],
) extends DefGen {

  private val templateId = s"${moduleId.pkg}:${moduleId.moduleName}:$name"

  override def renderRust(b: CodeBuilder): Unit = {
    val keyType = keyTypeOpt
      .map(t => TypeGen.renderType(moduleId, t, pkgIdToName))
      .getOrElse("()") // Unit if no key

    b.addEmptyLine()
    b.addBlock(s"impl daml_types::DamlType for $name {", "}") {
      b.addBlock("fn type_id() -> &'static str {", "}") {
        b.addLine(s""""$templateId"""")
      }
    }

    b.addEmptyLine()
    b.addBlock(s"impl daml_types::Template for $name {", "}") {
      b.addLine(s"type Key = $keyType;")
    }

    // Render the choices associated with this template
    choices.foreach(_.renderRust(moduleId, Left(name), b))
  }
}

/** Handles Template Keys defined in separate namespaces (if applicable).
  */
private[codegen] final case class TemplateNamespaceGen(
    moduleId: ModuleId,
    name: Name,
    key: Ast.Type,
    pkgIdToName: Map[PackageId, String],
) extends DefGen {
  override def renderRust(b: CodeBuilder): Unit = {
    // In Rust, we might just define a type alias if strictly necessary,
    // but usually the Key type is defined in the impl block.
    // We can emit a helper type alias if beneficial.
    val keyType = TypeGen.renderType(moduleId, key, pkgIdToName)
    b.addLine(s"pub type ${name}Key = $keyType;")
  }
}

/** Generates the main Data Types (Structs and Enums).
  */
private[codegen] final case class TypeConGen(
    moduleId: ModuleId,
    name: Name,
    paramNames: Seq[Ast.TypeVarName],
    cons: Ast.DataCons,
    pkgIdToName: Map[PackageId, String],
) extends DefGen {

  // Helper to detect recursion and wrap in Box<T>
  private def renderFieldType(tpe: Ast.Type): String = {
    val rustType = TypeGen.renderType(moduleId, tpe, pkgIdToName)

    // Check if the field type is the same as the struct/enum we are currently generating.
    // This is a naive check; for complex mutual recursion, we'd need a deeper analysis pass,
    // but this covers 99% of Daml recursive types (lists/trees).
    // Note: We strip generic parameters "<...>" for the name comparison.
    val bareType = rustType.takeWhile(_ != '<')

    if (bareType == name) s"Box<$rustType>" else rustType
  }

  override def renderRust(b: CodeBuilder): Unit = {
    b.addEmptyLine()

    // Generics handling: <A, B>
    val typeParams =
      if (paramNames.isEmpty) ""
      else {
        val paramsWithBounds = paramNames.map { p =>
          val upperP = p.capitalize
          s"$upperP: daml_types::Data + serde::Serialize + serde::de::DeserializeOwned"
        }
        s"<${paramsWithBounds.mkString(", ")}>"
      }

    // Add Serde attributes
    b.addLine("#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]")
    b.addLine("#[serde(rename_all = \"camelCase\")]") // Daml JSON API uses camelCase

    if (paramNames.nonEmpty) {
      val boundStr = paramNames
        .map { p =>
          s"${p.capitalize}: daml_types::Data + serde::Serialize + serde::de::DeserializeOwned"
        }
        .mkString(", ")
      b.addLine(s"""#[serde(bound = "$boundStr")]""")
    }

    cons match {
      case Ast.DataRecord(fields) =>
        b.addBlock(s"pub struct $name$typeParams {", "}") {
          fields.foreach { case (fieldName, tpe) =>
            b.addLine(s"pub $fieldName: ${renderFieldType(tpe)},")
          }
        }

      case Ast.DataVariant(variants) =>
        b.addBlock(s"pub enum $name$typeParams {", "}") {
          variants.foreach { case (variantName, tpe) =>
            b.addLine(s"$variantName(${renderFieldType(tpe)}),")
          }
        }

      case Ast.DataEnum(constructors) =>
        b.addBlock(s"pub enum $name$typeParams {", "}") {
          constructors.foreach(cName => b.addLine(s"$cName,"))
        }

      case Ast.DataInterface =>
        // Interfaces in Rust are tricky. For data generation, we might generate a wrapper
        // or simply skip strictly defining the data layout if it's dynamic.
        // Often represented as a Trait, but here we are generating Data definitions.
        b.addLine(s"// Interface $name not fully supported in simple codegen yet.")
    }
  }
}

/** Generates Choice Structs and Implementations.
  */
private[codegen] final case class ChoiceGen(
    name: Name,
    argType: Ast.Type,
    returnType: Ast.Type,
    pkgIdToName: Map[PackageId, String],
) {

  def renderRust(
      moduleId: ModuleId,
      templateNameOrInterfaceName: Either[Name, String],
      b: CodeBuilder,
  ): Unit = {
    val argTypeStr = TypeGen.renderType(moduleId, argType, pkgIdToName)
    val retTypeStr = TypeGen.renderType(moduleId, returnType, pkgIdToName)

    val choiceParentName = templateNameOrInterfaceName match {
      case Left(n) => n.toString
      case Right(n) => n
    }

    // The "Archive" choice is implicit on all templates.
    // We cannot generate a "struct Archive" every time, or we get duplicate definitions.
    // Instead, we implement the Choice trait directly on the external argument type
    // (da::internal::template::Archive).
    if (name == "Archive") {
      b.addEmptyLine()
      b.addBlock(s"impl daml_types::Choice<$choiceParentName> for $argTypeStr {", "}") {
        b.addLine(s"type Return = $retTypeStr;")
        b.addBlock("fn name() -> &'static str {", "}") {
          b.addLine(s""""$name"""")
        }
      }
      // Stop here, do not generate a struct
    } else {

      // Check if the argument type name matches the choice name.
      // Note: We strip potential generic params or module prefixes for strict name comparison if needed,
      // but usually exact string match is sufficient for local types.
      val isRedundantWrapper = argTypeStr == name

      b.addEmptyLine()

      if (!isRedundantWrapper) {
        // Only generate the wrapper struct if the names differ (e.g., choice takes a primitive Int)
        b.addLine("#[derive(Debug, Clone, Serialize, Deserialize)]")
        b.addLine("#[serde(rename_all = \"camelCase\")]")
        b.addLine(s"pub struct $name(pub $argTypeStr);")
      }

      // Implement the Choice Trait
      // If isRedundantWrapper is true, we are implementing it on the existing payload struct.
      b.addEmptyLine()
      b.addBlock(s"impl daml_types::Choice<$choiceParentName> for $name {", "}") {
        b.addLine(s"type Return = $retTypeStr;")

        b.addBlock("fn name() -> &'static str {", "}") {
          b.addLine(s""""$name"""")
        }
      }
    }
  }
}

// Interfaces are usually handled by generating a Marker trait or specific logic.
// For this snippet, we will stub it out or treat it similarly to Templates.
private[codegen] final case class InterfaceGen(
    moduleId: ModuleId,
    packageName: PackageName,
    name: String,
    choices: Seq[ChoiceGen],
    view: TypeConId,
    pkgIdToName: Map[PackageId, String],
) extends DefGen {

  override def renderRust(b: CodeBuilder): Unit = {
    // 1. Resolve the View Type
    val viewType = TypeGen.renderType(moduleId, Ast.TTyCon(view), pkgIdToName)

    // 2. Generate the Marker Struct
    // This represents the Interface in ContractId<FeaturedAppRight>
    b.addEmptyLine()
    b.addLine("#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]")
    b.addLine(s"pub struct $name;") // Unit struct

    // 3. Implement Common Trait
    b.addEmptyLine()
    b.addBlock(s"impl daml_types::DamlType for $name {", "}") {
      b.addBlock("fn type_id() -> &'static str {", "}") {
        // Reconstruct ID: package:module:name
        val ifaceId = s"${moduleId.pkg}:${moduleId.moduleName}:$name"
        b.addLine(s""""$ifaceId"""")
      }
    }

    // 3. Implement Interface Trait
    b.addEmptyLine()
    b.addBlock(s"impl daml_types::Interface for $name {", "}") {
      b.addLine(s"type View = $viewType;")
    }

    // 4. Generate the Choices for this Interface
    // We pass the struct name ($name) as the 'template' type for the Choice trait
    choices.foreach { choice =>
      choice.renderRust(moduleId, Right(name), b)
    }
  }
}
