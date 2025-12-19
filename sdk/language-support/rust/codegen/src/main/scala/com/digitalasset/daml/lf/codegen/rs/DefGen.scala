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
) extends DefGen {

  private val templateId = s"${moduleId.pkg}:${moduleId.moduleName}:$name"

  override def renderRust(b: CodeBuilder): Unit = {
    val keyType = keyTypeOpt
      .map(t => TypeGen.renderType(moduleId, t))
      .getOrElse("()") // Unit if no key

    b.addEmptyLine()
    b.addBlock(s"impl daml_types::Template for $name {", "}") {
      b.addLine(s"type Key = $keyType;")

      b.addBlock("fn template_id() -> &'static str {", "}") {
        b.addLine(s""""$templateId"""")
      }
    }

    // Render the choices associated with this template
    choices.foreach(_.renderRust(moduleId, name, b))
  }
}

/** Handles Template Keys defined in separate namespaces (if applicable).
  */
private[codegen] final case class TemplateNamespaceGen(
    moduleId: ModuleId,
    name: Name,
    key: Ast.Type,
) extends DefGen {
  override def renderRust(b: CodeBuilder): Unit = {
    // In Rust, we might just define a type alias if strictly necessary,
    // but usually the Key type is defined in the impl block.
    // We can emit a helper type alias if beneficial.
    val keyType = TypeGen.renderType(moduleId, key)
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
) extends DefGen {

  override def renderRust(b: CodeBuilder): Unit = {
    b.addEmptyLine()

    // Generics handling: <A, B>
    val typeParams = if (paramNames.isEmpty) "" else s"<${paramNames.mkString(", ")}>"

    // Add Serde attributes
    b.addLine("#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]")
    b.addLine("#[serde(rename_all = \"camelCase\")]") // Daml JSON API uses camelCase

    cons match {
      case Ast.DataRecord(fields) =>
        b.addBlock(s"pub struct $name$typeParams {", "}") {
          fields.foreach { case (fieldName, tpe) =>
            b.addLine(s"pub $fieldName: ${TypeGen.renderType(moduleId, tpe)},")
          }
        }

      case Ast.DataVariant(variants) =>
        b.addBlock(s"pub enum $name$typeParams {", "}") {
          variants.foreach { case (variantName, tpe) =>
            // Rust Enum Variants with data
            b.addLine(s"$variantName(${TypeGen.renderType(moduleId, tpe)}),")
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
) {

  def renderRust(moduleId: ModuleId, templateName: Name, b: CodeBuilder): Unit = {
    val choiceStructName = name // The choice name itself is usually unique within the module scope
    val argTypeStr = TypeGen.renderType(moduleId, argType)
    val retTypeStr = TypeGen.renderType(moduleId, returnType)

    b.addEmptyLine()
    // 1. Define the Choice Payload Struct
    // Note: If the argument type is Unit (empty record), we still generate a struct
    // or we alias it. Usually safest to generate a struct for Serde.

    b.addLine("#[derive(Debug, Clone, Serialize, Deserialize)]")
    b.addLine("#[serde(rename_all = \"camelCase\")]")

    // Check if argType is a Record or strict primitive.
    // For simplicity here, we wrap the argument type.
    // In idiomatic Daml-Rust, generated choices often mirror the fields of the Daml choice.
    // If 'argType' refers to a specific named record, we can use a type alias.
    // If it is a built-in like Unit, we use a unit struct.

    b.addLine(s"pub struct $choiceStructName(pub $argTypeStr);")

    // 2. Implement the Choice Trait
    b.addEmptyLine()
    b.addBlock(s"impl daml_types::Choice<$templateName> for $choiceStructName {", "}") {
      b.addLine(s"type Return = $retTypeStr;")

      b.addBlock("fn name() -> &'static str {", "}") {
        b.addLine(s""""$name"""")
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
) extends DefGen {

  override def renderRust(b: CodeBuilder): Unit = {
    // Generate a Trait for the Interface
    b.addEmptyLine()
    b.addBlock(s"pub trait $name: daml_types::Template {", "}") {
      b.addLine(s"// Interface view: ${TypeGen.renderType(moduleId, Ast.TTyCon(view))}")
    }

    // We might also generate the Choices associated with this Interface
    // Note: In Rust, these choices would be generic over <T: InterfaceName>
    choices.foreach { choice =>
      // Logic to render generic choices would go here
      b.addLine(s"// Choice ${choice.name} for interface $name omitted for brevity")
    }
  }
}
