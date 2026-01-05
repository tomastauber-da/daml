// Copyright (c) 2025 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.daml.lf.codegen.rs

import com.digitalasset.daml.lf.data.Ref._
import com.digitalasset.daml.lf.language.Ast
import com.digitalasset.daml.lf.language.Util

private object TypeGen {

  /** Generates the Rust type signature for a given Daml AST type.
    * Assumes the existence of a `daml_types` crate or module containing
    * standard wrapper types (Party, ContractId, etc.).
    */
  def renderType(
      currentModule: ModuleId,
      tpe: Ast.Type,
      pkgIdToName: Map[PackageId, String],
  ): String = {
    def rec(tpe: Ast.Type): String =
      tpe match {
        // Generic Type Variables (e.g., T)
        case Ast.TVar(name) => name.capitalize

        // --- Primitives ---
        // Unit in Rust is `()` or a specific struct if empty-object JSON is required.
        // Assuming `daml_types::Unit` from previous implementation.
        case Util.TUnit => "daml_types::Unit"
        case Util.TBool => "bool"
        // Daml Ints are serialized as Strings in JSON to avoid precision loss.
        // Use a wrapper or standard i64 depending on strictness requirements.
        case Util.TInt64 => "daml_types::Int"
        case Util.TText => "String"
        case Util.TTimestamp => "daml_types::Time"
        case Util.TParty => "daml_types::Party"
        case Util.TDate => "daml_types::Date"
        case Ast.TBuiltin(bt) => error(s"partially applied primitive type not serializable - $bt")

        // --- Type Applications ---
        case Util.TNumeric(_: Ast.TNat) => "daml_types::Numeric"

        // Lists become Vectors
        case Util.TList(targ) => s"Vec<${rec(targ)}>"

        // Optional becomes Option
        case Util.TOptional(targ) => s"Option<${rec(targ)}>"

        // TextMap is a standard Hash map
        case Util.TTextMap(targ) => s"std::collections::HashMap<String, ${rec(targ)}>"

        // GenMap is serialized as a list of entries [[k,v],..].
        // Using the wrapper defined in the previous prompt.
        case Util.TGenMap(karg, varg) => s"daml_types::GenMap<${rec(karg)}, ${rec(varg)}>"

        // ContractId wrapper
        case Util.TContractId(targ) => s"daml_types::ContractId<${rec(targ)}>"

        case Util.TUpdate(_) => error("Update not serializable")

        case Util.TTyConApp(tcon, targs) =>
          val renderTCon = renderTypeCon(currentModule, tcon, pkgIdToName)
          if (targs.isEmpty) renderTCon
          else s"$renderTCon<${targs.toSeq.map(rec).mkString(", ")}>"

        case _: Ast.TApp => error(s"type application not serializable - $tpe")

        // --- Others ---
        case _: Ast.TTyCon => error("lonely type constructor")
        case _: Ast.TSynApp => error("type synonym not serializable")
        case _: Ast.TForall => error("universally quantified type not serializable")
        case _: Ast.TStruct => error("structural record not serializable")
        case _: Ast.TNat => error("standalone type level natural not serializable")
      }

    rec(tpe)
  }

  /** Resolves the Rust path to a Type Constructor (Template or Record).
    */
  def renderTypeCon(
      currentModule: ModuleId,
      typeCon: TypeConId,
      pkgIdToName: Map[PackageId, String],
  ): String = {
    if (currentModule.pkg != typeCon.pkg) {
      // External Package
      val pkgName = pkgIdToName.getOrElse(typeCon.pkg, s"pkg_${typeCon.pkg}")

      // FIX: Do not use the nested structure (da::internal::template).
      // Use the flattened module name (da_internal_template) that matches the filename.
      val flatModuleName = sanitizeModuleName(typeCon.qualifiedName.module.toString)

      s"crate::$pkgName::$flatModuleName::${typeCon.qualifiedName.name}"

    } else if (currentModule.moduleName != typeCon.qualifiedName.module) {
      // Local Package, different module
      // Use flattened name here too
      val flatModuleName = sanitizeModuleName(typeCon.qualifiedName.module.toString)
      s"crate::$flatModuleName::${typeCon.qualifiedName.name}"
    } else {
      // Same Module
      typeCon.qualifiedName.name.dottedName
    }
  }
  // Ensure this matches the logic in RustCodeGen.scala exactly
  private def sanitizeModuleName(name: String): String = {
    val sanitized = name.toLowerCase.replaceAll("[^a-z0-9_]", "_")
    if (sanitized.matches("^[0-9].*")) s"_$sanitized" else sanitized
  }

  private def error(msg: String): Nothing = throw new RuntimeException("IMPOSSIBLE: " + msg)
}
