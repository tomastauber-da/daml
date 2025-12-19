// Copyright (c) 2025 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.daml.lf.codegen.rs

import com.digitalasset.daml.lf.data.Ref.{ModuleId, QualifiedName, TypeConId}
import com.digitalasset.daml.lf.language.Ast
import com.digitalasset.daml.lf.language.Util

private object TypeGen {

  /**
   * Generates the Rust type signature for a given Daml AST type.
   * Assumes the existence of a `daml_types` crate or module containing
   * standard wrapper types (Party, ContractId, etc.).
   */
  def renderType(currentModule: ModuleId, tpe: Ast.Type): String = {
    def rec(tpe: Ast.Type): String =
      tpe match {
        // Generic Type Variables (e.g., T)
        case Ast.TVar(name) => name

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
          val renderTCon = renderTypeCon(currentModule, tcon)
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

  /**
   * Resolves the Rust path to a Type Constructor (Template or Record).
   */
  def renderTypeCon(currentModule: ModuleId, typeCon: TypeConId): String = {
    // Rust uses `::` for namespace separation.
    // We assume external packages are mapped to `crate::pkg_{ID}::...`
    // or similar patterns.

    if (currentModule.pkg != typeCon.pkg) {
      // Reference to a type in an external package
      s"crate::pkg_${typeCon.pkg}::${pathName(typeCon.qualifiedName)}"
    } else if (currentModule.moduleName != typeCon.qualifiedName.module) {
      // Reference to a type in the same package but different module.
      // We use `crate::` (absolute path) to be safe and avoid relative path hell.
      s"crate::${pathName(typeCon.qualifiedName)}"
    } else {
      // Reference to a type in the current module
      typeCon.qualifiedName.name.dottedName
    }
  }

  private def pathName(qualifiedName: QualifiedName): String = {
    // Daml modules (My.Module) -> Rust modules (my::module)
    // You might want to apply .toLowerCase to segments here for idiomatic Rust snake_case.
    val modules = qualifiedName.module.segments.toSeq.map(_.toLowerCase).mkString("::")
    s"${modules}::${qualifiedName.name}"
  }

  private def error(msg: String): Nothing = throw new RuntimeException("IMPOSSIBLE: " + msg)
}