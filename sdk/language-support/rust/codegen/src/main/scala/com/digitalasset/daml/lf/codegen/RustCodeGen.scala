// Copyright (c) 2025 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.daml.lf.codegen

import com.digitalasset.daml.lf.codegen.rs._
import java.nio.file.{Files, Path}
import scala.collection.immutable.Map
import com.digitalasset.daml.lf.archive.{DamlLf, DarParser}
import com.digitalasset.daml.lf.data.Ref._
import com.digitalasset.daml.lf.language.Ast
import com.digitalasset.daml.lf.typesig.reader.DamlLfArchiveReader
import com.typesafe.scalalogging.StrictLogging
import scalaz.{-\/, \/-}

object RustCodeGen extends StrictLogging {

  def run(conf: RustCodeGenConf, damlVersion: String): Unit = {
    logger.info(s"Rust codegen running with config: $conf")

    // Load and parse DAR files
    val allPackages = conf.darFiles.flatMap { darPath =>
      logger.info(s"Processing DAR file: $darPath")
      DarParser
        .readArchiveFromFile(darPath.toFile)
        .fold(
          err => {
            throw new RuntimeException(s"Failed to read DAR file $darPath: $err")
          },
          dar => {
            dar.all.map(tryDecodeArchive)
          },
        )
    }.toMap

    logger.info(s"Loaded ${allPackages.size} packages")

    Files.createDirectories(conf.outputDirectory)

    val pkgIdToName = allPackages.map { case (pid, sig) =>
      pid -> sanitizePackageName(sig.metadata.name)
    }

    // Generate Rust code for each package
    allPackages.foreach { case (packageId, packageSig) =>
      generatePackage(conf.outputDirectory, packageId, packageSig, damlVersion, pkgIdToName)
    }

    // Generate a workspace-level lib.rs
    generateLibFile(conf.outputDirectory, allPackages)

    logger.info("Rust code generation completed successfully")
  }

  private def tryDecodeArchive(archive: DamlLf.Archive): (PackageId, Ast.PackageSignature) = {
    DamlLfArchiveReader.readPackage(archive) match {
      case -\/(error) => throw new RuntimeException(error)
      case \/-(result @ (packageId, _)) =>
        logger.trace(s"Daml-LF Archive decoded, packageId '$packageId'")
        result
    }
  }

  private def generatePackage(
      outputDir: Path,
      packageId: PackageId,
      packageSig: Ast.PackageSignature,
      damlVersion: String,
      pkgIdToName: Map[PackageId, String],
  ): Unit = {
    logger.info(
      s"Generating Rust code for package: ${packageSig.metadata.name} (package ID: $packageId)"
    )

    // Sanitize package name (e.g. "my-package" -> "my_package")
    val packageName = sanitizePackageName(packageSig.metadata.name)
    val packageDir = outputDir.resolve(packageName)
    Files.createDirectories(packageDir)

    // Generate individual modules
    packageSig.modules.foreach { case (moduleName, module) =>
      if (!module.isUtilityModule) {
        val moduleFileName = sanitizeModuleName(moduleName.toString()) + ".rs"
        val moduleFile = packageDir.resolve(moduleFileName)
        val moduleContent = generateModule(
          ModuleId(packageId, moduleName),
          packageSig.metadata.name,
          module,
          damlVersion,
          pkgIdToName,
        )
        Files.write(moduleFile, moduleContent.getBytes)
        logger.debug(s"Generated module file: $moduleFile")
      }
    }

    // Generate mod.rs for the package
    val modContent = generatePackageModFile(packageSig)
    val _ = Files.write(packageDir.resolve("mod.rs"), modContent.getBytes)
  }

  private def generateModule(
      moduleId: ModuleId,
      packageName: PackageName,
      module: Ast.ModuleSignature,
      damlVersion: String,
      pkgIdToName: Map[PackageId, String],
  ): String = {
    // 1. Partition Definitions
    val (topLevelDefinitions, nestedDefinitions) =
      module.serializableDefinitions.partition { case (name, _) => name.segments.length == 1 }

    val nestedDefinitionMap = nestedDefinitions.groupBy { case (name, _) => name.segments.head }

    // 2. Generate Templates and Data Definitions
    val templateAndDataDefs: Seq[DefGen] = topLevelDefinitions.toSeq
      .sortBy(_._1)
      .flatMap { case (dottedName, dataDef) =>
        val dataConsName = dottedName.segments.head
        module.templates.get(dottedName) match {
          case Some(template) =>
            genTemplate(moduleId, packageName, dataConsName, template, dataDef, pkgIdToName)
          case None =>
            val nestedDefs =
              nestedDefinitionMap.getOrElse(dataConsName, Map.empty).toSeq.sortBy(_._1)
            genDataDef(moduleId, dataConsName, dataDef, nestedDefs, pkgIdToName)
        }
      }

    // 3. Generate Interfaces
    val interfaces: Seq[DefGen] = module.interfaces.toSeq
      .sortBy(_._1)
      .map { case (name, interface) =>
        genInterface(moduleId, packageName, name, interface, pkgIdToName)
      }

    // 4. Render
    val sb = new CodeBuilder() // Using the CodeBuilder abstraction from previous context
    sb.addLine(s"// Rust bindings for Daml module: ${moduleId.moduleName}")
    sb.addLine(s"// This is a generated file - do not edit manually (DAML version: $damlVersion)")
    sb.addEmptyLine()
    sb.addLine("use daml_types::*;")
    sb.addLine("use serde::{Deserialize, Serialize};")
    sb.addEmptyLine()

    (interfaces ++ templateAndDataDefs).foreach(_.renderRust(sb))

    sb.toString()
  }

  private def genTemplate(
      moduleId: ModuleId,
      packageName: PackageName,
      templateName: Name,
      templateSig: Ast.TemplateSignature,
      dataDef: Ast.DDataType,
      pkgIdToName: Map[PackageId, String],
  ): Seq[DefGen] = {
    // 1. Generate the Struct/Enum for the template payload
    val paramNames = dataDef.params.toSeq.map { case (name, _) => name }
    val typeCon = TypeConGen(moduleId, templateName, paramNames, dataDef.cons, pkgIdToName)

    // 2. Generate Choices
    val choices = templateSig.choices.toSeq
      .sortBy(_._1)
      .map { case (name, choice) =>
        ChoiceGen(name, choice.argBinder._2, choice.returnType, pkgIdToName)
      }

    // 3. Generate the Implementation block
    val template = TemplateGen(
      moduleId,
      packageName,
      templateName,
      templateSig.key.map(_.typ), // Pass the AST Type, not a Decoder
      choices,
      templateSig.implements.values.toSeq.map(_.interfaceId),
      pkgIdToName,
    )

    // 4. Handle Key namespace if necessary (optional in Rust, but good for aliases)
    val namespaceOpt =
      templateSig.key.map(k => TemplateNamespaceGen(moduleId, templateName, k.typ, pkgIdToName))

    Seq(typeCon, template) ++ namespaceOpt
  }

  private def genDataDef(
      moduleId: ModuleId,
      dataConName: Name,
      dataDef: Ast.DDataType,
      nestedDefinitions: Seq[(DottedName, Ast.DDataType)],
      pkgIdToName: Map[PackageId, String],
  ): Seq[DefGen] = {
    val paramNames = dataDef.params.toSeq.map { case (name, _) => name }

    // 1. Main Type Definition
    val mainType = TypeConGen(moduleId, dataConName, paramNames, dataDef.cons, pkgIdToName)

    // 2. Nested Type Definitions
    // In Rust, we can flatten these or put them in a module.
    // Here we generate them as siblings or use NamespaceGen to wrap them in a `mod`.
    val nestedTypes = nestedDefinitions.map { case (dottedName, nestedDef) =>
      val nestedName = dottedName.segments.tail.head
      val nestedParams = nestedDef.params.toSeq.map { case (name, _) => name }
      TypeConGen(moduleId, nestedName, nestedParams, nestedDef.cons, pkgIdToName)
    }

    if (nestedTypes.isEmpty) {
      Seq(mainType)
    } else {
      // If there are nested types, we wrap them in a module to avoid naming collisions
      // or we just emit them. JS Gen puts them in a Namespace.
      // Rust equivalent: pub mod [Name] { ... }
      Seq(mainType, NamespaceGen(dataConName, nestedTypes))
    }
  }

  private def genInterface(
      moduleId: ModuleId,
      packageName: PackageName,
      interfaceName: DottedName,
      interface: Ast.DefInterfaceSignature,
      pkgIdToName: Map[PackageId, String],
  ): DefGen = {
    // Interfaces usually have a View type associated
    val viewId = interface.view match {
      case Ast.TTyCon(tycon) => tycon
      case _ => throw new RuntimeException(s"Invalid view type for $interfaceName")
    }

    val name = interfaceName.segments.toSeq.mkString("_") // Flatten Name

    val choices = interface.choices.toSeq
      .sortBy(_._1)
      .map { case (cName, choice) =>
        ChoiceGen(cName, choice.argBinder._2, choice.returnType, pkgIdToName)
      }

    InterfaceGen(moduleId, packageName, name, choices, viewId, pkgIdToName)
  }

  private def generatePackageModFile(packageSig: Ast.PackageSignature): String = {
    val sb = new StringBuilder
    sb.append(s"// Package: ${packageSig.metadata.name}\n")
    sb.append(s"// Version: ${packageSig.metadata.version}\n\n")

    packageSig.modules.foreach { case (moduleName, module) =>
      if (!module.isUtilityModule) {
        val modName = sanitizeModuleName(moduleName.toString())
        sb.append(s"pub mod $modName;\n")
      }
    }
    sb.toString()
  }

  private def generateLibFile(
      outputDir: Path,
      allPackages: Map[PackageId, Ast.PackageSignature],
  ): Unit = {
    val sb = new StringBuilder
    sb.append("// Daml Rust Bindings\n")
    sb.append("// This is a generated file - do not edit manually\n\n")
    val uniquePackageNames = allPackages.values
      .map(sig => sanitizePackageName(sig.metadata.name))
      .toSeq
      .distinct
      .sorted
    uniquePackageNames.foreach { pkgName =>
      sb.append(s"pub mod $pkgName;\n")
    }

    val _ = Files.write(outputDir.resolve("lib.rs"), sb.toString().getBytes)
  }

  private def sanitizePackageName(name: PackageName): String = {
    val sanitized = name.toString.toLowerCase.replaceAll("[^a-z0-9_]", "_")
    if (sanitized.matches("^[0-9].*")) s"_$sanitized" else sanitized
  }

  private def sanitizeModuleName(name: String): String = {
    val sanitized = name.toLowerCase.replaceAll("[^a-z0-9_]", "_")
    if (sanitized.matches("^[0-9].*")) s"_$sanitized" else sanitized
  }
}
