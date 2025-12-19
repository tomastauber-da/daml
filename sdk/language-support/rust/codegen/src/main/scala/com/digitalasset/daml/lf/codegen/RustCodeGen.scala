// Copyright (c) 2025 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.daml.lf.codegen

import java.nio.file.{Files, Path}
//import scala.collection.immutable.Map
import com.digitalasset.daml.lf.archive.{DamlLf, DarParser}
import com.digitalasset.daml.lf.data.Ref._
import com.digitalasset.daml.lf.language.Ast
import com.digitalasset.daml.lf.typesig.reader.DamlLfArchiveReader
import com.typesafe.scalalogging.StrictLogging
import scalaz.{-\/, \/-}

object RustCodeGen extends StrictLogging {

  def run(conf: RustCodeGenConf, damlVersion: String): Unit = {
    logger.info(s"Rust codegen running with config: $conf")
    logger.info(s"Daml version: $damlVersion")

    // Load and parse DAR files
    val allPackages = conf.darFiles.flatMap { darPath =>
      logger.info(s"Processing DAR file: $darPath")
      DarParser
        .readArchiveFromFile(darPath.toFile)
        .fold(
          err => {
            logger.error(s"Failed to read DAR file $darPath: $err")
            throw new RuntimeException(s"Failed to read DAR file $darPath: $err")
          },
          dar => {
            val packages = dar.all.map(tryDecodeArchive)
            packages
          },
        )
    }.toMap

    logger.info(s"Loaded ${allPackages.size} packages")

    // Create output directory
    Files.createDirectories(conf.outputDirectory)

    // Generate Rust code for each package
    allPackages.foreach { case (packageId, packageSig) =>
      generatePackage(conf.outputDirectory, packageId, packageSig /*, allPackages*/ )
    }

    // Generate a lib.rs file that includes all modules
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
      // allPackages: Map[PackageId, Ast.PackageSignature],
  ): Unit = {
    logger.info(
      s"Generating Rust code for package: ${packageSig.metadata.name} (package ID: $packageId)"
    )

    // Create a directory for this package
    val packageDir = outputDir.resolve(sanitizePackageName(packageSig.metadata.name))
    Files.createDirectories(packageDir)

    // Generate a stub module file for each Daml module
    packageSig.modules.foreach { case (moduleName, module) =>
      if (!module.isUtilityModule) {
        val moduleFileName = sanitizeModuleName(moduleName.toString()) + ".rs"
        val moduleFile = packageDir.resolve(moduleFileName)
        val moduleContent = generateModuleStub(moduleName, module)
        Files.write(moduleFile, moduleContent.getBytes)
        logger.debug(s"Generated module file: $moduleFile")
      }
    }

    // Generate a mod.rs file for the package
    val modContent = generatePackageModFile(packageSig)
    val _ = Files.write(packageDir.resolve("mod.rs"), modContent.getBytes)
  }

  private def generateModuleStub(moduleName: ModuleName, module: Ast.ModuleSignature): String = {
    val sb = new StringBuilder
    sb.append(s"// Rust bindings for Daml module: $moduleName\n")
    sb.append("// This is a generated file - do not edit manually\n\n")
    sb.append("use serde::{Deserialize, Serialize};\n\n")

    // Add a placeholder comment about what would be generated
    sb.append("// TODO: Generate Rust types for:\n")
    sb.append(s"// - ${module.templates.size} template(s)\n")
    sb.append(s"// - ${module.interfaces.size} interface(s)\n")
    sb.append(s"// - ${module.definitions.size} definition(s)\n")
    sb.append("\n")

    sb.toString()
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

    allPackages.values.foreach { packageSig =>
      val pkgName = sanitizePackageName(packageSig.metadata.name)
      sb.append(s"pub mod $pkgName;\n")
    }

    val _ = Files.write(outputDir.resolve("lib.rs"), sb.toString().getBytes)
  }

  private def sanitizePackageName(name: PackageName): String = {
    val sanitized = name
      .toLowerCase()
      .replaceAll("[^a-z0-9_]", "_")
    // Prepend underscore if starts with digit
    if (sanitized.matches("^[0-9].*")) s"_$sanitized" else sanitized
  }

  private def sanitizeModuleName(name: String): String = {
    val sanitized = name
      .toLowerCase()
      .replaceAll("[^a-z0-9_]", "_")
    // Prepend underscore if starts with digit
    if (sanitized.matches("^[0-9].*")) s"_$sanitized" else sanitized
  }
}
