// Copyright (c) 2025 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.daml.lf.codegen

import java.nio.file.{Files, Path}

import scala.collection.immutable.Map

import com.digitalasset.daml.lf.archive.DarParser
import com.digitalasset.daml.lf.data.Ref._
import com.digitalasset.daml.lf.language.Ast
import com.digitalasset.daml.lf.typesig.reader.DamlLfArchiveReader
import com.typesafe.scalalogging.StrictLogging
import org.slf4j.LoggerFactory
import scalaz.{-\/, \/-}

object RustCodeGen extends StrictLogging {

  private val log = LoggerFactory.getLogger(getClass)

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
            val packages = dar.all.map { case (hash, archive) =>
              val packageId = PackageId.assertFromString(hash)
              DamlLfArchiveReader(packageId, archive) match {
                case -\/(err) =>
                  logger.error(s"Failed to read package $packageId: $err")
                  throw new RuntimeException(s"Failed to read package $packageId: $err")
                case \/-(pkg) =>
                  logger.info(s"Successfully loaded package: ${pkg.metadata.name}-${pkg.metadata.version}")
                  (packageId, pkg)
              }
            }
            packages
          },
        )
    }.toMap

    logger.info(s"Loaded ${allPackages.size} packages")

    // Create output directory
    Files.createDirectories(conf.outputDirectory)

    // Generate Rust code for each package
    allPackages.foreach { case (packageId, packageSig) =>
      generatePackage(conf.outputDirectory, packageId, packageSig, allPackages)
    }

    // Generate a lib.rs file that includes all modules
    generateLibFile(conf.outputDirectory, allPackages)

    logger.info("Rust code generation completed successfully")
  }

  private def generatePackage(
      outputDir: Path,
      packageId: PackageId,
      packageSig: Ast.PackageSignature,
      allPackages: Map[PackageId, Ast.PackageSignature],
  ): Unit = {
    logger.info(s"Generating Rust code for package: ${packageSig.metadata.name}")

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
    Files.write(packageDir.resolve("mod.rs"), modContent.getBytes)
  }

  private def generateModuleStub(moduleName: ModuleName, module: Ast.Module): String = {
    val sb = new StringBuilder
    sb.append(s"// Rust bindings for Daml module: $moduleName\n")
    sb.append("// This is a generated file - do not edit manually\n\n")
    sb.append("use serde::{Deserialize, Serialize};\n\n")
    
    // Add a placeholder comment about what would be generated
    sb.append("// TODO: Generate Rust types for:\n")
    sb.append(s"// - ${module.templates.size} template(s)\n")
    sb.append(s"// - ${module.dataTypes.size} data type(s)\n")
    sb.append(s"// - ${module.values.size} value(s)\n")
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

  private def generateLibFile(outputDir: Path, allPackages: Map[PackageId, Ast.PackageSignature]): Unit = {
    val sb = new StringBuilder
    sb.append("// Daml Rust Bindings\n")
    sb.append("// This is a generated file - do not edit manually\n\n")
    
    allPackages.values.foreach { packageSig =>
      val pkgName = sanitizePackageName(packageSig.metadata.name)
      sb.append(s"pub mod $pkgName;\n")
    }
    
    Files.write(outputDir.resolve("lib.rs"), sb.toString().getBytes)
  }

  private def sanitizePackageName(name: PackageName): String = {
    name.toString()
      .toLowerCase()
      .replaceAll("[^a-z0-9_]", "_")
      .replaceAll("^[0-9]", "_")
  }

  private def sanitizeModuleName(name: String): String = {
    name
      .toLowerCase()
      .replaceAll("[^a-z0-9_]", "_")
      .replaceAll("^[0-9]", "_")
  }
}
