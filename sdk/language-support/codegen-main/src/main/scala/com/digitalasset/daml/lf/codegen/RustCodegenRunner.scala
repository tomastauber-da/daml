// Copyright (c) 2025 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.daml.lf.codegen

import ch.qos.logback.classic.Level
import com.daml.assistant.config.{ConfigLoadingError, PackageConfig}
import scopt.OptionParser

object RustCodegenRunner extends CodegenRunner {
  type Config = RustCodeGenConf

  override def configParser(isDpm: Boolean): OptionParser[RustCodeGenConf] =
    RustCodeGenConf.parser(parserName(isDpm))

  override def configureFromArgs(args: Array[String], isDpm: Boolean): Option[RustCodeGenConf] =
    RustCodeGenConf.parse(args, parserName(isDpm))

  override def configureFromPackageConfig(
      sdkConf: PackageConfig
  ): Either[ConfigLoadingError, RustCodeGenConf] =
    for {
      dar <- ConfigReader.darPath(sdkConf)
      codegenCursor = ConfigReader.codegen(sdkConf, target = "rust")
      outputDirectory <- ConfigReader.outputDirectory(codegenCursor)
      verbosity <- ConfigReader.verbosity(codegenCursor, Level.ERROR)
    } yield RustCodeGenConf(
      darFiles = Seq(dar),
      outputDirectory = outputDirectory,
      verbosity = verbosity,
    )

  override def generateCode(config: RustCodeGenConf, damlVersion: String): Unit =
    RustCodeGen.run(config, damlVersion)

  private def parserName(isDpm: Boolean): String = if (isDpm) "codegen-rust" else "codegen"
}
