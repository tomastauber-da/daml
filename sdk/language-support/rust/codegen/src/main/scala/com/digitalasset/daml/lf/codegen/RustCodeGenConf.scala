// Copyright (c) 2025 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.daml.lf.codegen

import java.nio.file.{Path, Paths}

import ch.qos.logback.classic.Level
import com.daml.buildinfo.BuildInfo
import scopt.OptionParser

/** @param darFiles The [[Set]] of Daml-LF [[Path]]s to convert into code. It MUST contain
  *                 all the Daml-LF packages dependencies.
  * @param outputDirectory The directory where the code will be generated
  */
final case class RustCodeGenConf(
    darFiles: Seq[Path],
    outputDirectory: Path,
    verbosity: Level = Level.ERROR,
)

object RustCodeGenConf {
  def parse(args: Array[String], parserName: String): Option[RustCodeGenConf] =
    parser(parserName).parse(
      args,
      RustCodeGenConf(darFiles = Seq.empty, outputDirectory = Paths.get(".")),
    )

  def parser(parserName: String): OptionParser[RustCodeGenConf] =
    new scopt.OptionParser[RustCodeGenConf](parserName) {
      head(parserName, BuildInfo.Version)
      note("Code generator for the Daml ledger Rust bindings.\n")

      arg[Path]("<DAR-file>...")(readPath)
        .unbounded()
        .action((p, c) => c.copy(darFiles = c.darFiles :+ p))
        .required()
        .text("DAR files to generate Rust bindings for")

      opt[Path]('o', "output-directory")(readPath)
        .action((p, c) => c.copy(outputDirectory = p))
        .required()
        .text("Output directory for the generated sources")

      opt[Level]('V', "verbosity")(readVerbosity)
        .action((l, c) => c.copy(verbosity = l))
        .text("Verbosity between 0 (only show errors) and 4 (show all messages) -- defaults to 0")

      help("help").text("This help text")
    }

  private val readPath: scopt.Read[Path] = scopt.Read.stringRead.map(s => Paths.get(s))

  private val readVerbosity: scopt.Read[Level] = scopt.Read.stringRead.map {
    case "0" => Level.ERROR
    case "1" => Level.WARN
    case "2" => Level.INFO
    case "3" => Level.DEBUG
    case "4" => Level.TRACE
    case _ =>
      throw new IllegalArgumentException(
        "Expected a verbosity value between 0 (least verbose) and 4 (most verbose)"
      )
  }
}
