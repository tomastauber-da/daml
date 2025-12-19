// Copyright (c) 2025 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.daml.lf.codegen

import ch.qos.logback.classic.{Level, Logger}
import org.slf4j.LoggerFactory

object RustCodeGenMain {

  private val log = LoggerFactory.getLogger(getClass)

  def main(args: Array[String]): Unit = {
    RustCodeGenConf.parse(args, "codegen-rust") match {
      case Some(conf) =>
        setupLogger(conf.verbosity)
        log.info("Starting Rust code generation")
        try {
          val damlVersion = sys.env
            .get("DPM_SDK_VERSION")
            .orElse(sys.env.get("DAML_SDK_VERSION"))
            .getOrElse("0.0.0")
          RustCodeGen.run(conf, damlVersion)
          System.exit(0)
        } catch {
          case e: Exception =>
            log.error("Code generation failed", e)
            System.exit(1)
        }
      case None =>
        System.exit(1)
    }
  }

  private def setupLogger(level: Level): Unit = {
    LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME) match {
      case l: Logger => l.setLevel(level)
      case _ =>
    }
  }
}
