package com.digitalasset.daml.lf.codegen

private[codegen] sealed trait DefGen {
  def renderRsSource(b: CodeBuilder): Unit
}