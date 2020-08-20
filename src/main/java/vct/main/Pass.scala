package vct.main

import vct.col.ast.stmt.decl.ProgramUnit
import vct.logging.{PassAddVisitor, PassReport}

import scala.annotation.varargs

abstract class Pass(val description: String) {
  def getDescripion = description

  @varargs def apply_pass(reportIn: PassReport, args: String*): PassReport = {
    val arg = reportIn.getOutput
    val reportOut = new PassReport(arg)
    reportOut.add(new PassAddVisitor(reportIn))
    reportOut.setOutput(apply(arg, args:_*))
    reportOut
  }

  @varargs protected def apply(report: PassReport, arg: ProgramUnit, args: String*): ProgramUnit =
    apply(arg, args:_*)

  @varargs protected def apply(arg: ProgramUnit, args: String*): ProgramUnit =
    ???
}