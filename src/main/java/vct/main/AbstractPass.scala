package vct.main

import vct.col.ast.stmt.decl.ProgramUnit
import vct.logging.{PassAddVisitor, PassReport}

import scala.annotation.varargs

abstract class AbstractPass(val description: String) {
  def apply_pass(reportIn: PassReport, args: Array[String]): PassReport = {
    val arg = reportIn.getOutput
    val reportOut = new PassReport(arg)
    reportOut.add(new PassAddVisitor(reportIn))
    reportOut.setOutput(apply(arg, args))
    reportOut
  }

  protected def apply(report: PassReport, arg: ProgramUnit, args: Array[String]): ProgramUnit =
    apply(arg, args)

  protected def apply(arg: ProgramUnit, args: Array[String]): ProgramUnit =
    ???
}

case class Pass(override val description: String, applyImpl: (ProgramUnit, Array[String]) => ProgramUnit) extends AbstractPass(description) {
  override def apply(arg: ProgramUnit, args: Array[String]): ProgramUnit = applyImpl(arg, args)
}

case class SimplePass(override val description: String, applyImpl: ProgramUnit => ProgramUnit) extends AbstractPass(description) {
  override protected def apply(arg: ProgramUnit, args: Array[String]): ProgramUnit = applyImpl(arg)
}