package vct.main

import vct.col.ast.stmt.decl.ProgramUnit
import vct.col.features.Feature
import vct.logging.{ErrorMapping, PassAddVisitor, PassReport}

import scala.annotation.varargs

abstract class AbstractPass(val key: String, val description: String) {
  def removes: Set[Feature]
  def introduces: Set[Feature]
  def permits: Set[Feature]

  def tup: (String, AbstractPass) = (key, this)

  def apply_pass(reportIn: PassReport, args: Array[String]): PassReport = {
    val arg = reportIn.getOutput
    val reportOut = new PassReport(arg)
    reportOut.add(new PassAddVisitor(reportIn))
    reportOut.setOutput(apply(reportOut, arg, args))
    reportOut
  }

  protected def apply(report: PassReport, arg: ProgramUnit, args: Array[String]): ProgramUnit =
    apply(arg, args)

  protected def apply(arg: ProgramUnit, args: Array[String]): ProgramUnit =
    ???
}

case class Pass(override val key: String,
                override val description: String,
                applyImpl: (ProgramUnit, Array[String]) => ProgramUnit,
                removes: Set[Feature] = Set(),
                introduces: Set[Feature] = Feature.DEFAULT_INTRODUCE,
                permits: Set[Feature] = Feature.DEFAULT_PERMIT)
  extends AbstractPass(key, description) {
  override def apply(arg: ProgramUnit, args: Array[String]): ProgramUnit = applyImpl(arg, args)
}

case class SimplePass(override val key: String,
                      override val description: String,
                      applyImpl: ProgramUnit => ProgramUnit,
                      removes: Set[Feature] = Set(),
                      introduces: Set[Feature] = Feature.DEFAULT_INTRODUCE,
                      permits: Set[Feature] = Feature.DEFAULT_PERMIT)
  extends AbstractPass(key, description) {
  override protected def apply(arg: ProgramUnit, args: Array[String]): ProgramUnit = applyImpl(arg)
}

case class ErrorMapPass(override val key: String,
                        override val description: String,
                        applyImpl: (ProgramUnit, ErrorMapping) => ProgramUnit,
                        removes: Set[Feature] = Set(),
                        introduces: Set[Feature] = Feature.DEFAULT_INTRODUCE,
                        permits: Set[Feature] = Feature.DEFAULT_PERMIT)
  extends AbstractPass(key, description) {
  override def apply_pass(reportIn: PassReport, args: Array[String]): PassReport = {
    val arg = reportIn.getOutput
    val reportOut = new PassReport(arg)
    val map = new ErrorMapping(reportIn)
    reportOut.add(map)
    reportOut.setOutput(applyImpl(arg, map))
    reportOut
  }
}