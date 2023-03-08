package vct.main.stages

import hre.io.Writeable
import hre.stages.Stage
import vct.col.ast.Verification
import vct.col.print.{PVL, Printer}
import vct.col.rewrite.Generation
import vct.options.Options

case object SaveStage {
  def ofOptions(options: Options): Stage[Verification[_ <: Generation], Unit] = {
    SaveStage(options.vesuvOutput)
  }
}

case class SaveStage(writeable : Writeable) extends Stage[Verification[_<: Generation], Unit] {
  override def friendlyName: String = "Saving File..."

  override def progressWeight: Int = 1

  override def run(in: Verification[_ <: Generation]): Unit = {
    writeable.write(w => {
      val printer = Printer(w, syntax = PVL)
      printer.print(in.tasks.head.program)
    })
  }
}