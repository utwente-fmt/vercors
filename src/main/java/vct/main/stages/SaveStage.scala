package vct.main.stages

import hre.io.Writeable
import hre.stages.Stage
import vct.col.ast.Verification
import vct.col.print.Printer
import vct.col.rewrite.Generation
import vct.options.Options

case class SaveStage(writeable : Writeable) extends Stage[Verification[_<: Generation], Unit] {
  override def friendlyName: String = "Saving File..."

  override def progressWeight: Int = 1

  override def run(in: Verification[_ <: Generation]): Unit = {
    writeable.write(w => {
      val printer = Printer(w)
      printer.print(in.tasks.head.program)
    })
  }
}

object SaveStage {
  def ofOptions(options: Options): Stage[Verification[_ <: Generation], Unit] = {
    SaveStage(options.veymontOutput)
  }
}
