package vct.main.stages

import vct.col.util.ExpectedError
import vct.options.Options

case object ExpectedErrors {
  // Just to be uniform with the other stages
  def ofOptions(options: Options): ExpectedErrors =
    ExpectedErrors()
}

case class ExpectedErrors() extends Stage[(Unit, Seq[ExpectedError]), Unit] {
  override def friendlyName: String = "Check inverted errors"
  override def progressWeight: Int = 1

  override def run(in: (Unit, Seq[ExpectedError])): Unit =
    in._2.foreach(_.signalDone())
}
