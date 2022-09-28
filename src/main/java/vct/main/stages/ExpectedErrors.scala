package vct.main.stages

import vct.col.origin.ExpectedError
import vct.options.Options

case object ExpectedErrors {
  // Just to be uniform with the other stages
  def ofOptions(options: Options): ExpectedErrors =
    ExpectedErrors()
}

case class ExpectedErrors() extends Stage[Seq[ExpectedError], Unit] {
  override def friendlyName: String = "Check inverted errors"
  override def progressWeight: Int = 1

  override def run(in: Seq[ExpectedError]): Unit =
    in.foreach(_.signalDone())
}
