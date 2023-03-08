package vct.main.stages

import hre.stages.Stage
import vct.col.ast.{Program, Verification, VerificationContext}
import vct.col.origin.{FileSpanningOrigin, Origin}
import vct.parsers.ParseResult
import vct.parsers.transform.BlameProvider

case object ToVerification {
  def ofOptions[G](blameProvider: BlameProvider): ToVerification[G] = {
    ToVerification(blameProvider)
  }
}

case class ToVerification[G]
(
  blameProvider: BlameProvider
) extends Stage[ParseResult[G], Verification[G]]{

  override def friendlyName: String = "ToVerification"
  override def progressWeight: Int = 1
  override def run(in: ParseResult[G]): Verification[G] = {
    implicit val o: Origin = FileSpanningOrigin
    Verification(List(VerificationContext(Program(in.decls)(blameProvider()))), in.expectedErrors)
  }
}
