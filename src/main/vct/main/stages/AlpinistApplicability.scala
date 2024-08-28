package vct.main.stages

import hre.stages.Stage
import vct.col.ast.Verification
import vct.col.rewrite.Generation
import vct.options.Options

object AlpinistApplicability {
  def ofOptions(
      options: Options
  ): Stage[Verification[_ <: Generation], Verification[_ <: Generation]] =
    AlpinistApplicability()
}

case class AlpinistApplicability()
    extends Stage[
      Verification[_ <: Generation],
      Verification[_ <: Generation],
    ] {
  override def friendlyName: String = "Applicability"
  override def progressWeight: Int = 1

  override def run(
      in: Verification[_ <: Generation]
  ): Verification[_ <: Generation] = {
    /* Put any applicability checks here, throw a UserError if optimizations are not applicable. */
    in
  }
}
