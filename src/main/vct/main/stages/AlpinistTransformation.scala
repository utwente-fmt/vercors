package vct.main.stages

import hre.stages.Stage
import vct.col.ast.Verification
import vct.col.rewrite.{Generation, RewriterBuilder}
import vct.main.stages.Transformation.{
  PassEventHandler,
  reportIntermediateProgram,
  writeOutFunctions,
}
import vct.options.Options

object AlpinistTransformation {
  def ofOptions(
      options: Options
  ): Stage[Verification[_ <: Generation], Verification[_ <: Generation]] =
    AlpinistTransformation(
      onPassEvent =
        options.outputIntermediatePrograms
          .map(p => Transformation.reportIntermediateProgram(p, "verify"))
          .toSeq ++ Transformation.writeOutFunctions(
          Transformation.before,
          options.outputBeforePass,
        ) ++
          Transformation
            .writeOutFunctions(Transformation.after, options.outputAfterPass),
      optimizeUnsafe = options.devUnsafeOptimization,
    )
}

case class AlpinistTransformation(
    override val onPassEvent: Seq[PassEventHandler],
    override val optimizeUnsafe: Boolean = false,
) extends Transformation(
      onPassEvent,
      Seq( /* Put rewriters here that enact alpinist transformations */ ),
      optimizeUnsafe,
    )
