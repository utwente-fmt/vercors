package vct.main.stages

import hre.progress.Progress
import vct.col.ast.Program
import vct.col.rewrite.Generation
import vct.options.Options
import vct.parsers.transform.BlameProvider
import vct.result.VerificationError
import hre.io.Readable
import viper.api.Silicon

trait Stage[-Input, +Output] {
  def friendlyName: String
  def progressWeight: Int

  def run(in: Input): Output

  def thenRun[NewOutput](stage: Stage[Output, NewOutput]): Stages[Input, NewOutput] =
    UnitStages(this).thenRun(stage)
}

trait ContextStage[-Input, Ctx, +Output] extends Stage[(Input, Ctx), (Output, Ctx)] {
  def run(in: (Input, Ctx)): (Output, Ctx) =
    (runWithoutContext(in._1), in._2)

  def runWithoutContext(input: Input): Output
}

case object Stages {
  def default(blameProvider: BlameProvider): Stages[Seq[Readable], Unit] = {
    Parsing(blameProvider)
      .thenRun(Resolution(blameProvider))
      .thenRun(SilverTransformation())
      .thenRun(SilverBackend(Silicon()))
      .thenRun(ExpectedErrors())
  }

  def ofOptions(options: Options, blameProvider: BlameProvider): Stages[Seq[Readable], Unit] = {
    Parsing.ofOptions(options, blameProvider)
      .thenRun(Resolution.ofOptions(options, blameProvider))
      .thenRun(Transformation.ofOptions(options))
      .thenRun(Backend.ofOptions(options))
      .thenRun(ExpectedErrors.ofOptions(options))
  }
}

trait Stages[-Input, +Output] {
  def run(in: Input): Either[VerificationError, Output] =
    try {
      Progress.stages(flatNames) {
        Right(runUnsafely(in))
      }
    } catch {
      case err: VerificationError => Left(err)
    }

  def runUnsafely(in: Input): Output

  def flatNames: Seq[(String, Int)]

  def thenRun[NewOutput](stage: Stage[Output, NewOutput]): Stages[Input, NewOutput] =
    StagesPair(this, UnitStages(stage))
}

case class UnitStages[-Input, +Output](stage: Stage[Input, Output]) extends Stages[Input, Output] {
  override def runUnsafely(in: Input): Output = stage.run(in)
  override def flatNames: Seq[(String, Int)] = Seq((stage.friendlyName, stage.progressWeight))
}

case class StagesPair[-Input, Mid, +Output](left: Stages[Input, Mid], right: Stages[Mid, Output]) extends Stages[Input, Output] {
  override def runUnsafely(in: Input): Output = {
    val mid = left.runUnsafely(in)
    Progress.nextPhase()
    right.runUnsafely(mid)
  }

  override def flatNames: Seq[(String, Int)] = left.flatNames ++ right.flatNames
}