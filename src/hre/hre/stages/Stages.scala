package hre.stages

import hre.progress.Progress
import vct.result.VerificationError

trait Stage[-Input, +Output] {
  def friendlyName: String
  def progressWeight: Int

  def run(in: Input): Output

  def thenRun[NewOutput](stage: Stage[Output, NewOutput]): Stages[Input, NewOutput] =
    UnitStages(this).thenRun(stage)

  def thenRun[NewOutput](stages: Stages[Output, NewOutput]): Stages[Input, NewOutput] =
    UnitStages(this).thenRun(stages)
}

trait ContextStage[-Input, Ctx, +Output] extends Stage[(Input, Ctx), (Output, Ctx)] {
  def run(in: (Input, Ctx)): (Output, Ctx) =
    (runWithoutContext(in._1), in._2)

  def runWithoutContext(input: Input): Output
}

trait Stages[-Input, +Output] {
  def run(in: Input): Either[VerificationError, Output] =
    try {
      Progress.stages(flatNames) { progressNext =>
        Right(runUnsafely(in, progressNext))
      }
    } catch {
      case err: VerificationError => Left(err)
    }

  def runUnsafely(in: Input, progressNext: () => Unit): Output

  def flatNames: Seq[(String, Int)]

  def thenRun[NewOutput](stage: Stage[Output, NewOutput]): Stages[Input, NewOutput] =
    thenRun(UnitStages(stage))

  def thenRun[NewOutput](stages: Stages[Output, NewOutput]): Stages[Input, NewOutput] =
    StagesPair(this, stages)
}

case class UnitStages[-Input, +Output](stage: Stage[Input, Output]) extends Stages[Input, Output] {
  override def runUnsafely(in: Input, progressNext: () => Unit): Output = stage.run(in)
  override def flatNames: Seq[(String, Int)] = Seq((stage.friendlyName, stage.progressWeight))
}

case class StagesPair[-Input, Mid, +Output](left: Stages[Input, Mid], right: Stages[Mid, Output]) extends Stages[Input, Output] {
  override def runUnsafely(in: Input, progressNext: () => Unit): Output = {
    val mid = left.runUnsafely(in, progressNext)
    progressNext()
    right.runUnsafely(mid, progressNext)
  }
  override def flatNames: Seq[(String, Int)] = left.flatNames ++ right.flatNames
}