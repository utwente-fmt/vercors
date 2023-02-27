package hre.stages

import hre.progress.Progress
import vct.result.VerificationError

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
    Progress.next()
    right.runUnsafely(mid)
  }

  override def flatNames: Seq[(String, Int)] = left.flatNames ++ right.flatNames
}