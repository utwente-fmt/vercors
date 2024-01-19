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

trait Stages[-Input, +Output] {
  def run(in: Input): Either[VerificationError, Output] =
    try {
      val stages = collect.asInstanceOf[Seq[Stage[Any, Nothing]]]
      Progress.stages(flatNames) { progressNext =>
        var cur: Any = in

        for((stage, idx) <- stages.zipWithIndex) {
          if(idx > 0) progressNext()
          cur = stage.run(cur)
        }

        Right(cur.asInstanceOf[Output])
      }
    } catch {
      case err: VerificationError => Left(err)
    }

  def collect: Seq[Stage[Nothing, Any]]

  def flatNames: Seq[(String, Int)] = collect.map(s => s.friendlyName -> s.progressWeight)

  def thenRun[NewOutput](stage: Stage[Output, NewOutput]): Stages[Input, NewOutput] =
    thenRun(UnitStages(stage))

  def thenRun[NewOutput](stages: Stages[Output, NewOutput]): Stages[Input, NewOutput] =
    StagesPair(this, stages)
}

case class UnitStages[-Input, +Output](stage: Stage[Input, Output]) extends Stages[Input, Output] {
  override def collect: Seq[Stage[Nothing, Any]] = Seq(stage)
}

case class StagesPair[-Input, Mid, +Output](left: Stages[Input, Mid], right: Stages[Mid, Output]) extends Stages[Input, Output] {
  override def collect: Seq[Stage[Nothing, Any]] = left.collect ++ right.collect
}