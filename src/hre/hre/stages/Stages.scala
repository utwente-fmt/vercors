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

  def saveInput: Stages[Input, (_ >: Input, Output)] = SaveInputStage(UnitStages(this))
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

  def saveInput: Stages[Input, (_ >: Input, Output)] =
    SaveInputStage(this)
}

object StagesHelpers {
  implicit class StagesDropSaved[Input, Output](stages: Stages[Input, (Input, Output)]) {
    def dropOutput: Stages[Input, Input] = DropOutputStage(stages)
    def dropInput: Stages[Input, Output] = DropInputStage(stages)
  }
}

case class UnitStages[-Input, +Output](stage: Stage[Input, Output]) extends Stages[Input, Output] {
  override def collect: Seq[Stage[Nothing, Any]] = Seq(stage)
}

case class StagesPair[-Input, Mid, +Output](left: Stages[Input, Mid], right: Stages[Mid, Output]) extends Stages[Input, Output] {
  override def collect: Seq[Stage[Nothing, Any]] = left.collect ++ right.collect
}

case class SaveInputStage[Input, Output](stage: Stages[Input, Output]) extends Stages[Input, (Input, Output)] {
  override def run(in: Input): Either[VerificationError, (Input, Output)] = {
    stage.run(in).map { out => (in, out) }
  }

  override def collect: Seq[Stage[Nothing, Any]] = stage.collect
}

case class DropOutputStage[Input, Output](stage: Stages[Input, (Input, Output)]) extends Stages[Input, Input] {
  override def collect: Seq[Stage[Nothing, Any]] = stage.collect
  override def run(in: Input): Either[VerificationError, Input] = stage.run(in).map(_._1)
}

case class DropInputStage[Input, Output](stage: Stages[Input, (Input, Output)]) extends Stages[Input, Output] {
  override def collect: Seq[Stage[Nothing, Any]] = stage.collect
  override def run(in: Input): Either[VerificationError, Output] = stage.run(in).map(_._2)
}
