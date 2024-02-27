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

  def transform[Output2](f: Output => Output2): Stages[Input, Output2] = {
    UnitStages(this).thenRun(FunctionStage(f))
  }

  def preprocess[Input2](f: Input2 => Input): Stages[Input2, Output] =
    UnitStages(FunctionStage(f)).thenRun(UnitStages(this))
}

object Stages {
  def saveInput[Input, Output](stages: Stages[Input, Output]): Stages[Input, (Input, Output)] = SaveInputStage(stages)

  def preprocess[Input2, Input, Output](f: Input2 => Input, stages: Stages[Input, Output]): Stages[Input2, Output] =
    UnitStages(FunctionStage(f)).thenRun(stages)
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

  def transform[Output2](f: Output => Output2): Stages[Input, Output2] =
    thenRun(FunctionStage(f))
}

case class FunctionStage[T, S](f: T => S) extends Stage[T, S] {
  override def friendlyName: String = "..."
  override def progressWeight: Int = 1
  override def run(in: T): S = f(in)
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
