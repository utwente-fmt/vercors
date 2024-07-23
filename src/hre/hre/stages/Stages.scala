package hre.stages

import com.typesafe.scalalogging.LazyLogging
import hre.progress.Progress
import hre.util.Time
import vct.result.VerificationError

trait Stage[-Input, +Output] {
  def friendlyName: String
  def progressWeight: Int

  def run(in: Input): Output

  def thenRun[NewOutput](
      stage: Stage[Output, NewOutput]
  ): Stages[Input, NewOutput] = UnitStages(this).thenRun(stage)

  def thenRun[NewOutput](
      stages: Stages[Output, NewOutput]
  ): Stages[Input, NewOutput] = UnitStages(this).thenRun(stages)

  def saveInput: Stages[Input, (_ >: Input, Output)] =
    SaveInputStage(UnitStages(this))

  def transform[Output2](f: Output => Output2): Stages[Input, Output2] = {
    UnitStages(this).thenRun(FunctionStage(f))
  }

  def also(f: => Unit): Stages[Input, Output] = {
    UnitStages(this).thenRun(FunctionStage[Output, Output](x => { f; x }))
  }

  def drop(): Stages[Input, Unit] = {
    UnitStages(this).thenRun(FunctionStage[Output, Unit](_ => {}))
  }

  def preprocess[Input2](f: Input2 => Input): Stages[Input2, Output] =
    UnitStages(FunctionStage(f)).thenRun(UnitStages(this))
}

case class IdentityStage[T]() extends Stage[T, T] {
  def run(in: T): T = in

  override def friendlyName: String = "identity stage"

  override def progressWeight: Int = 0
}

abstract class WrapStage[InnerInput, InnerOutput, OuterInput, OuterOutput](
    stage: Stage[InnerInput, InnerOutput]
) extends Stage[OuterInput, OuterOutput] {
  override def friendlyName: String = stage.friendlyName

  override def progressWeight: Int = stage.progressWeight

  override def run(in: OuterInput): OuterOutput = zoomOut(stage.run(zoomIn(in)))

  def zoomIn(in: OuterInput): InnerInput
  def zoomOut(out: InnerOutput): OuterOutput
}

object Stages {
  def saveInput[Input, Output](
      stages: Stages[Input, Output]
  ): Stages[Input, (Input, Output)] = SaveInputStage(stages)

  def skipIf[Input](
      condition: Boolean,
      stages: Stages[Input, Unit],
  ): Stages[Input, Unit] = SkipIf(condition, stages)

  def branch[Input, Output](
      condition: Boolean,
      tt: Stages[Input, Output],
      ff: Stages[Input, Output],
  ): Branch[Input, Output] = Branch[Input, Output](condition, tt, ff)

  def preprocess[Input2, Input, Output](
      f: Input2 => Input,
      stages: Stages[Input, Output],
  ): Stages[Input2, Output] = UnitStages(FunctionStage(f)).thenRun(stages)

  def timed[Input, Output](
      name: String,
      stages: Stages[Input, Output],
  ): TimedStages[Input, Output] = TimedStages(name, stages)
}

trait Stages[-Input, +Output] {
  def run(in: Input): Either[VerificationError, Output] =
    try {
      val stages = collect.asInstanceOf[Seq[Stage[Any, Nothing]]]
      Progress.stages(flatNames) { progressNext =>
        var cur: Any = in

        for ((stage, idx) <- stages.zipWithIndex) {
          if (idx > 0)
            progressNext()
          cur = stage.run(cur)
        }

        Right(cur.asInstanceOf[Output])
      }
    } catch { case err: VerificationError => Left(err) }

  def collect: Seq[Stage[Nothing, Any]]

  def flatNames: Seq[(String, Int)] =
    collect.map(s => s.friendlyName -> s.progressWeight)

  def thenRun[NewOutput](
      stage: Stage[Output, NewOutput]
  ): Stages[Input, NewOutput] = thenRun(UnitStages(stage))

  def thenRun[NewOutput](
      stages: Stages[Output, NewOutput]
  ): Stages[Input, NewOutput] = StagesPair(this, stages)

  def transform[Output2](f: Output => Output2): Stages[Input, Output2] =
    thenRun(FunctionStage(f))

  def also(f: => Unit): Stages[Input, Output] =
    thenRun(FunctionStage[Output, Output](x => { val y = f; x }))

  def drop(): Stages[Input, Unit] =
    thenRun(FunctionStage[Output, Unit](_ => {}))
}

case class FunctionStage[T, S](f: T => S) extends Stage[T, S] {
  override def friendlyName: String = "..."
  override def progressWeight: Int = 1
  override def run(in: T): S = f(in)
}

case class UnitStages[-Input, +Output](stage: Stage[Input, Output])
    extends Stages[Input, Output] {
  override def collect: Seq[Stage[Nothing, Any]] = Seq(stage)
}

case class StagesPair[-Input, Mid, +Output](
    left: Stages[Input, Mid],
    right: Stages[Mid, Output],
) extends Stages[Input, Output] {
  override def collect: Seq[Stage[Nothing, Any]] = left.collect ++ right.collect
}

case class SaveInputStage[Input, Output](stages: Stages[Input, Output])
    extends Stages[Input, (Input, Output)] {
  case class RetainInput[Input, Output](stage: Stage[Input, Output])
      extends WrapStage[Input, Output, Input, (Input, Output)](stage) {
    var in: Option[Input] = None
    override def zoomIn(in: Input): Input = {
      this.in = Some(in)
      in
    }
    override def zoomOut(out: Output): (Input, Output) = (in.get, out)
  }

  case class Map_2[Left, Input, Output](stage: Stage[Input, Output])
      extends WrapStage[Input, Output, (Left, Input), (Left, Output)](stage) {
    var left: Option[Left] = None
    override def zoomIn(in: (Left, Input)): Input = {
      left = Some(in._1)
      in._2
    }
    override def zoomOut(out: Output): (Left, Output) = (left.get, out)
  }

  override def collect: Seq[Stage[Nothing, Any]] =
    stages.collect match {
      case Seq() => Seq()
      case Seq(stage) =>
        Seq(RetainInput[Nothing, Any](stage).asInstanceOf[Stage[Nothing, Any]])
      case stage +: stages =>
        RetainInput[Nothing, Any](stage).asInstanceOf[Stage[Nothing, Any]] +:
          stages.map(Map_2[Nothing, Nothing, Any])
    }
}

case class SkipIf[Input](condition: Boolean, stages: Stages[Input, Unit])
    extends Stages[Input, Unit] {
  override def collect: Seq[Stage[Nothing, Any]] =
    if (condition)
      Seq(FunctionStage((_: Input) => ()))
    else
      stages.collect
}

case class Branch[Input, Output](
    condition: Boolean,
    tt: Stages[Input, Output],
    ff: Stages[Input, Output],
) extends Stages[Input, Output] {
  override def collect: Seq[Stage[Nothing, Any]] =
    if (condition)
      tt.collect
    else
      ff.collect
}

case class TimedStages[Input, Output](
    name: String,
    stages: Stages[Input, Output],
) extends Stages[Input, Output] with LazyLogging {
  var start: java.time.Instant = null
  var end: java.time.Instant = null

  override def collect: Seq[Stage[Nothing, Any]] =
    IdentityStage().also {
      start = java.time.Instant.now();
      logger.warn(s"Start: $name (at ${Time.formatTime(start)})")
    }.collect ++ stages.collect ++ IdentityStage().also {
      end = java.time.Instant.now();
      logger.warn(s"Done: $name (at ${Time.formatTime(end)}, duration: ${Time
          .formatDuration(java.time.Duration.between(start, end))})")
    }.collect
}
