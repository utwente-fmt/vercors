package hre.progress

import hre.platform.Platform
import org.fusesource.jansi.{Ansi, AnsiConsole, AnsiType}

import scala.collection.parallel.CollectionConverters.IterableIsParallelizable

case object Progress {
  var forceProgress: Boolean = false

  val PROGRESS_BLOCKS = " ▏▎▍▌▋▊▉█"

  def install(progress: Boolean): Unit = {
    forceProgress = progress
    AnsiConsole.systemInstall()
  }

  private def wantProgress: Boolean = AnsiConsole.out().getType match {
    case AnsiType.Native | AnsiType.VirtualTerminal | AnsiType.Emulation => true
    case AnsiType.Unsupported | AnsiType.Redirected => forceProgress
  }

  private def wantPrettyProgress: Boolean = AnsiConsole.out().getType match {
    case AnsiType.Native | AnsiType.VirtualTerminal | AnsiType.Emulation => true
    case AnsiType.Unsupported | AnsiType.Redirected => false
  }

  def maxWidth: Int = (AnsiConsole.out().getTerminalWidth match {
    case 0 => 80
    case other => other
  }) - 2

  def maxProgressBarWidth: Int = maxWidth - 8

  private def esc(command: Char, args: String = ""): String =
    "\u001b[" + args + command

  private def up: String = esc('A', "1")
  private def clearLine: String = esc('K')

  private var firstLogLine = true

  def undoProgressMessage: String =
    if(firstLogLine) ""
    else if(wantProgress) {
      if(wantPrettyProgress)
        up + clearLine + up + clearLine
      else {
        "\r" + " ".repeat(maxWidth) + "\r"
      }
    } else ""

  def progressEstimate: Double = if(frames.nonEmpty) {
    val (headStart, _) = frames.foldLeft((0.0, 1.0)) {
      case ((headStart, currentWidth), frame) =>
        val totalWeight = frame.totalWeight
        val weightDone = frame.weightDone
        val currentWeight = frame.currentWeight

        (headStart + currentWidth * weightDone / totalWeight, currentWidth * currentWeight / totalWeight)
    }

    headStart
  } else 1.0

  def progressBar: String = {
    val progress = (progressEstimate * maxProgressBarWidth * PROGRESS_BLOCKS.length).toInt
    val fullBlocks = progress / PROGRESS_BLOCKS.length
    val halfBlockIdx = progress % PROGRESS_BLOCKS.length

    if(halfBlockIdx == 0) PROGRESS_BLOCKS.last.toString.repeat(fullBlocks) + " ".repeat(maxProgressBarWidth - fullBlocks)
    else PROGRESS_BLOCKS.last.toString.repeat(fullBlocks) + PROGRESS_BLOCKS(halfBlockIdx) + " ".repeat(maxProgressBarWidth - fullBlocks - 1)
  }

  def framesText: String =
    frames.map(frame => {
      val idx = frame.position + 1
      val count = frame.count
      val desc = frame.currentMessage

      s"($idx/$count) $desc"
    }).mkString(" › ")

  def progressMessage: String = if(frames.nonEmpty) {
    firstLogLine = false

    if(wantProgress) {
      if(wantPrettyProgress) {
        framesText.take(maxWidth) + "\n" + f"[$progressBar] ${progressEstimate*100}%.1f%%" + "\n"
      } else {
        f"[${progressEstimate*100}%.1f%%] $framesText".take(maxWidth)
      }
    } else ""
  } else {
    firstLogLine = true
    ""
  }

  def update(): Unit = {
    System.out.print(undoProgressMessage)
    System.out.print(progressMessage)
  }

  case class Phase(description: String, weight: Int)
  abstract class Frame {
    def position: Int
    def currentWeight: Int
    def currentMessage: String
    def count: Int
    def totalWeight: Int
    def weightDone: Int
  }

  case class ConcreteFrame(phases: Seq[Phase]) extends Frame {
    var position: Int = 0
    override def currentWeight: Int = if(0 <= position && position < phases.length) phases(position).weight else 0
    override def currentMessage: String = if(0 <= position && position < phases.length) phases(position).description else "?"
    override def count: Int = phases.size
    override def totalWeight: Int = phases.map(_.weight).sum
    override def weightDone: Int = phases.take(position).map(_.weight).sum
  }

  case class SetMessageFrame(count: Int, var currentMessage: String) extends Frame {
    var position: Int = 0
    override def currentWeight: Int = 1
    override def totalWeight: Int = count
    override def weightDone: Int = position
  }

  case class UnorderedConcreteFrame(phases: Seq[Phase]) extends Frame {
    var todo: Set[Phase] = phases.toSet
    override def position: Int = phases.size - todo.size
    override def currentWeight: Int = 1
    override def currentMessage: String = phases.filter(todo.contains).map(_.description).mkString(", ")
    override def count: Int = phases.size
    override def totalWeight: Int = phases.size
    override def weightDone: Int = position
  }

  private var frames: Seq[Frame] = Nil

  /* private */ def withFrame[T](frame: Frame)(f: => T): T = {
    frames :+= frame
    update()
    try {
      f
    } finally {
      frames = frames.init
      if(frames.isEmpty) update()
    }
  }

  def stages[T](stages: Seq[(String, Int)])(f: => T): T =
    withFrame(ConcreteFrame(stages.map { case (desc, weight) => Phase(desc, weight) }))(f)

  def dynamicMessages[T](count: Int, initialMessage: String)(f: => T): T =
    withFrame(SetMessageFrame(count, initialMessage))(f)

  def seqStages[T, S](xs: Iterable[T], desc: T => String)(f: => S): S =
    withFrame(ConcreteFrame(xs.map(x => Phase(desc(x), 1)).toSeq))(f)

  def parStages[T, S](xs: Iterable[T], desc: T => String)(f: Seq[Phase] => S): S = {
    val phases = xs.map(x => Phase(desc(x), 1)).toSeq
    withFrame(UnorderedConcreteFrame(phases))(f(phases))
  }

  def foreach[T](xs: Iterable[T], desc: T => String)(f: T => Unit): Unit =
    seqStages(xs, desc) {
      xs.foreach(x => {
        f(x)
        next()
      })
    }

  def parForeach[T](xs: Iterable[T], desc: T => String)(f: T => Unit): Unit =
    parStages(xs, desc) { phases =>
      xs.zip(phases).par.foreach { case (x, phase) =>
        f(x)
        nextDone(phase)
      }
    }

  def map[T, S](xs: Iterable[T], desc: T => String)(f: T => S): Iterable[S] =
    seqStages(xs, desc) {
      xs.map(x => {
        val result = f(x)
        next()
        result
      })
    }

  def next(): Unit =
    this.synchronized {
      frames.last match {
        case frame: ConcreteFrame =>
          frame.position += 1
        case _ => ???
      }

      update()
    }

  def nextMessage(message: String): Unit =
    this.synchronized {
      frames.last match {
        case frame: SetMessageFrame =>
          frame.position += 1
          frame.currentMessage = message
        case _ => ???
      }

      update()
    }

  def nextDone(phase: Phase): Unit =
    this.synchronized {
      frames.last match {
        case frame: UnorderedConcreteFrame =>
          frame.todo -= phase
      }

      update()
    }
}
