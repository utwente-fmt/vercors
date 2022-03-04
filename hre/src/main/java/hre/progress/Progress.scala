package hre.progress

import hre.platform.Platform

case object Progress {
  val PROGRESS_BLOCKS = " ▏▎▍▌▋▊▉█"
  val MAX_BORING_WIDTH = 78
  val PROGRESS_BAR_WIDTH = 60

  def isTTY: Boolean = Option(System.console()).isDefined

//  private val wantProgress = isTTY
//  private val wantPrettyProgress = Platform.getCurrent match {
//    case Platform.Windows => false
//    case Platform.Unix => true
//    case Platform.Mac => true
//  }

  private val wantProgress = true
  private val wantPrettyProgress = isTTY

  def esc(command: Char, args: String = ""): String =
    "\u001b[" + args + command

  def up: String = esc('A', "1")
  def clearLine: String = esc('K')

  private var firstLogLine = true

  def undoProgressMessage: String =
    if(firstLogLine) ""
    else if(wantProgress) {
      if(wantPrettyProgress)
        up + clearLine + up + clearLine
      else {
        "\r" + " ".repeat(MAX_BORING_WIDTH) + "\r"
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
    val progress = (progressEstimate * PROGRESS_BAR_WIDTH * PROGRESS_BLOCKS.length).toInt
    val fullBlocks = progress / PROGRESS_BLOCKS.length
    val halfBlockIdx = progress % PROGRESS_BLOCKS.length

    if(halfBlockIdx == 0) PROGRESS_BLOCKS.last.toString.repeat(fullBlocks) + " ".repeat(PROGRESS_BAR_WIDTH - fullBlocks)
    else PROGRESS_BLOCKS.last.toString.repeat(fullBlocks) + PROGRESS_BLOCKS(halfBlockIdx) + " ".repeat(PROGRESS_BAR_WIDTH - fullBlocks - 1)
  }

  def framesText: String =
    frames.map(frame => {
      val idx = frame.position + 1
      val count = frame.count
      val desc = frame.currentMessage

      s"($idx/$count) $desc"
    }).mkString(" › ")

  def progressMessage: String = {
    firstLogLine = false

    if(wantProgress) {
      if(wantPrettyProgress) {
        framesText + "\n" + progressBar + "\n"
        f"$framesText\n[$progressBar] ${progressEstimate*100}%.1f%%\n"
      } else {
        f"[${progressEstimate*100}%.1f%%] $framesText".take(MAX_BORING_WIDTH)
      }
    } else ""
  }

  def update(): Unit = {
    System.out.print(undoProgressMessage)
    if(frames.isEmpty) {
      firstLogLine = true
    } else {
      System.out.print(progressMessage)
    }
  }

  case class Phase(description: String, weight: Int)
  abstract class Frame {
    var position: Int = 0
    def currentWeight: Int
    def currentMessage: String
    def count: Int
    def totalWeight: Int
    def weightDone: Int
  }

  case class ConcreteFrame(phases: Seq[Phase]) extends Frame {
    override def currentWeight: Int = if(0 <= position && position < phases.length) phases(position).weight else 0
    override def currentMessage: String = if(0 <= position && position < phases.length) phases(position).description else "?"
    override def count: Int = phases.size
    override def totalWeight: Int = phases.map(_.weight).sum
    override def weightDone: Int = phases.take(position).map(_.weight).sum
  }

  case class LazyFrame(count: Int, var currentMessage: String) extends Frame {
    override def currentWeight: Int = 1
    override def totalWeight: Int = count
    override def weightDone: Int = position
  }

  private var frames: Seq[Frame] = Nil

  private def withFrame[T](frame: Frame)(f: => T): T = {
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
    withFrame(LazyFrame(count, initialMessage))(f)

  def seqStages[T, S](xs: Iterable[T], desc: T => String)(f: => S): S =
    withFrame(ConcreteFrame(xs.map(x => Phase(desc(x), 1)).toSeq))(f)

  def foreach[T](xs: Iterable[T], desc: T => String)(f: T => Unit): Unit =
    seqStages(xs, desc) {
      xs.foreach(x => {
        f(x)
        nextPhase()
      })
    }

  def map[T, S](xs: Iterable[T], desc: T => String)(f: T => S): Iterable[S] =
    seqStages(xs, desc) {
      xs.map(x => {
        val result = f(x)
        nextPhase()
        result
      })
    }

  def nextPhase(nextMessage: String = ""): Unit = {
    frames.last.position += 1

    frames.last match {
      case _: ConcreteFrame =>
      case frame: LazyFrame =>
        frame.currentMessage = nextMessage
    }

    update()
  }
}
