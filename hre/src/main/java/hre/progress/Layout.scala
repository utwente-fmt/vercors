package hre.progress

import hre.perf.Profile
import org.fusesource.jansi.Ansi.ansi
import org.fusesource.jansi.{AnsiConsole, AnsiType}

case object Layout {
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

  def maxHeight: Int = 32

  private def esc(command: Char, args: String = ""): String =
    "\u001b[" + args + command

  private def upBy(n: Int): String = if(n==0) "" else esc('A', n.toString)

  private def clearLine: String = esc('K')
  private def clearToEnd: String = esc('J', "0")

  private var printedLines = 0

  def undoProgressMessage: String =
    if (wantProgress) {
      if (wantPrettyProgress) {
        val clearLines = printedLines
        printedLines = 0
        upBy(clearLines) + clearToEnd
      } else {
        "\r" + " ".repeat(maxWidth) + "\r"
      }
    } else ""

  def progressEstimate: Double = TaskRegistry.getRootTask.progress

  def progressBadge: String =
    f"[${progressEstimate * 100}%.1f%%]"

  def progressBar: String = {
    val prefix = progressBadge + " ["
    val postfix = "]"
    val maxProgressBarWidth = maxWidth - prefix.length - postfix.length
    val bumpedProgress = progressEstimate * 0.99 + 0.01
    val progress = (bumpedProgress * maxProgressBarWidth * PROGRESS_BLOCKS.length).toInt
    val fullBlocks = progress / PROGRESS_BLOCKS.length
    val halfBlockIdx = progress % PROGRESS_BLOCKS.length

    prefix + (
      if (halfBlockIdx == 0) PROGRESS_BLOCKS.last.toString.repeat(fullBlocks) + " ".repeat(maxProgressBarWidth - fullBlocks)
      else PROGRESS_BLOCKS.last.toString.repeat(fullBlocks) + PROGRESS_BLOCKS(halfBlockIdx) + " ".repeat(maxProgressBarWidth - fullBlocks - 1)
    ) + postfix
  }

  def progressMessage: String = if (/*frames.nonEmpty*/true) {
    if (wantProgress) {
      if (wantPrettyProgress) {
        val lines = TaskRegistry.getRootTask.render(maxWidth, maxHeight-1)
        printedLines = lines.size + 1
        (lines :+ progressBar).mkString("", f"%n", f"%n")
      } else {
        val lines = TaskRegistry.getRootTask.render(maxWidth, 1)
        s"$progressBadge ${lines.last}"
      }
    } else ""
  } else {
    ""
  }

  def update(): Unit = {
    System.out.print(undoProgressMessage)
    System.out.print(progressMessage)
  }
}
