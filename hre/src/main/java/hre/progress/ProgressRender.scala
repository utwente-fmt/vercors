package hre.progress

import hre.progress.ProgressRender._

case object ProgressRender {
  val JOIN = " › "
  val HR: String = "-".repeat(38)
  val ELLIPSIS = " … "

  val HORIZONTAL_LINE = "─"
  val VERTICAL_LINE = "│"
  val BRANCH = "├"
  val HOOK = "└"

  def apply(label: String): ProgressRender =
    ProgressRender(Seq(label), 0)
}

case class ProgressRender(lines: Seq[String], primaryLineIndex: Int) {
  def prefix(prefix: String): ProgressRender =
    copy(lines = lines.take(primaryLineIndex) ++ Seq(prefix + lines(primaryLineIndex)) ++ lines.drop(primaryLineIndex+1))

  def postfix(postfix: String): ProgressRender =
    copy(lines = lines.take(primaryLineIndex) ++ Seq(lines(primaryLineIndex) + postfix) ++ lines.drop(primaryLineIndex+1))

  def prefix(pre: ProgressRender, badgeOnFailure: String): ProgressRender =
    if (pre.lines.size == 1) {
      prefix(pre.lines.head + ProgressRender.JOIN)
    } else {
      pre.subRenders(Seq(badgeOnFailure -> this))
    }

  def postfix(post: ProgressRender, postBadgeOnFailure: String): ProgressRender =
    post.prefix(this, postBadgeOnFailure)

  def subRenders(renders: Seq[(String, ProgressRender)]): ProgressRender = {
    val prefixedInit = renders.init.map {
      case (badge, unbadgedRender) =>
        val render = unbadgedRender.prefix(badge)
        render.lines.zipWithIndex.map {
          case (line, idx) if idx == render.primaryLineIndex =>
            BRANCH + HORIZONTAL_LINE + " " + line
          case (line, _) =>
            VERTICAL_LINE + " " + " " + line
        }
    }

    val prefixedLast = {
      val (badge, unbadgedRender) = renders.last
      val render = unbadgedRender.prefix(badge)
      render.lines.zipWithIndex.map {
        case (line, idx) if idx < render.primaryLineIndex =>
          VERTICAL_LINE + " " + " " + line
        case (line, idx) if idx == render.primaryLineIndex =>
          HOOK + HORIZONTAL_LINE + " " + line
        case (line, _) =>
          " ".repeat(3) + line
      }
    }

    ProgressRender(lines ++ prefixedInit.flatten ++ prefixedLast, primaryLineIndex)
  }
}