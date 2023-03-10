package hre.progress

case object ProgressRender {
  val JOIN = " › "
  val HR: String = "-".repeat(38)
  val ELLIPSIS = " … "

  def apply(label: String): ProgressRender =
    ProgressRender(Seq(label), 0)
}

case class ProgressRender(lines: Seq[String], primaryLineIndex: Int) {
  def prefix(prefix: String): ProgressRender =
    copy(lines = lines.take(primaryLineIndex) ++ Seq(prefix + lines(primaryLineIndex)) ++ lines.drop(primaryLineIndex+1))

  def postfix(postfix: String): ProgressRender =
    copy(lines = lines.take(primaryLineIndex) ++ Seq(lines(primaryLineIndex) + postfix) ++ lines.drop(primaryLineIndex+1))

  def prefix(pre: ProgressRender, prefixOnFailure: String): ProgressRender =
    if(lines.size == 1) {
      pre.postfix(ProgressRender.JOIN + lines.head)
    } else if (pre.lines.size == 1) {
      prefix(pre.lines.head + ProgressRender.JOIN)
    } else {
      ProgressRender(
        pre.prefix(prefixOnFailure).lines ++ Seq(ProgressRender.HR) ++ lines,
        primaryLineIndex + pre.lines.size + 1
      )
    }

  def postfix(post: ProgressRender, prefixOnFailure: String): ProgressRender =
    post.prefix(this, prefixOnFailure)
}
