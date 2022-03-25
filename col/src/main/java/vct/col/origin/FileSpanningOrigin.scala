package vct.col.origin

case object FileSpanningOrigin extends Origin {
  override def preferredName: String = "unknown"
  override def context: String = "[At node that spans multiple files]"
}
