package vct.col.origin

case object FileSpanningOrigin extends Origin {
  override def preferredName: String = "unknown"
  override def shortPosition: String = "multiple"
  override def context: String = "[At node that spans multiple files]"
  override def inlineContext: String = "[Node that spans multiple files]"
}
