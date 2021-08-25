package integration.helper

class StreamListener extends Appendable {
  var log: String = ""

  override def append(csq: CharSequence): Appendable = {
    log += csq
    this
  }

  override def append(csq: CharSequence, start: Int, end: Int): Appendable = {
    log += csq.subSequence(start,end)
    this
  }

  override def append(c: Char): Appendable = {
    log += c
    this
  }
}
