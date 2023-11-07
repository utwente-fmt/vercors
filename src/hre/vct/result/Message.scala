package vct.result

case object Message {
  val BOLD_HR = "======================================\n"
  val HR =      "--------------------------------------\n"

  def messagesInContext(messages: (HasContext, String)*): String =
    messages.zipWithIndex.map {
      case ((origin, message), idx) =>
        origin.bareMessageInContext(s"[${idx + 1}/${messages.size}] $message")
    }.mkString(BOLD_HR, HR, BOLD_HR)
}
