package vct.result

import vct.result.Message.{BOLD_HR, HR}

trait HasContext {

  /** May contain newlines, but must not start or end with newlines.
    */
  def contextText: String

  def bareMessageInContext(message: String): String =
    contextText + "\n" + HR + message

  def messageInContext(message: String): String =
    BOLD_HR + bareMessageInContext(message) + "\n" + BOLD_HR
}
