package vct.lsp

import org.eclipse.lsp4j.{MessageParams, MessageType}

object LspMessages {
  def showMessage(messageType: MessageType, text: String): Unit = {
    MyLanguageServer.client.showMessage(new MessageParams(messageType, text))
  }

  def showError(text: String): Unit = showMessage(MessageType.Error, text)
  def showInfo(text: String): Unit = showMessage(MessageType.Info, text)
  def showWarning(text: String): Unit = showMessage(MessageType.Warning, text)
}
