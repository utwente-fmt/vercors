package vct.lsp

import org.eclipse.lsp4j._
import org.eclipse.lsp4j.services.LanguageClient
import org.mockito.ArgumentCaptor
import org.mockito.Mockito._
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers
import vct.col.origin.VerificationFailure
import vct.result.VerificationError

class MyWorkspaceServiceTest extends AnyFunSuiteLike with Matchers {

  test("sendVerificationErrorDiagnostic falls back when no origin present") {
    val client = mock(classOf[LanguageClient])
    MyLanguageServer.client = client

    val err = mock(classOf[VerificationError])
    when(err.getMessage).thenReturn("Something went wrong")

    val uri = "file:///fake"
    VerificationErrorsUtils.sendVerificationErrorDiagnostic(uri, err)

    val msgCaptor = ArgumentCaptor.forClass(classOf[MessageParams])
    verify(client).showMessage(msgCaptor.capture())

    val shown = msgCaptor.getValue
    shown.getType shouldBe MessageType.Error
    val popupText = shown.getMessage
    popupText should include(
      "Verification failed, verification error without position, error type:"
    )
    popupText should include(err.getClass.getSimpleName)

    val diagCaptor = ArgumentCaptor.forClass(classOf[PublishDiagnosticsParams])
    verify(client).publishDiagnostics(diagCaptor.capture())

    val params = diagCaptor.getValue
    params.getUri shouldBe uri
    params.getDiagnostics.size() shouldBe 1
    val d = params.getDiagnostics.get(0)

    d.getSeverity shouldBe DiagnosticSeverity.Error
    d.getSource shouldBe "VerCors"
    d.getMessage shouldBe org.eclipse.lsp4j.jsonrpc.messages.Either.forLeft("Something went wrong")
    d.getRange.getStart.getLine shouldBe 0
    d.getRange.getStart.getCharacter shouldBe 0
    d.getRange.getEnd.getLine shouldBe 0
    d.getRange.getEnd.getCharacter shouldBe 0
  }

  test("Unhandled VerificationFailure") {
    val client = mock(classOf[LanguageClient])
    MyLanguageServer.client = client

    val vf = mock(classOf[VerificationFailure])
    when(vf.inlineDesc).thenReturn("Unexpected failure")
    when(vf.originsWithMessages).thenReturn(Nil)

    val uri = "file:///fake"
    VerificationErrorsUtils.sendUnexpectedFailureDiagnostics(uri, Seq(vf))

    val msgCaptor = ArgumentCaptor.forClass(classOf[MessageParams])
    verify(client).showMessage(msgCaptor.capture())
    val shown = msgCaptor.getValue
    shown.getType shouldBe MessageType.Error
    val popupText = shown.getMessage
    popupText should include("Unhandled verification failure")
    popupText should include(vf.getClass.getSimpleName)
    popupText should include("Unexpected failure")

    val diagCaptor = ArgumentCaptor.forClass(classOf[PublishDiagnosticsParams])
    verify(client).publishDiagnostics(diagCaptor.capture())
    val params = diagCaptor.getValue
    params.getUri shouldBe uri
    params.getDiagnostics.size() shouldBe 0
  }
}
