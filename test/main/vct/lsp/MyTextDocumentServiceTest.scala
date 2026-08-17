package vct.lsp

import org.eclipse.lsp4j._
import org.eclipse.lsp4j.services.LanguageClient
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito._
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.TreeMap

class MyTextDocumentServiceTest extends AnyFunSuiteLike with Matchers {

  test("test Definition when match is found") {
    val service = new MyTextDocumentService()
    service.originMap = TreeMap((10, 5, 15) -> (20, 2, 7))

    val params = new DefinitionParams()
    params.setTextDocument(new TextDocumentIdentifier("file:///fake"))
    params.setPosition(new Position(10, 5))

    val result = service.definition(params).get()

    result.isRight shouldBe true

    result.getRight.size() shouldBe 1

    val location = result.getRight.get(0)
    location.getTargetUri shouldBe "file:///fake"
    location.getTargetRange.getStart.getLine shouldBe 20
    location.getTargetRange.getStart.getCharacter shouldBe 2
    location.getTargetRange.getEnd.getCharacter shouldBe 7
  }

  test("test Definition when match is not found") {
    val service = new MyTextDocumentService()
    val client = mock(classOf[LanguageClient])
    MyLanguageServer.client = client
    service.originMap = TreeMap((10, 5, 15) -> (20, 2, 7))

    val params = new DefinitionParams()
    params.setTextDocument(new TextDocumentIdentifier("file:///fake"))
    params.setPosition(new Position(10, 4))

    val result = service.definition(params).get()

    result.isRight shouldBe true
    result.getRight.size() shouldBe 0

  }

  test("test Definition when there is no location coordinates") {
    val service = new MyTextDocumentService()
    val client = mock(classOf[LanguageClient])
    MyLanguageServer.client = client
    service.originMap = TreeMap.empty

    val params = new DefinitionParams()
    params.setTextDocument(new TextDocumentIdentifier("file:///fake"))
    params.setPosition(new Position(5, 5))

    val resultFuture = service.definition(params)
    val result = resultFuture.get()

    result.isRight shouldBe true
    result.getRight.size() shouldBe 0

    verify(client).showMessage(any())
  }
}
