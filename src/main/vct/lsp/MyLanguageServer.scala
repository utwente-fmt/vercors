package vct.lsp

import vct.lsp.MyLanguageServer.cancelledTokens
import org.eclipse.lsp4j._
import org.eclipse.lsp4j.services.{
  LanguageClient,
  LanguageServer,
  TextDocumentService,
  WorkspaceService,
}

import java.util.concurrent.CompletableFuture

class MyLanguageServer extends LanguageServer {
  private val textDocumentService = new MyTextDocumentService()
  private val workspaceService = new MyWorkspaceService()
  MyLanguageServer.client = null
  MyLanguageServer.textService = textDocumentService

  override def cancelProgress(params: WorkDoneProgressCancelParams): Unit = {
    val token = params.getToken.getLeft
    cancelledTokens.put(token, true)
  }

  def setClient(client: LanguageClient): Unit = {
    MyLanguageServer.client = client
  }

  override def initialize(
      params: InitializeParams
  ): CompletableFuture[InitializeResult] = {
    System
      .setOut(new java.io.PrintStream(java.io.OutputStream.nullOutputStream()))
    System
      .setErr(new java.io.PrintStream(java.io.OutputStream.nullOutputStream()))
    val capabilities = new ServerCapabilities()
    capabilities.setTextDocumentSync(TextDocumentSyncKind.Incremental)
    capabilities.setCompletionProvider(
      new CompletionOptions(true, java.util.Collections.emptyList())
    )
    capabilities.setDefinitionProvider(true)

    capabilities.setExecuteCommandProvider(new ExecuteCommandOptions(
      java.util.Arrays.asList("vercors.lspVerify")
    ))

    CompletableFuture.completedFuture(new InitializeResult(capabilities))
  }

  override def shutdown(): CompletableFuture[AnyRef] =
    CompletableFuture.completedFuture(null)

  override def exit(): Unit = System.exit(0)

  override def getTextDocumentService: TextDocumentService = textDocumentService

  override def getWorkspaceService: WorkspaceService = workspaceService
}

object MyLanguageServer {
  var client: LanguageClient = _
  val cancelledTokens = collection.mutable.Map[String, Boolean]()
  var textService: MyTextDocumentService = _
}
