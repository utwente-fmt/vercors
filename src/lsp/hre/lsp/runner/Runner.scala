package hre.lsp.runner

import hre.lsp.channel.JsonObjectChannel
import hre.lsp.wire.{
  Diagnostic,
  DocumentDiagnosticParams,
  FullDocumentDiagnosticReport,
  InitializeParams,
  InitializeResult,
  Location,
  Position,
  Range,
  ResponseError,
}
import upickle.core.TraceVisitor
import upickle.default.{Reader, transform, writeJs}

case class Runner(transport: JsonObjectChannel, vercors: VercorsRunner) {
  def process(message: ujson.Obj): Unit = {
    try {
      if (!message.value.contains("id"))
        processNotification(message)
      else if (message.value.contains("method"))
        processRequest(message)
      else if (message.value.contains("result"))
        processResponse(message)
      else if (message.value.contains("error"))
        processErrorResponse(message)
      else
        invalidRequest(message)
    } catch { case _: ujson.Value.InvalidData => invalidRequest(message) }
  }

  def error(to: ujson.Obj, err: ResponseError): Unit =
    transport.write(ujson.Obj(
      "jsonrpc" -> "2.0",
      "id" -> to.value.getOrElse("id", null),
      "error" -> writeJs(err),
    ))

  def invalidRequest(message: ujson.Obj): Unit =
    error(message, ResponseError(-32600, "Invalid request"))

  def methodNotFound(message: ujson.Obj): Unit =
    error(message, ResponseError(-32601, "Method not found"))

  def processNotification(message: ujson.Obj): Unit = {}

  def respond(message: ujson.Obj, response: ujson.Value): Unit =
    transport.write(ujson.Obj(
      "jsonrpc" -> "2.0",
      "id" -> message.value("id"),
      "result" -> response,
    ))

  def readJs[T: Reader](value: ujson.Value): T =
    TraceVisitor
      .withTrace(trace = true, implicitly[Reader[T]])(transform(value).to[T])

  def processRequest(message: ujson.Obj): Unit = {
    message.value("method").str match {
      case "initialize" =>
        System.err.println("About to parse initialize message")
        System.err.printf("%s%n", readJs[InitializeParams](message("params")))
        respond(
          message,
          writeJs(InitializeResult(
            InitializeResult.ServerCapabilities(diagnosticProvider =
              Some(InitializeResult.DiagnosticOptions(
                identifier = "VerCors",
                interFileDependencies = true,
                workspaceDiagnostics = false,
              ))
            ),
            InitializeResult.ServerInfo(),
          )),
        )
      case "textDocument/diagnostic" =>
        val params = readJs[DocumentDiagnosticParams](message("params"))
        System.err.printf("%s%n", params)
        respond(
          message,
          writeJs(FullDocumentDiagnosticReport(
            resultId = None,
            items = Seq(Diagnostic(
              range = Range(Position(0, 0), Position(2, 1)),
              message =
                "\uD83D\uDEA8\uD83D\uDEA8 v detected \uD83D\uDEA8\uD83D\uDEA8",
              relatedInformation = Some(Seq(Diagnostic.RelatedInformation(
                Location(
                  params.textDocument.uri,
                  Range(Position(1, 0), Position(1, 1)),
                ),
                "this is where the next line is ok",
              ))),
            )),
          )),
        )
      case _ => methodNotFound(message)
    }
  }

  def processResponse(message: ujson.Obj): Unit = {}

  def processErrorResponse(message: ujson.Obj): Unit = {}

  def run(): Unit =
    while (
      transport.read() match {
        case Some(message) =>
          process(message)
          true
        case None => false
      }
    ) {}
}
