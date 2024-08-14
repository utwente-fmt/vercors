package hre.lsp.wire

import hre.lsp.wire.InitializeResult.{ServerCapabilities, ServerInfo}

import upickle.default.{ReadWriter => RW, macroRW}

object InitializeResult {
  implicit val rwServerInfo: RW[ServerInfo] = macroRW
  implicit val rwDiagnosticOptions: RW[DiagnosticOptions] = macroRW
  implicit val rwServerCapabilities: RW[ServerCapabilities] = macroRW
  implicit val rw: RW[InitializeResult] = macroRW

  case class ServerInfo(name: String = "VerCors-LSP", version: String = "0.1")

  case class ServerCapabilities(
      positionEncoding: String = "utf-16",
      diagnosticProvider: Option[DiagnosticOptions] = None,
  )

  case class DiagnosticOptions(
      identifier: String,
      interFileDependencies: Boolean,
      workspaceDiagnostics: Boolean,
  )
}

case class InitializeResult(
    capabilities: ServerCapabilities,
    serverInfo: ServerInfo,
)
