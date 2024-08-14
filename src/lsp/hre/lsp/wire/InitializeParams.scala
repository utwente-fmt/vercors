package hre.lsp.wire

import hre.lsp.wire.InitializeParams.{
  ClientCapabilities,
  ClientInfo,
  WorkspaceFolder,
}
import upickle.default.{ReadWriter => RW, macroRW}

object InitializeParams {
  implicit val rwClientInfo: RW[ClientInfo] = macroRW
  implicit val rwClientCapabilities: RW[ClientCapabilities] = macroRW
  implicit val rwWorkspaceFolder: RW[WorkspaceFolder] = macroRW
  implicit val rw: RW[InitializeParams] = macroRW

  case class ClientInfo(name: String, version: Option[String] = None)
  case class ClientCapabilities()
  case class WorkspaceFolder(uri: String, name: String)
}

case class InitializeParams(
    processId: Option[Int] = None,
    clientInfo: Option[ClientInfo],
    rootPath: Option[String] = None,
    rootUri: Option[String] = None,
    capabilities: ClientCapabilities,
    workspaceFolders: Seq[WorkspaceFolder],
)
