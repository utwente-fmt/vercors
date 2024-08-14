package hre.lsp.wire

import hre.lsp.wire.DocumentUri.SCHEME_ALLOWLIST
import upickle.default.{macroRW, readwriter, ReadWriter => RW}

import java.net.URI

object DocumentUri {
  implicit val rw: RW[DocumentUri] = readwriter[ujson.Str]
    .bimap(_.uri.toString, s => DocumentUri(new URI(s.value)))

  val SCHEME_ALLOWLIST: Set[String] = Set("file", "data")
}

case class DocumentUri(uri: URI) {
  require(SCHEME_ALLOWLIST.contains(uri.getScheme))
}
