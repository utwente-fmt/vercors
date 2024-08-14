package hre.lsp.wire

import upickle.default.{readwriter, ReadWriter => RW}

object Identifier {
  implicit val rw: RW[Identifier] = readwriter[ujson.Value].bimap[Identifier](
    {
      case IdentifierInt(id) => ujson.Num(id)
      case IdentifierString(id) => ujson.Str(id)
    },
    {
      case ujson.Num(n) => IdentifierInt(n.toInt)
      case other => IdentifierString(other.str)
    },
  )
}

sealed trait Identifier
case class IdentifierInt(id: Int) extends Identifier
case class IdentifierString(id: String) extends Identifier
