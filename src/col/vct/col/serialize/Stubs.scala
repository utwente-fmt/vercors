package vct.col.serialize

object GlobalDeclaration

trait Program {
  def writeTo(x: Any): Unit
}

object Program {
  def parseFrom(x: Any): Program = ???
}