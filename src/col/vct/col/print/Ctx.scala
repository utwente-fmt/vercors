package vct.col.print

import vct.col.ast.{Declaration, Node}
import vct.col.resolve.ctx.Referrable

object Ctx {
  def computeNames(node: Node[_]): Map[Referrable[_], String] = {
    Map.empty
  }
}

case class Ctx(
  lang: Any = null,
  width: Int = 120,
  tabWidth: Int = 4,
  names: Map[Referrable[_], String] = Map.empty,
) {
  def namesIn(node: Node[_]): Ctx =
    copy(names = Ctx.computeNames(node))

  def name(decl: Declaration[_]): String =
    names.getOrElse(Referrable.from(decl).head, s"?unnamed:${decl.o.preferredName}?")
}
