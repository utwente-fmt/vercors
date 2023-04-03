package vct.col.print

import vct.col.ast.{Declaration, Node}
import vct.col.ref.Ref
import vct.col.resolve.ctx.Referrable

import scala.util.Try

object Ctx {
  sealed trait Syntax
  case object PVL extends Syntax
  case object Silver extends Syntax
  case object Java extends Syntax
  case object C extends Syntax
  case object Cuda extends Syntax
  case object OpenCL extends Syntax

  def computeNames(node: Node[_]): Map[Referrable[_], String] = {
    Map.empty
  }
}

case class Ctx(
  syntax: Ctx.Syntax = Ctx.Java,
  width: Int = 120,
  tabWidth: Int = 4,
  names: Map[Referrable[_], String] = Map.empty,
  inSpec: Boolean = false,
) {
  def namesIn(node: Node[_]): Ctx =
    copy(names = Ctx.computeNames(node))

  def name(decl: Declaration[_]): String =
    names.getOrElse(Referrable.from(decl).head, s"?unnamed:${decl.o.preferredName}?")

  def name(ref: Ref[_, _ <: Declaration[_]]): String =
    name(Try(ref.decl).getOrElse(return "?brokenref?"))
}
