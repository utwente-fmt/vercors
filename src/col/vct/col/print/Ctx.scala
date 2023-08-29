package vct.col.print

import vct.col.ast.{Declaration, Node}
import vct.col.ref.Ref

import scala.util.Try

object Ctx {
  sealed trait Syntax
  case object PVL extends Syntax
  case object Silver extends Syntax
  case object Java extends Syntax
  case object C extends Syntax
  case object CPP extends Syntax
  case object Cuda extends Syntax
  case object OpenCL extends Syntax
}

case class Ctx(
  syntax: Ctx.Syntax = Ctx.PVL,
  width: Int = 120,
  tabWidth: Int = 4,
  names: Map[Declaration[_], String] = Map.empty,
  inSpec: Boolean = false,
) {
  def namesIn[G](node: Node[G]): Ctx =
    copy(names = {
      val namer = Namer[G](syntax)
      namer.name(node)
      namer.finish.asInstanceOf[Map[Declaration[_], String]]
    })

  def name(decl: Declaration[_]): String =
    names.getOrElse(decl, s"${decl.o.preferredName}_${decl.hashCode()}")

  def name(ref: Ref[_, _ <: Declaration[_]]): String =
    name(Try(ref.decl).getOrElse(return "?brokenref?"))
}
