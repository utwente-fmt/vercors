package vct.col.newrewrite.util

import vct.col.ast._
import vct.col.origin._
import vct.col.newrewrite.util.FreeVariables.{FreeVar, This}

/**
 * Substitute fresh variables for values that are free under a node (or nodes)
 */
case object Extract {
  case class ExtractOrigin(name: String) extends Origin {
    override def preferredName: String = name
    override def messageInContext(message: String): String =
      s"At: [extracted expression]\n$message"
  }

  def extract(nodes: Expr*): (Seq[Expr], Map[Variable, Expr]) = {
    val map = nodes.flatMap(FreeVariables.freeVariables(_)).distinct.map {
      case FreeVar(v) => new Variable(v.t)(v.ref.decl.o) -> v
      case This(t) => new Variable(t.ref.get)(ExtractOrigin("this")) -> t
    }.toMap

    val substitute = Substitute(map.map(pair => pair._2 -> Local(pair._1.ref)(ExtractOrigin(""))).toMap)

    (nodes.map(substitute.dispatch), map)
  }
}
