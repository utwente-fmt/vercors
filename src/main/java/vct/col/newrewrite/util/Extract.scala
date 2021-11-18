package vct.col.newrewrite.util

import vct.col.ast._
import vct.col.origin._
import vct.col.newrewrite.util.FreeVariables.{FreeVar, This}

import scala.collection.mutable

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
    val extract = Extract()
    val result = nodes.map(extract.extract)
    (result, extract.finish())
  }
}

case class Extract() {
  import Extract._

  private val map = mutable.Map[FreeVariables.FreeVariable, Variable]()

  private def update(node: Node): Map[Expr, Expr] =
    FreeVariables.freeVariables(node).map {
      case free @ FreeVar(v) => v ->
        Local(map.getOrElseUpdate(free, new Variable(v.t)(v.ref.decl.o)).ref)(ExtractOrigin(""))
      case free @ This(t) => t ->
        Local(map.getOrElseUpdate(free, new Variable(t.ref.get)(ExtractOrigin("this"))).ref)(ExtractOrigin(""))
    }.toMap[Expr, Expr]

  def extract(expr: Expr): Expr =
    Substitute(update(expr)).dispatch(expr)

  def extract(stat: Statement): Statement =
    Substitute(update(stat)).dispatch(stat)

  def finish(): Map[Variable, Expr] = {
    map.map {
      case (FreeVar(v), extracted) => extracted -> v
      case (This(t), extracted) => extracted -> t
    }.toMap
  }
}