package vct.col.rewrite.util

import vct.col.ast._
import vct.col.origin._
import vct.col.rewrite.util.FreeVariables.{FreeThisModel, FreeThisObject, FreeVar}
import vct.col.util.Substitute

import scala.collection.mutable

/**
 * Substitute fresh variables for values that are free under a node (or nodes)
 */
case object Extract {
  case class ExtractOrigin(name: String) extends Origin {
    override def preferredName: String = name
    override def shortPosition: String = "generated"
    override def context: String = "[At extracted expression]"
    override def inlineContext: String = "[Extracted expression]"
  }

  def extract[G](nodes: Expr[G]*): (Seq[Expr[G]], Map[Variable[G], Expr[G]]) = {
    val extract = Extract[G]()
    val result = nodes.map(extract.extract)
    (result, extract.finish())
  }
}

case class Extract[G]() {
  import Extract._

  private val map = mutable.Map[FreeVariables.FreeVariable[G], Variable[G]]()

  private def update(node: Node[G]): Map[Expr[G], Expr[G]] =
    FreeVariables.freeVariables(node).map {
      case free @ FreeVar(v) => v ->
        Local(map.getOrElseUpdate(free, new Variable(v.t)(v.ref.decl.o)).ref[Variable[G]])(ExtractOrigin(""))
      case free @ FreeThisObject(t) => t ->
        Local(map.getOrElseUpdate(free, new Variable(TClass(t.cls))(ExtractOrigin("this"))).ref[Variable[G]])(ExtractOrigin(""))
      case free @ FreeThisModel(t) => t ->
        Local(map.getOrElseUpdate(free, new Variable(TModel(t.cls))(ExtractOrigin("this"))).ref[Variable[G]])(ExtractOrigin(""))
    }.toMap[Expr[G], Expr[G]]

  def extract(expr: Expr[G]): Expr[G] =
    Substitute(update(expr)).dispatch(expr)

  def extract(stat: Statement[G]): Statement[G] =
    Substitute(update(stat)).dispatch(stat)

  def finish(): Map[Variable[G], Expr[G]] = {
    map.map {
      case (FreeVar(v), extracted) => extracted -> v
      case (FreeThisObject(t), extracted) => extracted -> t
      case (FreeThisModel(t), extracted) => extracted -> t
    }.toMap
  }
}