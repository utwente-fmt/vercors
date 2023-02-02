package vct.col.rewrite.util

import vct.col.ast._
import vct.col.origin._
import vct.col.rewrite.util.FreeVariables.{FreeThisModel, FreeThisObject, ReadFreeVar, ReadTypeVar, WriteFreeVar}
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

  /**
   * Note: expressions must be free of assignments to locals (not necessarily all side effects), and type variables.
   */
  def extract[G](nodes: Expr[G]*): (Seq[Expr[G]], Map[Variable[G], Expr[G]]) = {
    val extract = Extract[G]()
    val result = nodes.map(extract.extract)
    val (ts, in, out) = extract.finish()
    require(ts.isEmpty)
    require(out.isEmpty)
    (result, in)
  }
}

case class Extract[G]() {
  import Extract._

  private val map = mutable.Map[FreeVariables.FreeVariable[G], Variable[G]]()

  private def updateExprs(node: Node[G]): Map[Expr[G], Expr[G]] =
    FreeVariables.freeVariables(node).collect {
      case free @ ReadFreeVar(v) => v ->
        Local(map.getOrElseUpdate(free, new Variable(v.t)(v.ref.decl.o)).ref[Variable[G]])(ExtractOrigin(""))
      case free @ WriteFreeVar(v) => v ->
        Local(map.getOrElseUpdate(free, new Variable(v.t)(v.ref.decl.o)).ref[Variable[G]])(ExtractOrigin(""))
      case free @ FreeThisObject(t) => t ->
        Local(map.getOrElseUpdate(free, new Variable(TClass(t.cls))(ExtractOrigin("this"))).ref[Variable[G]])(ExtractOrigin(""))
      case free @ FreeThisModel(t) => t ->
        Local(map.getOrElseUpdate(free, new Variable(TModel(t.cls))(ExtractOrigin("this"))).ref[Variable[G]])(ExtractOrigin(""))
    }.toMap[Expr[G], Expr[G]]

  private def updateTypes(node: Node[G]): Map[TVar[G], Type[G]] =
    FreeVariables.freeVariables(node).collect {
      case free@ReadTypeVar(v) => v ->
        TVar(map.getOrElseUpdate(free, new Variable(v.ref.decl.t)(v.ref.decl.o)).ref[Variable[G]])(ExtractOrigin(""))
    }.toMap[TVar[G], Type[G]]

  def extract(expr: Expr[G]): Expr[G] =
    Substitute(updateExprs(expr), updateTypes(expr)).dispatch(expr)

  def extract(stat: Statement[G]): Statement[G] =
    Substitute(updateExprs(stat), updateTypes(stat)).dispatch(stat)

  def finish(): (Map[Variable[G], Type[G]], Map[Variable[G], Expr[G]], Map[Variable[G], Expr[G]]) = {
    (
      map.collect {
        case (ReadTypeVar(v), extracted) => extracted -> v
      }.toMap,
      map.collect {
        case (ReadFreeVar(v), extracted) => extracted -> v
        case (FreeThisObject(t), extracted) => extracted -> t
        case (FreeThisModel(t), extracted) => extracted -> t
      }.toMap,
      map.collect {
        case (WriteFreeVar(v), extracted) => extracted -> v
      }.toMap
    )
  }
}