package vct.col.rewrite.util

import vct.col.ast._
import vct.col.origin._
import vct.col.rewrite.util.FreeVariables.{FreeThisModel, FreeThisObject, ReadFreeVar, ReadTypeVar, WriteFreeVar}
import vct.col.util.AstBuildHelpers.{VarBuildHelpers, assignLocal}
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
    val extract.Data(ts, in, inForOut, out, _) = extract.finish()
    require(ts.isEmpty)
    require(inForOut.isEmpty)
    require(out.isEmpty)
    (result, in)
  }
}

case class Extract[G]() {
  import Extract._

  private val map = mutable.Map[FreeVariables.FreeVariable[G], Variable[G]]()
  private val write = mutable.Set[Local[G]]()
  private val read = mutable.Set[Local[G]]()

  private def updateExprs(node: Node[G]): Map[Expr[G], Expr[G]] =
    FreeVariables.freeVariables(node).collect {
      case free @ ReadFreeVar(v) =>
        read += v
        v -> Local(map.getOrElseUpdate(free, new Variable(v.t)(v.ref.decl.o)).ref[Variable[G]])(ExtractOrigin(""))
      case WriteFreeVar(v) =>
        write += v
        v -> Local(map.getOrElseUpdate(ReadFreeVar(v), new Variable(v.t)(v.ref.decl.o)).ref[Variable[G]])(ExtractOrigin(""))
      case free @ FreeThisObject(t) =>
        t -> Local(map.getOrElseUpdate(free, new Variable(TClass(t.cls))(ExtractOrigin("this"))).ref[Variable[G]])(ExtractOrigin(""))
      case free @ FreeThisModel(t) =>
        t -> Local(map.getOrElseUpdate(free, new Variable(TModel(t.cls))(ExtractOrigin("this"))).ref[Variable[G]])(ExtractOrigin(""))
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

  case class Data(types: Map[Variable[G], Type[G]],
                  in: Map[Variable[G], Expr[G]],
                  inForOut: Map[(Variable[G], Variable[G]), Expr[G]],
                  out: Map[Variable[G], Expr[G]],
                  initialAssignments: Statement[G])

  def finish(): Data = {
    val types = map.collect {
      case (ReadTypeVar(v), extracted) => extracted -> v
    }.toMap

    // Locals that are not written: their usage becomes a parameter
    val in = map.collect {
      case (ReadFreeVar(v), extracted) if !write.contains(v) => extracted -> v
      case (FreeThisObject(t), extracted) => extracted -> t
      case (FreeThisModel(t), extracted) => extracted -> t
    }.toMap

    // Locals that are written and read: get an initial value as a parameter, all usages are an out parameter
    val inForOut = map.collect {
      case (ReadFreeVar(v), extracted) if read.contains(v) && write.contains(v) =>
        (new Variable[G](v.t)(v.ref.decl.o), extracted) -> v
    }.toMap

    // Locals that are written and read: the out parameter is assigned the initial value immediately
    val initialAssignments = Block((for(((in, out), _) <- inForOut) yield {
      implicit val o: Origin = in.o
      assignLocal(out.get, in.get)
    }).toSeq)(ExtractOrigin(""))

    // Locals that are written: get an out parameter. The initial value is skipped if not read.
    val out = map.collect {
      case (ReadFreeVar(v), extracted) if write.contains(v) => extracted -> v
    }.toMap

    Data(types, in, inForOut, out, initialAssignments)
  }
}