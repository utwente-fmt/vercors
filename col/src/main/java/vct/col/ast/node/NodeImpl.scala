package vct.col.ast.node

import vct.col.ast._
import vct.col.check._
import vct.col.origin._
import vct.col.print.Printer

import scala.runtime.ScalaRunTime

/**
 * [[Node]] is the root type for all nodes in the abstract syntax tree. The family is sealed, meaning the compiler
 * guarantees that all node definitions occur in one file; Node.scala.
 *
 * Note that the structure of the Node file is restricted: helpers are generated based on the definitions in the file,
 * so you cannot write arbitrary code in the file. Please refer to the developer documentation in the
 * <a href="https://github.com/utwente-fmt/vercors/wiki">VerCors wiki</a> for details.
 *
 * [[Node]] is subdivided into categories. The root of each category extends the marker trait [[NodeFamily]]; the code
 * generation looks for this trait to determine the node categories. Some of the bigger categories include:
 *
 * <ul>
 *   <li>[[Type]] contains all the types that can be declared and stored, as well as some helper types.</li>
 *   <li>[[Statement]] contains nodes that are executable, and may change the state.</li>
 *   <li>[[Expr]] contains nodes that have a defined value and type, given a state.</li>
 * </ul>
 *
 * The [[Declaration]] family has a special status, and is hence not marked as a [[NodeFamily]]. All direct descendants
 * of [[Declaration]] together constitute the "declaration kinds".
 *
 * For each node `equals` must be implemented in such a way that equality means they are semantically equivalent. For
 * most nodes that means simple structural equality, while for (references to) declarations this means identity
 * (reference) equality.
 *
 * @tparam G The generation marker: not used as a concrete type.
 */
trait NodeImpl[G] { this: Node[G] =>
  def check(context: CheckContext[G]): Seq[CheckError]
  def o: Origin

  def enterCheckContext(context: CheckContext[G]): CheckContext[G] =
    context

  /* Check children first, so that the check of nodes higher in the tree may depend on the type and correctness of
    subnodes */
  final def checkTrans(context: CheckContext[G]): Seq[CheckError] = {
    val childrenErrors = checkContextRecursor(context, (ctx, node) => node.checkTrans(ctx)).flatten

    if(childrenErrors.nonEmpty) {
      childrenErrors
    } else {
      check(context)
    }
  }

  /**
   * Applies a function to all subnodes, passing the appropriate context per subnode. By default, all subnodes get the
   * same context, as defined by [[enterCheckContext]]
   * @param context the initial context
   * @param f the function to apply to all subnodes
   * @tparam T the result type of the function
   * @return the result of applying `f` with the appropriate context per subnode to all subnodes
   */
  def checkContextRecursor[T](context: CheckContext[G], f: (CheckContext[G], Node[G]) => T): Seq[T] =
    subnodes.map(f(enterCheckContext(context), _))

  def subnodes: Seq[Node[G]] = Subnodes.subnodes(this)

  def transSubnodes: LazyList[Node[G]] =
    this #:: subnodes.to(LazyList).flatMap(_.transSubnodes)

  def map[T](f: PartialFunction[Node[G], T]): LazyList[T] =
    f.lift(this) match {
      case Some(value) => LazyList(value)
      case None => subnodes.to(LazyList).flatMap(_.map(f))
    }

  def flatMap[T](f: PartialFunction[Node[G], Seq[T]]): LazyList[T] =
    f.lift(this) match {
      case Some(value) => value.to(LazyList)
      case None => subnodes.to(LazyList).flatMap(_.flatMap(f))
    }

  def unsafeTransmuteGeneration[TNode[_] <: Node[_], G2]
                               (implicit witness: this.type <:< TNode[G])
                               : TNode[G2] = (this : TNode[G]).asInstanceOf[TNode[G2]]

  override def toString: String = {
    try {
      val sb = new java.lang.StringBuilder
      val printer = Printer(sb)
      printer.print(this)
      sb.toString
    } catch {
      // If the printer has a bug, try to print a useful representation
      case t: Throwable => (this match {
        // Case classes are automatically a product type, which produces the nice Type(arg1, arg2) representation.
        case p: scala.Product => ScalaRunTime._toString(p)
        // Otherwise, fall back to printing the subnodes
        case _ => s"${this.getClass.getSimpleName}(${subnodes.map(_.toString).mkString(", ")})"
      }) + s" (err: ${t.getClass.getCanonicalName})"
    }
  }
}
