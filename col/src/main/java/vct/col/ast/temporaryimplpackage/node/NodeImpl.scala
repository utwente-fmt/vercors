package vct.col.ast.temporaryimplpackage.node

import vct.col.ast._
import vct.col.check._
import vct.col.origin._
import vct.col.print.Printer

import scala.runtime.ScalaRunTime

trait NodeImpl[G] { this: Node[G] =>
  def check(context: CheckContext[G]): Seq[CheckError]
  def o: Origin

  def enterCheckContext(context: CheckContext[G]): CheckContext[G] =
    context

  /* Check children first, so that the check of nodes higher in the tree may depend on the type and correctness of
    subnodes */
  def checkTrans(context: CheckContext[G]): Seq[CheckError] = {
    val innerContext = enterCheckContext(context)
    val childrenErrors = subnodes.flatMap(_.checkTrans(innerContext))

    if(childrenErrors.nonEmpty) {
      childrenErrors
    } else {
      check(context)
    }
  }

  def transSubnodes: LazyList[Node[G]] =
    this #:: subnodes.to(LazyList).flatMap(_.transSubnodes)

  def subnodes: Seq[Node[G]] = Subnodes.subnodes(this)

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
        case p: Product[_] => ScalaRunTime._toString(p)
        // Otherwise, fall back to the ugly Type@hexAdress notation
        case _ => super.toString
      }) + s" (err: ${t.getClass.getCanonicalName})"
    }
  }
}
