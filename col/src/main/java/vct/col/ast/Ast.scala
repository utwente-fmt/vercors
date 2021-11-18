package vct.col.ast

import vct.col.check.{CheckContext, CheckError, TypeError, TypeErrorText}
import vct.col.coerce.{CoercingRewriter, NopCoercingRewriter}
import vct.col.print.Printer
import vct.col.origin._
import vct.result.VerificationResult.SystemError

import scala.runtime.ScalaRunTime

case class Program(declarations: Seq[GlobalDeclaration])(val blame: Blame[UnsafeCoercion])(implicit val o: Origin) extends NodeFamily with Declarator {
  def check: Seq[CheckError] =
    checkTrans(CheckContext())

  override def check(context: CheckContext): Seq[CheckError] = Nil
}

trait Node {
  def check(context: CheckContext): Seq[CheckError]
  def o: Origin

  def enterCheckContext(context: CheckContext): CheckContext =
    context

  /* Check children first, so that the check of nodes higher in the tree may depend on the type and correctness of
    subnodes */
  def checkTrans(context: CheckContext): Seq[CheckError] = {
    val innerContext = enterCheckContext(context)
    val childrenErrors = subnodes.flatMap(_.check(innerContext))

    if(childrenErrors.nonEmpty) {
      childrenErrors
    } else {
      check(context)
    }
  }

  def transSubnodes: Stream[Node] =
    this #:: subnodes.toStream.flatMap(_.transSubnodes)

  def subnodes: Seq[Node] = Subnodes.subnodes(this)

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
        case p: Product => ScalaRunTime._toString(p)
        // Otherwise, fall back to the ugly Type@hexAdress notation
        case _ => super.toString
      }) + s" (err: ${t.getClass.getCanonicalName})"
    }
  }
}

/*
  Marker trait to indicate this node, or this hierarchy of nodes, always rewrites to itself. This is for example for
  Expr (which always rewrites to an Expr), but also single-purpose nodes, such as a catch clause.
 */
trait NodeFamily extends Node {
  override def check(context: CheckContext): Seq[CheckError] =
    try {
      NopCoercingRewriter.coerceAny(this)
      Nil
    } catch {
      case CoercingRewriter.Incoercible(e, t) => Seq(TypeError(e, t))
      case CoercingRewriter.IncoercibleText(e, m) => Seq(TypeErrorText(e, _ => m))
    }
}

trait Declarator extends Node {
  def declarations: Seq[Declaration]
  override def enterCheckContext(context: CheckContext): CheckContext =
    context.withScope(declarations.toSet)
}

abstract class ASTStateError extends SystemError