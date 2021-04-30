package vct.col.ast

import vct.result.VerificationResult.SystemError

case class Program(decls: Seq[GlobalDeclaration]) {
  def check: Seq[CheckError] =
    decls.flatMap(_.checkTrans(CheckContext(scopes=Seq(decls.toSet))))
}

trait Node {
  def check(context: CheckContext): Seq[CheckError]
  def o: Origin

  def enterCheckContext(context: CheckContext): CheckContext =
    context

  /* Check children first, so that the check of nodes higher in the tree may depend on the type and correctness of
    subnodes */
  def checkTrans(context: CheckContext): Seq[CheckError] = {
    val childrenErrors = subnodes.flatMap(_.check(enterCheckContext(context)))

    if(childrenErrors.nonEmpty) {
      childrenErrors
    } else {
      check(context)
    }
  }

  def transSubnodes: Stream[Node] =
    this #:: subnodes.toStream.flatMap(_.transSubnodes)

  def subnodes: Seq[Node] = Subnodes.subnodes(this)
}

/*
  Marker trait to indicate this node, or this hierarchy of nodes, always rewrites to itself. This is for example for
  Expr (which always rewrites to an Expr), but also single-purpose nodes, such as a catch clause.
 */
trait NodeFamily extends Node

abstract class ASTStateError extends SystemError