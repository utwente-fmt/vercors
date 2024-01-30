package vct.rewrite.runtime.util.permissionTransfer

import vct.col.ast.{Variable, _}
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.rewrite.runtime.util.LedgerHelper.LedgerMethodBuilderHelper

import scala.annotation.tailrec
import scala.collection.immutable.Seq



sealed trait PermissionContent[G <: Generation]
case class ClassArg[G <: Generation](cls: Class[G]) extends PermissionContent[G]
case class Outer[G <: Generation](outer: Rewriter[G]) extends PermissionContent[G]
case class FactorContent[G <: Generation](factor: Expr[Rewritten[G]]) extends PermissionContent[G]
case class TreadIdContent[G <: Generation](threadId: Expr[Rewritten[G]]) extends PermissionContent[G]
case class Offset[G <: Generation](offset: Expr[Rewritten[G]]) extends PermissionContent[G]
case class Ledger[G <: Generation](ledger: LedgerMethodBuilderHelper[Rewritten[G]]) extends PermissionContent[G]

object PermissionData {
  def apply[Pre <: Generation](): PermissionData[Pre] = {
    PermissionData[Pre](Seq.empty)
  }
}

case class PermissionData[Pre <: Generation](permissionContent: Seq[PermissionContent[Pre]]) {
  type Post = Rewritten[Pre]

  lazy val cls : Option[Class[Pre]] = permissionContent.collectFirst { case c: ClassArg[Pre] => c }.map(c => c.cls)
  lazy val outer : Rewriter[Pre] = permissionContent.collectFirst { case o: Outer[Pre] => o }.map(o => o.outer).getOrElse(new Rewriter())
  lazy val factor: Option[Expr[Post]] = permissionContent.collectFirst { case o: FactorContent[Pre] => o }.map(f => f.factor)
  lazy val threadId: ThreadId[Post] = ThreadId[Post](permissionContent.collectFirst { case o: TreadIdContent[Pre] => o }.map(t => t.threadId))(Origin(Seq.empty))
  lazy val offset: Option[Expr[Post]] = permissionContent.collectFirst { case o: Offset[Pre] => o }.map(o => o.offset)
  lazy val ledger: Option[LedgerMethodBuilderHelper[Post]] = permissionContent.collectFirst { case o: Ledger[Pre] => o }.map(o => o.ledger)


  def factored(e: Expr[Post])(implicit origin: Origin = e.o): Expr[Post] = factor match {
    case Some(f: Expr[Post]) => f r_* e
    case _ => e
  }

  def getOffset(e: Expr[Pre]): Expr[Post] = {
    if(offset.isEmpty) {
      return outer.dispatch(e)
    }
    offset.get
  }

  def setCls(newArg: Class[Pre]): PermissionData[Pre] = {
    PermissionData[Pre](permissionContent.flatMap{
      case ea: ClassArg[Pre] => Nil
      case o => Seq(o)
    } :+ ClassArg[Pre](newArg))
  }

  def setOuter(newArg: Rewriter[Pre]): PermissionData[Pre] = {
    PermissionData[Pre](permissionContent.flatMap{
      case _: Outer[Pre] => Nil
      case o => Seq(o)
    } :+ Outer[Pre](newArg))
  }

  def setFactor(newArg: Expr[Post]): PermissionData[Pre] = {
    PermissionData[Pre](permissionContent.flatMap{
      case ea: FactorContent[Pre] => Nil
      case o => Seq(o)
    } :+ FactorContent[Pre](newArg))
  }

  def setThreadId(newArg: Expr[Post]): PermissionData[Pre] = {
    PermissionData[Pre](permissionContent.flatMap{
      case ea: TreadIdContent[Pre] => Nil
      case o => Seq(o)
    } :+ TreadIdContent[Pre](newArg))
  }

  def setOffset(newArg: Expr[Post]): PermissionData[Pre] = {
    PermissionData[Pre](permissionContent.flatMap{
      case ea: Offset[Pre] => Nil
      case o => Seq(o)
    } :+ Offset[Pre](newArg))
  }

  def setLedger(newArg: LedgerMethodBuilderHelper[Post]): PermissionData[Pre] = {
    PermissionData[Pre](permissionContent.flatMap {
      case ea: Ledger[Pre] => Nil
      case o => Seq(o)
    } :+ Ledger[Pre](newArg))
  }
}

