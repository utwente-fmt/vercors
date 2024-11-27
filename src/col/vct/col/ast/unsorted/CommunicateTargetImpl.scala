package vct.col.ast.unsorted

import vct.col.ast.node.NodeFamilyImpl
import vct.col.ast.{
  CommTargetEndpoint,
  CommTargetIndex,
  CommTargetRange,
  CommunicateTarget,
  Endpoint,
}
import vct.col.print._
import vct.col.ast.ops.CommunicateTargetFamilyOps
import vct.col.check.{CheckContext, CheckError}
import vct.col.ref.Ref

trait CommunicateTargetImpl[G] extends CommunicateTargetFamilyOps[G] {
  this: CommunicateTarget[G] =>
//  override def layout(implicit ctx: Ctx): Doc = this.layout

  def ref: Ref[G, Endpoint[G]]

  def asName: CommTargetEndpoint[G] =
    this match {
      case target: CommTargetEndpoint[G] => target
      case _ => ???
    }

  def asIndex: CommTargetIndex[G] =
    this match {
      case target: CommTargetIndex[G] => target
      case _ => ???
    }

  def asRange: CommTargetRange[G] =
    this match {
      case target: CommTargetRange[G] => target
      case _ => ???
    }

  override def check(context: CheckContext[G]): Seq[CheckError] = Nil
}
