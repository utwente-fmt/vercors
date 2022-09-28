package viper.api.transform

import vct.col.ast._
import vct.col.origin.AccountedDirection
import viper.silver.ast.Info

case class NodeInfo[T <: Node[_]](node: T) extends Info {
  var seqIndexSubscriptNode: Option[SeqSubscript[_]] = None
  var permissionValuePermissionNode: Option[Expr[_]] = None
  var predicatePath: Option[Seq[AccountedDirection]] = None
  var invariant: Option[LoopInvariant[_]] = None
  var starall: Option[Starall[_]] = None

  override def comment: Seq[String] = Nil
  override def isCached: Boolean = false
}
