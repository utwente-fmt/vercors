package viper.api

import vct.col.ast.{Expr, Node, SeqSubscript}
import vct.col.origin.AccountedDirection
import viper.silver.ast.Info

case class NodeInfo[T <: Node[_]](node: T) extends Info {
  var seqIndexSubscriptNode: Option[SeqSubscript[_]] = None
  var permissionValuePermissionNode: Option[Expr[_]] = None
  var predicatePath: Option[Seq[AccountedDirection]] = None

  override def comment: Seq[String] = Nil
  override def isCached: Boolean = false
}
