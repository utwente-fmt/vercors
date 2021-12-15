package viper.api

import vct.col.ast.{Expr, Node, SeqSubscript}
import viper.silver.ast.Info

case class NodeInfo[T <: Node[_]](node: T) extends Info {
  var seqIndexSubscriptNode: Option[SeqSubscript[_]] = None
  var permissionValuePermissionNode: Option[Expr[_]] = None

  override def comment: Seq[String] = Nil
  override def isCached: Boolean = false
}
