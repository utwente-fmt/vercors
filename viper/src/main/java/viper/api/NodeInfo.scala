package viper.api

import vct.col.ast.{Node, SeqSubscript, SilverResource}
import viper.silver.ast.Info

case class NodeInfo[T <: Node[_]](node: T) extends Info {
  var seqIndexSubscriptNode: Option[SeqSubscript[_]] = None
  var permissionValuePermissionNode: Option[SilverResource[_]] = None

  override def comment: Seq[String] = Nil
  override def isCached: Boolean = false
}
