package viper.api

import vct.col.ast.{Node, SeqSubscript, SilverResource}
import viper.silver.ast.Info

case class NodeInfo[T <: Node](node: T) extends Info {
  var seqIndexSubscriptNode: Option[SeqSubscript] = None
  var permissionValuePermissionNode: Option[SilverResource] = None

  override def comment: Seq[String] = Nil
  override def isCached: Boolean = false
}
