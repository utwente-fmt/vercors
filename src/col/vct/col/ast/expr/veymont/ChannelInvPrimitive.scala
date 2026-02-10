package vct.col.ast.expr.veymont

sealed trait ChannelInvRole {
  def invert: Option[ChannelInvRole]
}
object ChannelInvRole {
  object Sender extends ChannelInvRole {
    override def invert: Option[ChannelInvRole] = Some(Receiver)
  }
  object Receiver extends ChannelInvRole {
    override def invert: Option[ChannelInvRole] = Some(Sender)
  }
  object Message extends ChannelInvRole {
    override def invert: Option[ChannelInvRole] = None
  }
}

trait ChannelInvPrimitive[G] {
  def role: ChannelInvRole
  def complements(primitive: ChannelInvPrimitive[G]): Boolean = primitive.role.invert.contains(role)
}
