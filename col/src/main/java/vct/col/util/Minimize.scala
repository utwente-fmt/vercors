package vct.col.util

import vct.col.origin

object Minimize {
  def mkOrigin(inner: origin.Origin, shouldFocus: Boolean, shouldIgnore: Boolean): origin.Origin =
    Option.when(shouldFocus)(Minimize.Mode.Focus)
      .orElse(Option.when(shouldIgnore)(Minimize.Mode.Ignore))
      .map(Minimize.Origin(inner, _))
      .getOrElse(inner)

  // Currently the minimization system piggybacks on the origin system.
  // To improve: the minimization targets could be a first-class property of the Program type
  // Similar to how the rootObject is a property of Program
  case class Origin(o: origin.Origin, mode: Mode) extends origin.Origin {
    override def preferredName: String = o.preferredName
    override def shortPosition: String = o.shortPosition
    override def context: String = o.context
    override def inlineContext: String = o.inlineContext
  }

  sealed abstract class Mode()
  object Mode {
    final case object Focus extends Mode
    final case object Ignore extends Mode
  }
}
