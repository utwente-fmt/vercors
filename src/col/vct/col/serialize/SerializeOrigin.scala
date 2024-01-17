package vct.col.serialize

import vct.col.ast.{serialize => ser}
import vct.col.origin.Origin

import scala.annotation.unused

object SerializeOrigin {
  def deserialize(@unused origin: ser.Origin): Origin =
    Origin(Nil)

  def serialize(@unused origin: Origin): ser.Origin =
    ser.Origin(Nil)
}
