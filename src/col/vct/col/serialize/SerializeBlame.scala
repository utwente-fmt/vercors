package vct.col.serialize

import vct.col.origin.{Blame, Origin, VerificationFailure}
import vct.col.ast.{serialize => ser}

import scala.annotation.unused

object SerializeBlame {
  def deserialize[T <: VerificationFailure](
      @unused
      blame: ser.Blame,
      origin: Origin,
  ): Blame[T] = origin

  def serialize(blame: Blame[_]): ser.Blame =
    ser.Blame(ser.Blame.Blame.BlameInput(ser.BlameInput()))
}
