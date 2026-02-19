package vct.col.ast.`type`

import vct.col.ast.{TBool, TCInt, Type}
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.TCIntOps

trait TCIntImpl[G] extends TCIntOps[G] {
  this: TCInt[G] =>

  def shouldConvertFrom(other: Type[G]): Boolean = {
    other match {
      case t: TCInt[G] =>
        (signed, t.signed) match {
          case (false, false) | (true, true) => rank >= t.rank
          case (false, true) =>
            if (rank >= t.rank)
              true
            else if (
              t.storedBits > storedBits
            ) // Strictly greater than implies that the maximum of the unsigned value fits in the signed one
              false
            else
              ??? // Should lead to unsigned version of signed type
          case (true, false) =>
            if (t.rank >= rank)
              false
            else if (
              storedBits > t.storedBits
            ) // Strictly greater than implies that the maximum of the unsigned value fits in the signed one
              true
            else
              ??? // Should lead to unsigned version of signed type
        }
      case TBool() => true
    }
  }

  override def layout(implicit ctx: Ctx): Doc = Text("int")
}
