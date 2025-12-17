package vct.col.ast.lang.c

import vct.col.ast.{CTArray, ConstantInt, Type}
import vct.col.print.{Ctx, Doc, Group}
import vct.col.ast.ops.CTArrayOps
import vct.col.typerules.TypeSize

trait CTArrayImpl[G] extends CTArrayOps[G] {
  this: CTArray[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(innerType.show <> "[" <> Doc.args(size.toSeq) <> "]")

  override def bits: TypeSize =
    size match {
      case Some(size: ConstantInt[G]) =>
        innerType.bits match {
          case TypeSize.Unknown() => TypeSize.Unknown()
          case TypeSize.Exact(s) => TypeSize.Exact(size.value * s)
          case TypeSize.Minimally(s) => TypeSize.Minimally(size.value * s)
        }
      // Will be special-cased in LangCToCol#sizeOf
      case _ => TypeSize.Minimally(0)
    }

  def innerMostType: Type[G] = {
    var current: Type[G] = this
    while (current.isInstanceOf[CTArray[G]]) {
      current = current.asInstanceOf[CTArray[G]].innerType
    }
    current
  }
}
