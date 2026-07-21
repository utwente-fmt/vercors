package vct.col.ast.expr.heap.read

import vct.col.ast.{
  CTArray,
  DerefPointer,
  PointerArrayType,
  TConstPointerArray,
  TNonNullConstPointerArray,
  TNonNullPointerArray,
  TPointerArray,
  Type,
}
import vct.col.print._
import vct.col.ast.ops.DerefPointerOps

trait DerefPointerImpl[G] extends DerefPointerOps[G] {
  this: DerefPointer[G] =>
  override def t: Type[G] =
    pointer.t match {
      case a: CTArray[G] => a.innerType
      case a: PointerArrayType[G] if a.dimensions.size > 1 => a.descend
      case a: PointerArrayType[G] => a.element
      case t => pointer.t.asPointer.get.element
    }

  override def precedence: Int = Precedence.PREFIX
  override def layout(implicit ctx: Ctx): Doc = Text("*") <> assoc(pointer)
}
