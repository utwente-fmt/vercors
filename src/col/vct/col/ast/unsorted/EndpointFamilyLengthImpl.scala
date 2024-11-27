package vct.col.ast.unsorted

import vct.col.ast.{EndpointFamilyLength, TInt, Type}
import vct.col.ast.ops.EndpointFamilyLengthOps
import vct.col.print._

trait EndpointFamilyLengthImpl[G] extends EndpointFamilyLengthOps[G] {
  this: EndpointFamilyLength[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???

  override def t: Type[G] = TInt()
}
