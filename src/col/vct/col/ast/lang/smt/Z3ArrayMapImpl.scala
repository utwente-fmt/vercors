package vct.col.ast.lang.smt

import vct.col.ast.Z3ArrayMap
import vct.col.ast.Type
import vct.col.print._
import vct.col.ast.ops.Z3ArrayMapOps

trait Z3ArrayMapImpl[G] extends Z3ArrayMapOps[G] { this: Z3ArrayMap[G] =>
  override def t: Type[G] = ref.ref.decl.returnType
  // def layout(implicit ctx: Ctx): Doc = ???
}
