package vct.col.ast.lang.smt

import vct.col.ast.{TSmtlibArray, Type, Z3ArrayOfFunction}
import vct.col.print._
import vct.col.ast.ops.Z3ArrayOfFunctionOps

trait Z3ArrayOfFunctionImpl[G] extends Z3ArrayOfFunctionOps[G] { this: Z3ArrayOfFunction[G] =>
  override lazy val t: Type[G] = TSmtlibArray(ref.ref.decl.args.map(_.t), ref.ref.decl.returnType)
  // def layout(implicit ctx: Ctx): Doc = ???
}
