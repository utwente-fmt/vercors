package vct.col.ast.lang.c

import vct.col.ast.TOpenCLVector
import vct.col.ast.ops.TOpenCLVectorOps
import vct.col.print._

trait TOpenCLVectorImpl[G] extends TOpenCLVectorOps[G] {
  this: TOpenCLVector[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("openCLVector") <> "<" <> innerType <> "," <> size.toString <> ">"
}
