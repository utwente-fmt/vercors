package vct.col.ast.lang.java

import vct.col.ast.{JavaClass, JavaClassOrInterface, JavaStringValue, JavaTClass, Type}
import vct.col.print.{Ctx, Doc, Text}
import vct.result.VerificationError.Unreachable
import vct.col.ast.ops.JavaStringValueOps

trait JavaStringValueImpl[G] extends JavaStringValueOps[G] { this: JavaStringValue[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("\"") <> data <> Text("\"")
}
