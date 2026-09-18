package vct.col.ast.expr.heap.alloc

import vct.col.ast.{NewObjectUnique, TClassUnique, TClass, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}
import vct.col.ast.ops.NewObjectUniqueOps

trait NewObjectUniqueImpl[G] extends NewObjectUniqueOps[G] {
  this: NewObjectUnique[G] =>
  override def t: Type[G] = TClassUnique[G](cls.decl.classType(Seq()), uniqueMap)

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Text("new") <+> t <> "()"
    }
