package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print.{Ctx, Doc, Group, Text}
import vct.col.ast.ops.CodeStringGetPredicateOps

trait CodeStringGetPredicateImpl[G] extends CodeStringGetPredicateOps[G] {
  this: CodeStringGetPredicate[G] =>

  def getClassTypeLower(implicit ctx: Ctx): Text = Text(ctx.name(cls).toLowerCase)

  override def layout(implicit ctx: Ctx): Doc =
    Group(
      Group(Text(ctx.name(cls)) <+> Text("tmp = new") <+> ctx.name(cls) <> "(" <+> Doc.args(args) <+> ");") <+/>
        Group(
          Group(Text("for(") <> ctx.name(cls) <+> getClassTypeLower <+> ": predicateStore.get(Thread.currentThread().threadId()))") <+>
            Text("{") <>> Group(Text("if(tmp.equals(") <> getClassTypeLower <> ")) return" <+> getClassTypeLower <> ";") <+/> "}" <+/>
            Text("return null;")
        )
    )
}