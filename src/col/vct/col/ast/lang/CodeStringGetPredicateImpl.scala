package vct.col.ast.lang

import vct.col.ast._
import vct.col.print.{Ctx, Doc, Group, Text}

trait CodeStringGetPredicateImpl[G] {
  this: CodeStringGetPredicate[G] =>


  def getClassType: Text = Text(cls.decl.o.getPreferredNameOrElse())

  def getClassTypeLower: Text = Text(cls.decl.o.getPreferredNameOrElse().toLowerCase)

  override def layout(implicit ctx: Ctx): Doc =
    Group(
      Group(getClassType <+> Text("tmp = new") <+> cls.decl.o.getPreferredNameOrElse() <> "(" <+> Doc.args(args) <+> ");") <+/>
        Group(
          Group(Text("for(") <> getClassType <+> getClassTypeLower <+> ": predicateStore.get(Thread.currentThread().threadId()))") <+>
            Text("{") <>> Group(Text("if(tmp.equals(") <> getClassTypeLower <> ")) return" <+> getClassTypeLower <> ";") <+/> "}" <+/>
            Text("return null;")
        )
    )

}