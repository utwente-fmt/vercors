package vct.col.ast.lang.Isar

import vct.col.ast.IsarLocaleCommand
import vct.col.ast.lang.Isar.IsarDoc._
import vct.col.ast.ops.IsarLocaleCommandOps
import vct.col.check.CheckContext
import vct.col.print._

trait IsarLocaleCommandImpl[G] extends IsarLocaleCommandOps[G] {
  this: IsarLocaleCommand[G] =>
  override def enterCheckContextScopes(
      context: CheckContext[G]
  ): Seq[CheckContext.ScopeFrame[G]] =
    context.withScope(this.typevars, toScan = Nil)

  def layout_extensions(implicit ctx: Ctx): Doc = {
    Group(
      Doc.foldr(
        this.extensions.map(_.decl.asInstanceOf[IsarLocaleCommand[G]].name)
          .map(Text)
      )(_ <+> "+" <+> _)
    )
  }

  def layout_fixes(implicit ctx: Ctx): Doc = { Doc.stack(this.fixes) }

  def layout_assumes(implicit ctx: Ctx): Doc = {
    Doc.stack(this.assumes.map { a =>
      // TODO make a an ADT axiom
      Text("assumes") <+> inner { a.show }
    })
  }

  def layout_context(implicit ctx: Ctx): Doc = {
    if (
      !this.fixes.isEmpty || !this.assumes.isEmpty || !this.extensions.isEmpty
    ) { Text("=") <+> layout_extensions </> layout_fixes </> layout_assumes }
    else { Empty }
  }

  override def layout(implicit ctx: Ctx): Doc = {
    // locale name_signature = extension + ...
    //  fixes function_name :: function_signature ...
    //  assume axiom_name : axioms_signature ...
    // begin
    // end
    Text("locale") <+> Text(this.name + "_signature") <+> layout_context </>
      begin </> end
  }
}
