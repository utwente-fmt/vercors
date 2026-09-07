package vct.col.ast.lang.Isar

import vct.col.ast.{
  IsarDataConstructor,
  IsarDefinitionCommand,
  IsarPartialConstructorCommand,
  IsarTypedefCommand,
  TIsarType,
}
import vct.col.ast.lang.Isar.IsarDoc._
import vct.col.ast.ops.IsarPartialConstructorCommandOps
import vct.col.check.CheckContext
import vct.col.print._

trait IsarPartialConstructorCommandImpl[G]
    extends IsarPartialConstructorCommandOps[G] {
  this: IsarPartialConstructorCommand[G] =>
  override def enterCheckContextScopes(
      context: CheckContext[G]
  ): Seq[CheckContext.ScopeFrame[G]] =
    context.withScope(this.typevars ++ this.args, toScan = Nil)

  def layout_constructor(implicit ctx: Ctx): Doc = {
    Text("(") <>
      this.constructor.decl.asInstanceOf[IsarDataConstructor[G]].name <+>
      Doc.spread(this.args) <> Text(")")
  }
  override def layout(implicit ctx: Ctx): Doc = {
    Text("definition") <+> this.name <+> <::> <+> inner {
      type_signature(this.args.map(_.t) :+ this.returnType)
    } </> where <+/> inner {
      Text(this.name) <+> Doc.spread(this.args) <+> Text("≡") <+>
        Text("(if (") <+>
        this.guard.decl.asInstanceOf[IsarDefinitionCommand[G]].name <+>
        layout_constructor <+> Text(") then") <+> Text(
          "(Abs_" +
            this.returnType.asInstanceOf[TIsarType[G]].adt.decl
              .asInstanceOf[IsarTypedefCommand[G]].typename
        ) <+> layout_constructor <> Text(") else undefined)")
    }
  }
}
