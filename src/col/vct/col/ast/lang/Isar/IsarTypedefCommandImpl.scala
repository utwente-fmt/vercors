package vct.col.ast.lang.Isar

import vct.col.ast.lang.Isar.IsarDoc.inner
import vct.col.ast.{
  IsarDatatypeCommand,
  IsarDefinitionCommand,
  IsarTypedefCommand,
  TVar,
}
import vct.col.ast.ops.IsarTypedefCommandOps
import vct.col.check.CheckContext
import vct.col.print._
import vct.col.ref.DirectRef
import vct.col.util.AstBuildHelpers._

trait IsarTypedefCommandImpl[G] extends IsarTypedefCommandOps[G] {
  this: IsarTypedefCommand[G] =>
  override def enterCheckContextScopes(
      context: CheckContext[G]
  ): Seq[CheckContext.ScopeFrame[G]] =
    context.withScope(this.typevars, toScan = Nil)

  def layout_type_variables(implicit ctx: Ctx): Doc = {
    Group(
      if (typevars.isEmpty)
        Empty
      else
        Text("(") <> Doc.args(typevars.map(v => TVar[G](new DirectRef(v)))) <>
          Text(") ")
    )
  }

  def layout_inner_type(implicit ctx: Ctx): Doc = {
    layout_type_variables <>
      Text(this.rawtype.decl.asInstanceOf[IsarDatatypeCommand[G]].typename)
  }

  override def layout(implicit ctx: Ctx): Doc =
    Text("typedef") <+> layout_type_variables <+> Text(this.typename) <+>
      Text("=") <+> inner {
        Text("{ x::") <> layout_inner_type <+> Text(".") <+>
          this.typeAxiom.decl.asInstanceOf[IsarDefinitionCommand[G]].name <+>
          Text("x") <+> Text("}")
      } </> Text("sorry") </>
      Empty </> // NOTE typedef requires a witness for the type
      Text("setup_lifting type_definition_" + this.typename)
}
