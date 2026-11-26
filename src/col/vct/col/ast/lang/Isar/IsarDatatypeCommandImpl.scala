package vct.col.ast.lang.Isar

import vct.col.ast.lang.Isar.IsarDoc._
import vct.col.ast.{IsarDatatypeCommand, TVar}
import vct.col.ast.ops.IsarDatatypeCommandOps
import vct.col.check.CheckContext
import vct.col.print.{Group, _}
import vct.col.ref.DirectRef
import vct.col.util.AstBuildHelpers._

trait IsarDatatypeCommandImpl[G] extends IsarDatatypeCommandOps[G] {
  this: IsarDatatypeCommand[G] =>
  override def enterCheckContextScopes(
      context: CheckContext[G]
  ): Seq[CheckContext.ScopeFrame[G]] =
    context.withScope(this.typevars, toScan = Nil)

  def layout_typedecl(implicit ctx: Ctx): Doc =
    Text("typedecl") <+> Group(
      if (typevars.isEmpty)
        Empty
      else
        Text("(") <> Doc.args(typevars.map(v => TVar[G](new DirectRef(v)))) <>
          Text(")")
    ) <+> Text(typename)

  def layout_datatype(implicit ctx: Ctx): Doc =
    Text("datatype") <+> Group(
      if (typevars.isEmpty)
        Empty
      else
        Text("(") <> Doc.args(typevars.map(v => TVar[G](new DirectRef(v)))) <>
          Text(")")
    ) <+> Text(typename) <+> Text("=") <+>
      IsarDoc.alternative(this.constructors)

  override def layout(implicit ctx: Ctx): Doc = {
    // datatype command requires constructors which some ADT will not have
    // therefore we layout it as type declaration instead
    if (this.constructors.isEmpty) { layout_typedecl }
    else { layout_datatype }
  }
}
