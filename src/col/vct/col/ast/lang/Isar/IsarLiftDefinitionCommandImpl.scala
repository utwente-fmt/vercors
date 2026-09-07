package vct.col.ast.lang.Isar

import vct.col.ast.{IsarDefinitionCommand, IsarLiftDefinitionCommand}
import vct.col.ast.lang.Isar.IsarDoc._
import vct.col.ast.ops.IsarLiftDefinitionCommandOps
import vct.col.check.CheckContext
import vct.col.print._

trait IsarLiftDefinitionCommandImpl[G] extends IsarLiftDefinitionCommandOps[G] {
  this: IsarLiftDefinitionCommand[G] =>
  override def enterCheckContextScopes(
      context: CheckContext[G]
  ): Seq[CheckContext.ScopeFrame[G]] =
    context.withScope(this.typevars, toScan = Nil)
  override def layout(implicit ctx: Ctx): Doc =
    Text("lift_definition") <+> Text(this.name) <+>
      Text("::") <+> // TODO why does <::> not resolve?
      IsarDoc.inner { type_signature(this.signature) } <+> Text("is") <+>
      Text(this.inner.decl.asInstanceOf[IsarDefinitionCommand[G]].name) <+>
      Text(".")
}
