package vct.col.ast.lang.Isar

import vct.col.ast.lang.Isar.IsarDoc._
import vct.col.ast.{IsarDefinitionCommand, TVar}
import vct.col.ast.ops.IsarDefinitionCommandOps
import vct.col.check.CheckContext
import vct.col.print._

trait IsarDefinitionCommandImpl[G] extends IsarDefinitionCommandOps[G] {
  this: IsarDefinitionCommand[G] =>
  override def enterCheckContextScopes(
      context: CheckContext[G]
  ): Seq[CheckContext.ScopeFrame[G]] =
    context.withScope(this.typevars ++ this.args, toScan = Nil)
  override def layout(implicit ctx: Ctx): Doc =
    Text("definition") <+> this.name <+> <::> <+> inner {
      type_signature(this.args.map(_.t) :+ this.returnType)
    } </> where <+/> inner {
      // TODO why is _ not printed?
      val ignored: Iterable[Show] = this.args.map(_ => ignore)
      Text(this.name) <+> Doc.spread(this.args) <+> Text("≡") <+>
        (if (this.body.isEmpty)
           Text("undefined")
         else { this.body.get.show })
    }
}
