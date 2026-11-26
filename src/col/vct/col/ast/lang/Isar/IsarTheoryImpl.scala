package vct.col.ast.lang.Isar

import vct.col.ast.IsarTheory
import vct.col.ast.lang.Isar.IsarDoc.{begin, end}
import vct.col.ast.ops.IsarTheoryOps
import vct.col.check.CheckContext
import vct.col.print._
trait IsarTheoryImpl[G] extends IsarTheoryOps[G] {
  this: IsarTheory[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("theory") <+> Text(ctx.theoryName) </> Text("imports") <+>
      Text(this.imports.reduce[String] { case (l, r) => s"$l $r" }) </>
      begin </> Doc.stack2(commands) </> end
}
