package vct.col.ast.lang.smt

import vct.col.ast.ProverLanguage
import vct.col.ast.ops.ProverLanguageFamilyOps
import vct.col.print._

trait ProverLanguageImpl[G] extends ProverLanguageFamilyOps[G] {
  this: ProverLanguage[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
