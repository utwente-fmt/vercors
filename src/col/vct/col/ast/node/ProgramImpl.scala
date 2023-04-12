package vct.col.ast.node

import vct.col.ast.Program
import vct.col.ast.util.Declarator
import vct.col.check.{CheckContext, CheckError}
import vct.col.print.{Ctx, Doc}

import scala.collection.parallel.CollectionConverters._

trait ProgramImpl[G] extends Declarator[G] { this: Program[G] =>
  def check: Seq[CheckError] = checkTrans(CheckContext())

  override def layout(implicit ctx: Ctx): Doc =
    Doc.stack(declarations)
}