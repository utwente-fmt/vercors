package vct.col.ast.temporaryimplpackage.node

import vct.col.ast.Program
import vct.col.ast.temporaryimplpackage.util.Declarator
import vct.col.check.{CheckContext, CheckError}
import scala.collection.parallel.CollectionConverters._

trait ProgramImpl[G] extends Declarator[G] { this: Program[G] =>
  def check: Seq[CheckError] = {
    val context = enterCheckContext(CheckContext())
    declarations.flatMap(_.checkTrans(context)).toIndexedSeq
  }

  override def check(context: CheckContext[G]): Seq[CheckError] = Nil
}