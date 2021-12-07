package vct.col.ast.temporaryimplpackage.node

import vct.col.ast.Program
import vct.col.ast.temporaryimplpackage.util.Declarator
import vct.col.check.{CheckContext, CheckError}
import scala.collection.parallel.CollectionConverters._

trait ProgramImpl extends Declarator { this: Program =>
  def check: Seq[CheckError] =
    declarations.par.flatMap(_.check(enterCheckContext(CheckContext()))).toIndexedSeq

  override def check(context: CheckContext): Seq[CheckError] = Nil
}