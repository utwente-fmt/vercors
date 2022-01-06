package vct.col.veymont

import vct.col.ast.stmt.decl.{Method, ProgramUnit}
import vct.col.ast.util.AbstractRewriter

class GlobalProgPems (override val source: ProgramUnit) extends AbstractRewriter(null, true) {

  override def visit(m : Method) : Unit = {
    Warning("GlobalProgPerms not implemented!")
    super.visit(m)
  }

}
