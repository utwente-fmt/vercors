package vct.col.rewrite.gpgpuoptimizations

import vct.col.ast.stmt.decl.ProgramUnit
import vct.col.ast.util.AbstractRewriter

case class TileKernel(override val source: ProgramUnit) extends AbstractRewriter(source) {

}
