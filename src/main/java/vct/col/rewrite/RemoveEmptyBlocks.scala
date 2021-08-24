package vct.col.rewrite

import vct.col.ast.stmt.composite.{LoopStatement, ParallelBlock, ParallelRegion}
import vct.col.ast.stmt.decl.{Contract, ProgramUnit}
import vct.col.ast.util.AbstractRewriter

class RemoveEmptyBlocks(override val source : ProgramUnit)  extends AbstractRewriter(null, true){

  override def visit(pr : ParallelRegion) = {
    val nonEmptyBlocks : List[ParallelBlock] = pr.blocks.filter(_.block.getStatements.nonEmpty);
    if(nonEmptyBlocks.nonEmpty) {
        result = create.region(pr.getOrigin,pr.fuse, pr.contract,nonEmptyBlocks:_*)
    }
  }

}
