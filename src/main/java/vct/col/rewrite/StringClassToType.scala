package vct.col.rewrite

import vct.col.ast.`type`.{ClassType, PrimitiveSort}
import vct.col.ast.stmt.decl.ProgramUnit
import vct.col.ast.util.AbstractRewriter

case class StringClassToType(override val source: ProgramUnit) extends AbstractRewriter(source) {
  override def visit(t: ClassType): Unit = {
    if (t.names == Seq("String")) {
      result = create primitive_type PrimitiveSort.String
    } else {
      super.visit(t)
    }
  }
}
