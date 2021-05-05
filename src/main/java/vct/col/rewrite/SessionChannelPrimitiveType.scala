package vct.col.rewrite

import vct.col.ast.`type`.{ClassType, PrimitiveSort, PrimitiveType}
import vct.col.ast.stmt.decl.{Method, ProgramUnit}
import vct.col.ast.util.AbstractRewriter
import vct.col.util.SessionUtil.channelClassName

class SessionChannelPrimitiveType(override val source: ProgramUnit, val sort : Either[PrimitiveSort,ClassType]) extends AbstractRewriter(null, true) {

  override def visit(t : PrimitiveType) : Unit = {
    sort match {
      case Left(s) => result = create.primitive_type(s)
      case Right(c) => result = create.class_type(c.getName)
    }
  }

  override def visit(m : Method) : Unit = {
    if(m.kind == Method.Kind.Constructor)
      result = create.method_kind(m.kind, m.getReturnType, rewrite(m.getContract), sort.toString + channelClassName,
        m.getArgs, rewrite(m.getBody))
    else
      super.visit(m)
  }
}
