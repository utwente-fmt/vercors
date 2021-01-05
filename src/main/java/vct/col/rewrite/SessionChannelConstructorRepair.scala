package vct.col.rewrite

import vct.col.ast.stmt.composite.BlockStatement
import vct.col.ast.stmt.decl.{Method, ProgramUnit}
import vct.col.ast.util.AbstractRewriter
import vct.col.util.SessionUtil.channelClassName

class SessionChannelConstructorRepair(override val source : ProgramUnit)  extends AbstractRewriter(null, true) {

  override def visit(m : Method) = {
    if(m.name == channelClassName) {
      val repairedBody = m.getBody match {
        case b : BlockStatement => create.block(rewrite(b.getStatements).dropRight(2):_*)
        case o => Fail("Session Fail: The constructor of the Channel class must have a BlockStatement as body");o
      }
      result = create.method_kind(m.kind,m.getReturnType,m.getContract,m.name,m.getArgs,repairedBody)
    } else {
      super.visit(m)
    }
  }
}
