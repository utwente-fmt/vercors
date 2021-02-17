package vct.col.rewrite

import hre.ast.MessageOrigin
import vct.col.ast.stmt.decl.{ASTClass, ProgramUnit}
import vct.col.ast.util.AbstractRewriter
import vct.col.util.SessionUtil.{barrierFieldName, getChanClass, isThreadClassName}
import scala.collection.JavaConversions._

class SessionBarrier(override val source: ProgramUnit) extends AbstractRewriter(null, true) {

  def addBarrierToConstructors() = {
    for(entry <- source.get()) {
      entry match {
        case c : ASTClass => {
          if(isThreadClassName(c.name)) {
            c.add_dynamic(create.field_name(barrierFieldName))
          }
        }
      }
    }
  }
}
