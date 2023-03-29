package vct.col.ast.expr.context

import vct.col.ast.{TAnyClass, TSeqProg, ThisSeqProg, Type}

trait ThisSeqProgImpl[G] { this: ThisSeqProg[G] =>
  override def t: Type[G] = TSeqProg(cls)
}