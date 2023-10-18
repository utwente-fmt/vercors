package vct.col.ast.family.pvlcommunicate

import vct.col.ast.{PVLThreadName, TClass, Type}
import vct.col.resolve.ctx.RefVeyMontThread

trait PVLThreadNameImpl[G] { this: PVLThreadName[G] =>
  override def threadType: TClass[G] = ref.get match {
    case RefVeyMontThread(decl) => decl.threadType match {
      case t @ TClass(_) => t
      case _ => ???
    }
    case _ => ???
  }
}
