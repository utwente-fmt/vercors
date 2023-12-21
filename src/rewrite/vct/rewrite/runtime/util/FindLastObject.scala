package vct.rewrite.runtime.util
import hre.util.ScopedStack
import vct.col.ast._
import hre.util.ScopedStack

object FindLastObject {
  val previousDeref: ScopedStack[Deref[_]] = new ScopedStack()

  def findObjectReference[G](d: Deref[G]): Unit = {
    d.obj match {
      case _: ThisObject[G] =>
      case local: Local[G] =>
      case deref: Deref[G] => previousDeref.having(deref){findObjectReference(deref)}
      case _ => ???
    }
  }

}
