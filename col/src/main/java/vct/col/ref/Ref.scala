package vct.col.ref

import vct.col.ast.Declaration

import scala.runtime.ScalaRunTime

/** NB: While Ref's can be stricter than just any Declaration (e.g. Ref[Function]), we can construct any variant with
  * just a Declaration. This is because:
  * - We cannot prove that the successor of a Declaration is of the correct type when we construct it: then it wouldn't
  * be lazy and we wouldn't be able to cross-reference declarations out of AST order.
  * - We do not want to cast or check refs to be of the correct kind every time we want to use it.
  * The most acceptable solution is then to pretend to have a safe interface that returns a declaration of the right
  * kind, but quietly check the type on first access.
  */
trait Ref[T <: Declaration] {
  def decl: T

  def tryResolve(resolver: String => Declaration): Unit = {}

  override def equals(obj: scala.Any): Boolean = obj match {
    case other: Ref[_] => decl == other.decl
  }

  override def hashCode(): Int = ScalaRunTime._hashCode((Ref.getClass, decl))
}

object Ref {
  val EXC_MESSAGE = "The AST is in an invalid state: a Ref contains a declaration of the wrong kind."

  def unapply[T <: Declaration](obj: Ref[T]): Some[T] = Some(obj.decl)
}