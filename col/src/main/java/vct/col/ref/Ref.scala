package vct.col.ref

import vct.col.ast.Declaration
import vct.col.rewrite.ScopeContext

import scala.reflect.ClassTag
import scala.runtime.ScalaRunTime

/** NB: While Ref's can be stricter than just any Declaration (e.g. Ref[Function]), we can construct any variant with
  * just a Declaration. This is because:
  * - We cannot prove that the successor of a Declaration is of the correct type when we construct it: then it wouldn't
  * be lazy and we wouldn't be able to cross-reference declarations out of AST order.
  * - We do not want to cast or check refs to be of the correct kind every time we want to use it.
  * The most acceptable solution is then to pretend to have a safe interface that returns a declaration of the right
  * kind, but quietly check the type on first access.
  */
trait Ref[G, Decl <: Declaration[G]] {
  def decl: Decl

  def tryResolve(resolver: String => Declaration[G]): Unit = {}

  override def equals(obj: scala.Any): Boolean = obj match {
    case other: Ref[G, _] => decl == other.decl
  }

  override def hashCode(): Int = ScalaRunTime._hashCode((Ref.getClass, decl))

  def asTransmutable[Decl2[_] <: Declaration[_]](implicit witness: Decl <:< Decl2[G]): Ref.TransmutableRef[G, Decl2] =
    new Ref.TransmutableRef(decl)
}

object Ref {
  val EXC_MESSAGE = "The AST is in an invalid state: a Ref contains a declaration of the wrong kind."

  def unapply[G, Decl <: Declaration[G]](obj: Ref[G, Decl]): Some[Decl] = Some(obj.decl)

  class TransmutableRef[G, Decl[_] <: Declaration[_]](decl: => Decl[G]) {
    def asRegular[Decl2 <: Declaration[G]](implicit witness: Decl[G] <:< Decl2, tag: ClassTag[Decl2]): Ref[G, Decl2] =
      new LazyRef(decl)

    def unsafeTransmuteGeneration[Post]: TransmutableRef[Post, Decl] = asInstanceOf
  }

  def transmute
    [Pre, Post, Decl[_] <: Declaration[_], DPre <: Declaration[Pre], DPost <: Declaration[Post]]
    (ref: Ref[Pre, DPre])
    (implicit w1: DPre <:< Decl[Pre], w2: Decl[Post] <:< DPost, tag: ClassTag[DPost])
    : Ref[Post, DPost] = ref.asTransmutable[Decl].unsafeTransmuteGeneration.asRegular
}