package vct.col.ref

import vct.col.ast.Declaration
import vct.col.ref

import scala.reflect.ClassTag

class UnresolvedRef[G, Decl <: Declaration[G]](val name: String)(implicit tag: ClassTag[Decl]) extends Ref[G, Decl] {
  private var resolvedDecl: Option[Declaration[G]] = None

  override def tryResolve(resolver: String => Declaration[G]): Unit = resolve(resolver(name))

  def resolve(decl: Declaration[G]): Unit = resolvedDecl = Some(decl)

  def decl: Decl = resolvedDecl match {
    case None => throw NotResolved(this, tag)
    case Some(decl: /*tagged*/ Decl) => decl
    case Some(other) => throw ref.MistypedRef(other, tag)
  }
}
