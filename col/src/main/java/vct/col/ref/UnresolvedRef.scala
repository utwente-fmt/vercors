package vct.col.ref

import vct.col.ast.Declaration
import vct.col.ref

import scala.reflect.ClassTag

class UnresolvedRef[T <: Declaration](val name: String)(implicit tag: ClassTag[T]) extends Ref[T] {
  private var resolvedDecl: Option[Declaration] = None

  override def tryResolve(resolver: String => Declaration): Unit = resolve(resolver(name))

  def resolve(decl: Declaration): Unit = resolvedDecl = Some(decl)

  def decl: T = resolvedDecl match {
    case None => throw NotResolved(this, tag)
    case Some(decl: /*tagged*/ T) => decl
    case Some(other) => throw ref.MistypedRef(other, tag)
  }
}
