package vct.col.ref

import vct.col.ast.Declaration

import scala.reflect.ClassTag

class DirectRef[T <: Declaration](genericDecl: Declaration)(implicit tag: ClassTag[T]) extends Ref[T] {
  override def decl: T = genericDecl match {
    case decl: /*tagged*/ T => decl
    case other => throw MistypedRef(other, tag)
  }
}
