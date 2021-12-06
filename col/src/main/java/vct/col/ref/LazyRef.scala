package vct.col.ref

import vct.col.ast.Declaration
import vct.col.ref

import scala.reflect.ClassTag

class LazyRef[T <: Declaration](lazyDecl: => Declaration)(implicit tag: ClassTag[T]) extends Ref[T] {
  def decl: T = lazyDecl match {
    case decl: /*tagged*/ T => decl
    case other => throw ref.MistypedRef(other, tag)
  }
}
