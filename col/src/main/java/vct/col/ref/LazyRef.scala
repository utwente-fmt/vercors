package vct.col.ref

import vct.col.ast.Declaration
import vct.col.ref

import scala.reflect.ClassTag

class LazyRef[G, Decl <: Declaration[G]](lazyDecl: => Declaration[G])(implicit tag: ClassTag[Decl]) extends Ref[G, Decl] {
  def decl: Decl = lazyDecl match {
    case decl: /*tagged*/ Decl => decl
    case other => throw ref.MistypedRef(other, tag)
  }
}
