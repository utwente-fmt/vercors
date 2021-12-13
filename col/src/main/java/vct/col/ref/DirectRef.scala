package vct.col.ref

import vct.col.ast.Declaration

import scala.reflect.ClassTag

class DirectRef[G, Decl <: Declaration[G]](genericDecl: Declaration[G])(implicit tag: ClassTag[Decl]) extends Ref[G, Decl] {
  override def decl: Decl = genericDecl match {
    case decl: /*tagged*/ Decl => decl
    case other => throw MistypedRef(other, tag)
  }
}
