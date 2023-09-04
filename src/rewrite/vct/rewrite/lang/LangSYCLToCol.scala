package vct.rewrite.lang

import com.typesafe.scalalogging.LazyLogging
import vct.col.ast.{AbstractRewriter, TRef, Type}
import vct.col.rewrite.lang.LangSpecificToCol
import vct.col.rewrite.{Generation, Rewritten}

case class LangSYCLToCol[Pre <: Generation](rw: LangSpecificToCol[Pre]) extends LazyLogging {
  type Post = Rewritten[Pre]
  implicit val implicitRewriter: AbstractRewriter[Pre, Post] = rw

  def rewriteType(typ: Type[Pre]): TRef[Post] = TRef()

}
