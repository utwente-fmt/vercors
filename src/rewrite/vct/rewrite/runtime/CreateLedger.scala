package vct.rewrite.runtime

import vct.col.ast.RewriteHelpers.RewriteClass
import vct.col.ast.{Class, Declaration, GlobalDeclaration, Procedure, TClass}
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}

object CreateLedger extends RewriterBuilder {
  override def key: String = "removeSelfLoops"

  override def desc: String = "Removing unnecessary self loop by removing Object class and corresponding procedure"
}


case class CreateLedger[Pre <: Generation]() extends Rewriter[Pre] {


}