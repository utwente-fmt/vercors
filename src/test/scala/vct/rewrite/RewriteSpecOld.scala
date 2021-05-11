package vct.rewrite

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.{MatchResult, Matcher}
import vct.col.ast.generic.ASTNode
import vct.col.ast.util.{ASTFactory, ASTFrame, ASTVisitor, AbstractRewriter}

case class RewriteSpecOld(rewriter: AbstractRewriter, before: ASTVisitor[_<:ASTNode]*) extends AnyFlatSpec with Matchers {
  protected var create = new ASTFactory

  def rewrite(node: ASTNode): ASTNode = {
    var input = node

    for(pass <- before) {
      input.accept(pass)
      if(pass.isInstanceOf[AbstractRewriter]) {
        input = pass.asInstanceOf[ASTFrame[ASTNode]].getResult
      }
    }

    rewriter.rewrite(input)
  }

  def rewriteTo(node: ASTNode) = RewriteMatcher(node)

  case class RewriteMatcher(right: ASTNode) extends Matcher[ASTNode] {
    override def apply(left: ASTNode): MatchResult = {
      val rewritten = rewrite(left)
      MatchResult(rewritten.equals(right), s"$left did not rewrite to $right, but instead to $rewritten", s"$left was rewritten to $right")
    }
  }
}
