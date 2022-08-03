package vct.col.newrewrite

import vct.col.ast._
import vct.col.util.AstBuildHelpers._
import RewriteHelpers._
import vct.col.ast.temporaryimplpackage.statement.composite.LoopImpl.IterationContractData
import vct.col.origin.{DiagnosticOrigin, Origin}
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.result.VerificationError.UserError

case object IterationContractToParBlock extends RewriterBuilder {
  override def key: String = "iterationContract"
  override def desc: String = "Translate loops with an iteration-style contract to a parallel block."

  case object IterationContractOrigin extends Origin {
    override def preferredName: String = ???
    override def shortPosition: String = "generated"
    override def context: String = ???
    override def inlineContext: String = ???
  }
}

case class IterationContractToParBlock[Pre <: Generation]() extends Rewriter[Pre] {
  import IterationContractToParBlock._

  override def dispatch(stat: Statement[Pre]): Statement[Post] = stat match {
    case loop @ Loop(init, cond, update, contract @ IterationContract(requires, ensures, context_everywhere), body) =>
      val IterationContractData(v, low, high) = loop.getIterationContractData(IterationContractOrigin).fold(throw _, identity)

      val newV = variables.dispatch(v)

      implicit val o: Origin = loop.o
      ParStatement(
        ParBlock(
          decl = new ParBlockDecl(),
          iters = Seq(IterVariable(newV, dispatch(low), dispatch(high))),
          requires = dispatch(requires),
          ensures = dispatch(ensures),
          context_everywhere = dispatch(context_everywhere),
          content = dispatch(body),
        )(contract.blame)
      )
    case other => rewriteDefault(other)
  }
}
