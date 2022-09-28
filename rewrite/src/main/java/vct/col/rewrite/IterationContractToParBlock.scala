package vct.col.rewrite

import vct.col.ast._
import vct.col.util.AstBuildHelpers._
import RewriteHelpers._
import hre.util.ScopedStack
import vct.col.ast.statement.composite.LoopImpl.IterationContractData
import vct.col.origin.{DiagnosticOrigin, Origin}
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
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

  case class VariableReadOutsideParLoop(e: Local[_]) extends UserError {
    override def code: String = "localOutsideParLoop"
    override def text: String = e.o.messageInContext("This variable may not be read here, since it is used as a loop variable in a loop specified with an iteration contract.")
  }

  case class VariableWrittenInParLoop(assign: Assign[_]) extends UserError {
    override def code: String = "writeParVar"
    override def text: String = assign.o.messageInContext("This variable may not be written to, since it is used as a loop variable in a loop specified with an iteration contract.")
  }
}

case class IterationContractToParBlock[Pre <: Generation]() extends Rewriter[Pre] {
  import IterationContractToParBlock._

  var iterationLoopVariables: Set[Variable[Pre]] = Set.empty
  val currentIterationVariables: ScopedStack[Variable[Pre]] = ScopedStack()

  override def dispatch(program: Program[Pre]): Program[Post] = {
    iterationLoopVariables = program.transSubnodes.collect {
      case loop @ Loop(_, _, _, contract @ IterationContract(_, _, _), _) =>
        loop.getIterationContractData(IterationContractOrigin).fold(throw _, identity).v
    }.toSet

    program.rewrite()
  }

  override def dispatch(stat: Statement[Pre]): Statement[Post] = stat match {
    case loop @ Loop(init, cond, update, contract @ IterationContract(requires, ensures, context_everywhere), body) =>
      val IterationContractData(v, low, high) = loop.getIterationContractData(IterationContractOrigin).fold(throw _, identity)

      val newV = variables.dispatch(v)

      implicit val o: Origin = loop.o
      currentIterationVariables.having(v) {
        sendDecls.scope {
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
        }
      }

    case a @ Assign(Local(Ref(v)), _) if iterationLoopVariables.contains(v) =>
      throw VariableWrittenInParLoop(a)

    case s @ Scope(locals, _) =>
      s.rewrite(locals = variables.dispatch(locals.filterNot(iterationLoopVariables.contains)))

    case other => rewriteDefault(other)
  }

  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = e match {
    case l @ Local(Ref(v)) if iterationLoopVariables.contains(v) && !currentIterationVariables.toSeq.contains(v) =>
      throw VariableReadOutsideParLoop(l)
    case other => rewriteDefault(other)
  }
}
