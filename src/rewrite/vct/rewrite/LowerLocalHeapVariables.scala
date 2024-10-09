package vct.rewrite

import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.ast.{Variable, _}
import vct.col.origin.{AssignLocalOk, LabelContext, Origin, PanicBlame}
import vct.col.util.AstBuildHelpers._
import vct.col.ref.Ref
import vct.col.util.{CurrentRewriteProgramContext, SuccessionMap}
import vct.result.VerificationError

case object LowerLocalHeapVariables extends RewriterBuilder {
  override def key: String = "lowerLocalHeapVariables"

  override def desc: String =
    "Lower LocalHeapVariables to Variables if their address is never taken"
}

case class LowerLocalHeapVariables[Pre <: Generation]() extends Rewriter[Pre] {
  private val stripped: SuccessionMap[LocalHeapVariable[Pre], Variable[Post]] =
    SuccessionMap()
  private val lowered: SuccessionMap[LocalHeapVariable[Pre], Variable[Post]] =
    SuccessionMap()

  override def dispatch(program: Program[Pre]): Program[Post] = {
    val dereferencedHeapLocals = program.collect {
      case DerefPointer(hl @ HeapLocal(_)) => System.identityHashCode(hl)
    }
    val nakedHeapLocals = program.collect {
      case hl @ HeapLocal(Ref(v))
          if !dereferencedHeapLocals.contains(System.identityHashCode(hl)) =>
        v
    }
    VerificationError.withContext(CurrentRewriteProgramContext(program)) {
      program.rewrite(declarations = {
        program.collect {
          case HeapLocal(Ref(v)) if !nakedHeapLocals.contains(v) => v
        }.foreach(v =>
          stripped(v) =
            new Variable[Post](dispatch(v.t.asPointer.get.element))(v.o)
        )
        globalDeclarations.dispatch(program.declarations)
      })
    }
  }

  override def dispatch(node: Statement[Pre]): Statement[Post] = {
    implicit val o: Origin = node.o
    node match {
      // Same logic as CollectLocalDeclarations
      case Scope(vars, impl) =>
        val (newVars, newImpl) = variables.collect {
          vars.foreach(dispatch)
          dispatch(impl)
        }
        Scope(newVars, newImpl)
      case HeapLocalDecl(v) =>
        if (stripped.contains(v)) { variables.declare(stripped(v)) }
        else {
          lowered(v) = new Variable[Post](dispatch(v.t))(v.o)
          variables.declare(lowered(v))
        }
        Block(Nil)
      case _ => node.rewriteDefault()
    }
  }

  override def dispatch(node: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = node.o
    node match {
      case DerefPointer(HeapLocal(Ref(v))) if stripped.contains(v) =>
        stripped(v).get
      case HeapLocal(Ref(v)) if lowered.contains(v) => {
        // lowered.contains(v) should always be true since all stripped HeapLocals would be caught by DerefPointer(HeapLocal(Ref(v)))
        Local(lowered.ref(v))
      }
      case _ => node.rewriteDefault()
    }
  }
}
