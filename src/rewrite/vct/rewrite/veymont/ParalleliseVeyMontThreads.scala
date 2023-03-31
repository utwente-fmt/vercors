package vct.rewrite.veymont

import hre.util.ScopedStack
import vct.col.ast.{BooleanValue, Class, ClassDeclaration, Declaration, InstanceField, InstanceMethod, Node, Program, VeyMontSeqProg, VeyMontThread}
import vct.col.origin.{Origin, PreferredNameOrigin}
import vct.col.rewrite.veymont.StructureCheck.VeyMontStructCheckError
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.typerules.CoercionUtils.o
import vct.result.VerificationError.UserError
import vct.rewrite.veymont.ParalleliseVeyMontThreads.{ParalliseVeyMontThreadsError, ThreadClassOrigin}

object ParalleliseVeyMontThreads extends RewriterBuilder {
  override def key: String = "ParalleliseVeyMontThreads"

  override def desc: String = "Generate classes for VeyMont threads in parallel program"

  case class ParalliseVeyMontThreadsError(node : Node[_], msg: String) extends UserError {
    override def code: String = "ParalleliseVeyMontThreadsError"

    override def text: String = node.o.messageInContext(msg)
  }

  case class ThreadClassOrigin(thread: VeyMontThread[_]) extends Origin {
    override def preferredName: String = thread.o.preferredName.toUpperCase() + "Thread"

    override def context: String = thread.o.context

    override def inlineContext: String = thread.o.inlineContext

    override def shortPosition: String = thread.o.shortPosition
  }
}

case class ParalleliseVeyMontThreads[Pre <: Generation]() extends Rewriter[Pre] {

  val inSeqProg: ScopedStack[VeyMontThread[Pre]] = ScopedStack()


  override def dispatch(decl : Declaration[Pre]) : Unit = {
    decl match {
      case seqProg: VeyMontSeqProg[Pre] =>
        val threadClasses = generateThreadClasses(seqProg)
        for(tc <- threadClasses) {
          globalDeclarations.declare(tc)
        }
      case other => rewriteDefault(other)
    }
  }


  private def generateThreadClasses(seqProg: VeyMontSeqProg[Pre]) : Seq[Class[Post]] = {
    val threadClasses = seqProg.threads.map { t =>
      inSeqProg.push(t)
      try {
        classDeclarations.scope {
          val threadField = new InstanceField[Post](dispatch(t.threadType), Set.empty)(t.o)
          val methods: Seq[ClassDeclaration[Post]] = seqProg.methods.map {
            case m: InstanceMethod[Pre] => getThreadMethod(t, m)
            case _ => throw ParalliseVeyMontThreadsError(seqProg, "Methods of seq_program need to be of type InstanceMethod")
          }
          new Class[Post](
            threadField +: methods,
            Seq(),
            BooleanValue(true)(t.o))(ThreadClassOrigin(t))
        }
      } finally {
        inSeqProg.pop()
      }
    }
    threadClasses
  }


  private def getThreadMethod(thread: VeyMontThread[Pre], method : InstanceMethod[Pre]): InstanceMethod[Post] = {
    new InstanceMethod[Post](
        dispatch(method.returnType),
        variables.dispatch(method.args),
        variables.dispatch(method.outArgs),
        variables.dispatch(method.typeArgs),
        method.body.map(dispatch),
        dispatch(method.contract))(method.blame)(method.o)
  }


}
