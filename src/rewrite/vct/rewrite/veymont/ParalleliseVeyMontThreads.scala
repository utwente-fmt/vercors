package vct.rewrite.veymont

import hre.util.ScopedStack
import vct.col.ast.{Assert, Assign, Block, BooleanValue, Branch, Class, ClassDeclaration, Declaration, Eval, InstanceField, InstanceMethod, Loop, Node, Program, RunMethod, Scope, Statement, Type, VeyMontAssignExpression, VeyMontCommExpression, VeyMontSeqProg, VeyMontThread}
import vct.col.origin.{Origin, PreferredNameOrigin}
import vct.col.ref.Ref
import vct.col.rewrite.veymont.StructureCheck.VeyMontStructCheckError
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.typerules.CoercionUtils.o
import vct.result.VerificationError.UserError
import vct.rewrite.veymont.ParalleliseVeyMontThreads.{ParalliseVeyMontThreadsError, ThreadClassOrigin}

import scala.collection.immutable.Map

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
  val inSeqThreadMap : ScopedStack[Map[VeyMontThread[Pre],InstanceField[Post]]] = ScopedStack()

  override def dispatch(decl : Declaration[Pre]) : Unit = {
    decl match {
      case seqProg: VeyMontSeqProg[Pre] =>
        val threadFieldMap = generateThreadFields(seqProg)
        inSeqThreadMap.having(threadFieldMap) {
          val threadClasses = generateThreadClasses(seqProg)
          for (tc <- threadClasses) {
            globalDeclarations.declare(tc)
          }
        }
      case t: VeyMontThread[Pre] => //sucessor is threadField in threadClass constructed above
      case other => rewriteDefault(other)
    }
  }

  private def generateThreadFields(seqProg: VeyMontSeqProg[Pre]) : Map[VeyMontThread[Pre],InstanceField[Post]] =
    seqProg.threads.map { thread => (thread -> new InstanceField[Post](dispatch(thread.threadType), Set.empty)(thread.o)) }.toMap

  private def generateThreadClasses(seqProg: VeyMontSeqProg[Pre]) : Seq[Class[Post]] = {
    val threadClasses = seqProg.threads.map { thread =>
      inSeqProg.having(thread) {
        val threadFieldMap = inSeqThreadMap.top
        classDeclarations.scope {
          val channelFields = getChannelFields(collectChannelsFromRun(seqProg) ++ collectChannelsFromMethods(seqProg))
          val methods: Seq[ClassDeclaration[Post]] = seqProg.methods.map {
            case m: InstanceMethod[Pre] => getThreadMethod(m)
            case _ => throw ParalliseVeyMontThreadsError(seqProg, "Methods of seq_program need to be of type InstanceMethod")
          }
          new Class[Post](
            (threadFieldMap(thread) +: channelFields) ++ methods,
            Seq(),
            BooleanValue(true)(thread.o))(ThreadClassOrigin(thread))
        }
      }
    }
    threadClasses
  }


  private def getThreadMethod(method : InstanceMethod[Pre]): InstanceMethod[Post] = {
    new InstanceMethod[Post](
        dispatch(method.returnType),
        variables.dispatch(method.args),
        variables.dispatch(method.outArgs),
        variables.dispatch(method.typeArgs),
        method.body.map(dispatch),
        dispatch(method.contract))(method.blame)(method.o)
  }

  private def getChannelFields(chanDescr : Seq[(Type[Pre], String,Statement[Pre])]): Seq[InstanceField[Post]] = Seq.empty
  //InstanceField[Post](dispatch(ChannelClassOfRightType), Set.empty)(ChannelOrigin)

  private def collectChannelsFromRun(seqProg : VeyMontSeqProg[Pre]) =
    seqProg.runMethod match {
      case r: RunMethod[Pre] => getChannelsFromBody(r.body, r)
      case other => throw ParalliseVeyMontThreadsError(other, "seq_program run method expected")
    }

  private def collectChannelsFromMethods(seqProg: VeyMontSeqProg[Pre]) =
    seqProg.methods.flatMap {
    case m: InstanceMethod[Pre] => getChannelsFromBody(m.body, m)
    case other => throw ParalliseVeyMontThreadsError(other, "seq_program method expected")
  }

  private def getChannelsFromBody(body: Option[Statement[Pre]], method: ClassDeclaration[Pre]) = {
    body match {
      case None => throw ParalliseVeyMontThreadsError(method, "Method in seq_program needs to have non-empty body")
      case Some(b) => getChannelNamesAndTypes(b)
    }
  }

  private def getChannelNamesAndTypes(s : Statement[Pre]): Seq[(Type[Pre], String,Statement[Pre])] = {
    s.collect{case e@VeyMontCommExpression(recv,sender,assign) =>
      (recv.decl.threadType,recv.decl.o.preferredName + sender.decl.o.preferredName + "Channel",e)}
  }
/*
  override def dispatch(st : Statement[Pre]) : Statement[Post] = {
    if (inSeqProg.nonEmpty) {
      val thread = inSeqProg.top
      val threadFieldMap = inSeqThreadMap.top
      st match {
        case VeyMontCommExpression(recv,sender,assign) => dispatch(assign)
        case VeyMontAssignExpression(_, _) => rewriteDefault(st)
        case Assign(_, _) => rewriteDefault(st)
        case Branch(_) => rewriteDefault(st)
        case Loop(_, _, _, _, _) => rewriteDefault(st)
        case Scope(_, _) => rewriteDefault(st)
        case Block(_) => rewriteDefault(st)
        case Eval(expr) => dispatch(expr)
        case Assert(_) => rewriteDefault(st)
        case _ => throw VeyMontStructCheckError(st, "Statement not allowed in seq_program")
      }
    } else rewriteDefault(st)
  }
*/
}
