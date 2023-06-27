package vct.rewrite.veymont

import hre.util.ScopedStack
import vct.col.ast
import vct.col.ast.RewriteHelpers.{RewriteAssign, RewriteDeref, RewriteJavaClass, RewriteMethodInvocation}
import vct.col.ast.{Assert, Assign, Block, BooleanValue, Branch, Class, ClassDeclaration, Declaration, Deref, DerefVeyMontThread, Eval, Expr, InstanceField, InstanceMethod, JavaClass, JavaInvocation, JavaMethod, JavaNamedType, JavaTClass, Local, Loop, MethodInvocation, Node, Program, RunMethod, Scope, Skip, Statement, TClass, ThisObject, ThisSeqProg, Type, VeyMontAssignExpression, VeyMontCommExpression, VeyMontCondition, VeyMontSeqProg, VeyMontThread}
import vct.col.origin.{Origin, PreferredNameOrigin}
import vct.col.ref.Ref
import vct.col.resolve.ctx.RefJavaMethod
import vct.col.rewrite.veymont.AddVeyMontAssignmentNodes.getDerefsFromExpr
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilderArg, Rewritten}
import vct.col.util.SuccessionMap
import vct.result.VerificationError.UserError
import vct.rewrite.veymont.ParalleliseVeyMontThreads.{ChannelClassOrigin, ParalliseVeyMontThreadsError, ThreadClassOrigin}

import scala.collection.immutable.Seq

object ParalleliseVeyMontThreads extends RewriterBuilderArg[JavaClass[_]] {
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

  case class ChannelClassOrigin(channelName: String, assign: Statement[_]) extends Origin {
    override def preferredName: String = channelName

    override def context: String = assign.o.context

    override def inlineContext: String = assign.o.inlineContext

    override def shortPosition: String = assign.o.shortPosition
  }
}

case class ParalleliseVeyMontThreads[Pre <: Generation](channelClass: JavaClass[_]) extends Rewriter[Pre] { outer =>

  private val threadBuildingBlocks: ScopedStack[ThreadBuildingBlocks[Pre]] = ScopedStack()
  private val threadClassSucc: SuccessionMap[VeyMontThread[Pre],Class[Post]] = SuccessionMap()
  private val threadMethodSucc: SuccessionMap[(VeyMontThread[Pre],ClassDeclaration[Pre]),InstanceMethod[Post]] = SuccessionMap()

  override def dispatch(decl : Declaration[Pre]) : Unit = {
    decl match {
      case seqProg: VeyMontSeqProg[Pre] =>
        val channelInfo = collectChannelsFromRun(seqProg) ++ collectChannelsFromMethods(seqProg)
        val indexedChannelInfo : Seq[ChannelInfo[Pre]] = channelInfo.groupBy(_.channelName).values.flatMap(chanInfoSeq =>
          if (chanInfoSeq.size <= 1) chanInfoSeq
          else chanInfoSeq.zipWithIndex.map{ case (chanInfo,index) => new ChannelInfo(chanInfo.comExpr,chanInfo.channelType,chanInfo.channelName + index) }).toSeq
        val channelClasses = generateChannelClasses(indexedChannelInfo)
        val channelFields = getChannelFields(indexedChannelInfo, channelClasses)
        seqProg.threads.foreach(thread => {
          val threadField = new InstanceField[Post](dispatch(thread.threadType), Set.empty)(thread.o)
          threadBuildingBlocks.having(new ThreadBuildingBlocks(seqProg.runMethod, seqProg.methods, channelFields, channelClasses, thread, threadField)) {
            dispatch(thread)
          }
        })
      case thread: VeyMontThread[Pre] => {
        if(threadBuildingBlocks.nonEmpty) {
          val threadRes: ThreadBuildingBlocks[Pre] = threadBuildingBlocks.top
          val threadMethods: Seq[ClassDeclaration[Post]] = createThreadMethod(thread, threadRes)
          createThreadClass(thread, threadRes, threadMethods)
        } else rewriteDefault(thread)
      }
      //case m: InstanceMethod[Pre] => getThreadMethod(m)
      //case r: RunMethod[Pre] => getThreadRunMethod(r)
      case other => rewriteDefault(other)
    }
  }

  private def createThreadMethod(thread: VeyMontThread[Pre], threadRes: ThreadBuildingBlocks[Pre]) = {
    threadRes.methods.map { preMethod =>
      val postMethod = getThreadMethodFromDecl(thread)(preMethod)
      threadMethodSucc.update((thread, preMethod), postMethod)
      postMethod
    }
  }

  private def createThreadClass(thread: VeyMontThread[Pre], threadRes: ThreadBuildingBlocks[Pre], threadMethods: Seq[ClassDeclaration[Post]]): Unit = {
    val channelFieldsForThread = threadRes.channelFields.view.filterKeys {
      case (comExpr,_) => comExpr.receiver.decl == thread || comExpr.sender.decl == thread
    }.values.toSeq
    val threadRun = getThreadRunFromDecl(thread, threadRes.runMethod)
    classDeclarations.scope {
      //classDeclarations.collect {
      val threadClass = new Class[Post](
        (threadRes.threadField +: channelFieldsForThread) ++ (threadRun +: threadMethods),
        Seq(),
        BooleanValue(true)(thread.o))(ThreadClassOrigin(thread))
      globalDeclarations.declare(threadClass)
      threadClassSucc.update(thread, threadClass)
    }
  }

  private def getThreadMethodFromDecl(thread: VeyMontThread[Pre])(decl: ClassDeclaration[Pre]): InstanceMethod[Post]  = decl match {
    case m: InstanceMethod[Pre] => getThreadMethod(m)
    case _ => throw ParalliseVeyMontThreadsError(thread, "Methods of seq_program need to be of type InstanceMethod")
  }

  private def getThreadRunFromDecl(thread: VeyMontThread[Pre], decl: ClassDeclaration[Pre]): RunMethod[Post] = decl match {
    case m: RunMethod[Pre] => getThreadRunMethod(m)
    case _ => throw ParalliseVeyMontThreadsError(thread, "RunMethod expected in seq_program")
  }

  private def getChannelFields(channelInfo: Seq[ChannelInfo[Pre]], channelClasses: Map[Type[Pre],JavaClass[Post]]): Map[(VeyMontCommExpression[Pre],Origin),InstanceField[Post]] = {
    channelInfo.map { chanInfo =>
      val chanField = new InstanceField[Post](JavaTClass(channelClasses(chanInfo.channelType).ref,Seq.empty), Set.empty)(ChannelClassOrigin(chanInfo.channelName,chanInfo.comExpr.assign))
      ((chanInfo.comExpr,chanInfo.comExpr.o), chanField) }.toMap
  }

  private def generateChannelClasses(channelInfo: Seq[ChannelInfo[Pre]]) : Map[Type[Pre],JavaClass[Post]] = {
    val channelTypes = channelInfo.map(_.channelType).toSet
    channelTypes.map(channelType =>
      channelType -> {
        val chanClassPre = channelClass.asInstanceOf[JavaClass[Pre]]
        val rw = new ChannelClassGenerator(channelType)
        new RewriteJavaClass[Pre, Post](chanClassPre)(rw).rewrite(decls = classDeclarations.collect {
          chanClassPre.decls.foreach(d => rw.dispatch(d))
        }._1)
      }
    ).toMap
  }

  case class ChannelClassGenerator(channelType: Type[_]) extends Rewriter[Pre] {
    override val allScopes = outer.allScopes

    override def dispatch(t: Type[Pre]): Type[Post] = t match {
      //  case jt: JavaType[Pre] => jt match {
      case jnt: JavaNamedType[Pre] =>
        if (jnt.names.head._1 == "MessageType") {
          dispatch(channelType.asInstanceOf[Type[Pre]])
        } else rewriteDefault(jnt)
      case _ => rewriteDefault(t)
    }
  }

  private def collectChannelsFromRun(seqProg: VeyMontSeqProg[Pre]) =
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

  private def getChannelNamesAndTypes(s: Statement[Pre]): Seq[ChannelInfo[Pre]] = {
    s.collect { case e@VeyMontCommExpression(recv, sender, chanType, assign) =>
      new ChannelInfo(e,chanType, recv.decl.o.preferredName + sender.decl.o.preferredName + "Channel")
    }
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

  private def getThreadRunMethod(run: RunMethod[Pre]): RunMethod[Post] = {
    new RunMethod[Post](
      run.body.map(dispatch),
      dispatch(run.contract))(run.blame)(run.o)
  }

  override def dispatch(node: Expr[Pre]): Expr[Post] = {
    if(threadBuildingBlocks.nonEmpty) {
      val thread = threadBuildingBlocks.top.thread
      node match {
        case c: VeyMontCondition[Pre] => c.condition.find{ case (threadRef,_) =>
          threadRef.decl == thread
        } match {
          case Some((_,threadExpr)) => dispatch(threadExpr)
          case _ => throw ParalliseVeyMontThreadsError(node, "Condition of if statement or while loop must contain an expression for every thread")
        }
        case m: MethodInvocation[Pre] => m.obj match {
          case threadRef: DerefVeyMontThread[Pre] => m.rewrite(obj = dispatch(threadRef))
          case _ => threadMethodSucc.get((thread, m.ref.decl)) match {
            case Some(postMethod) => m.rewrite(obj = dispatch(m.obj), ref = postMethod.ref, m.args.map(dispatch))
            case None => throw ParalliseVeyMontThreadsError(m, "No successor for this method found")
          }
        }
        case d: Deref[Pre] => d.obj match {
          case t: DerefVeyMontThread[Pre] =>
            val threadField = threadBuildingBlocks.top.threadField
            if(t.ref.decl == thread) {
              d.rewrite(
                obj = Deref(ThisObject(threadClassSucc.ref[Post, Class[Post]](thread))(thread.o), threadField.ref[InstanceField[Post]])(null)(d.o)
              )
            }
            else rewriteDefault(node)

        }
        case _ => rewriteDefault(node)
      }
    } else rewriteDefault(node)
  }


    override def dispatch(st : Statement[Pre]) : Statement[Post] = {
      if (threadBuildingBlocks.nonEmpty) {
        val thread = threadBuildingBlocks.top.thread
        st match {
          case v@VeyMontCommExpression(recv,sender,chanType,assign) =>
            val channelField = threadBuildingBlocks.top.channelFields((v,v.o))
            val channelClass = threadBuildingBlocks.top.channelClasses(chanType)
            val thisChanField = Deref(ThisObject(threadClassSucc.ref[Post, Class[Post]](thread))(thread.o), channelField.ref[InstanceField[Post]])(null)(assign.o)
            val assignment = assign.asInstanceOf[Assign[Pre]]
            if (recv.decl == thread) {
              val readMethod = findChannelClassMethod(v, channelClass, "readValue")
              val assignValue = JavaInvocation(Some(thisChanField),Seq.empty, "readValue",Seq.empty, Seq.empty,Seq.empty)(null)(v.o)
              assignValue.ref = Some(RefJavaMethod(readMethod))
              Assign(dispatch(assignment.target),assignValue)(null)(v.o)
            } else if(sender.decl == thread) {
              val writeMethod = findChannelClassMethod(v, channelClass, "writeValue")
              val writeInvoc = JavaInvocation(Some(thisChanField),Seq.empty,"writeValue",Seq(dispatch(assignment.value)),Seq.empty,Seq.empty)(null)(v.o)
              writeInvoc.ref = Some(RefJavaMethod(writeMethod))
              Eval(writeInvoc)(v.o)
            }
            else Skip()(assign.o)
          case v@VeyMontAssignExpression(threadRef, assign) =>
            if (threadRef.decl == thread)
              dispatch(assign)
            else Skip()(assign.o)
          case a: Assign[Pre] => rewriteDefault(st)
          case Branch(_) => rewriteDefault(st)
          case Loop(_, _, _, _, _) => rewriteDefault(st)
          case Scope(_, _) => rewriteDefault(st)
          case Block(_) => rewriteDefault(st)
          case Eval(expr) => expr match {
            case m: MethodInvocation[Pre] => m.obj match {
              case _: ThisSeqProg[Pre] => Eval(dispatch(expr))(st.o)
              case d: DerefVeyMontThread[Pre] => if(d.ref.decl == thread) Eval(dispatch(expr))(st.o) else Skip()(st.o)
              case _ => throw ParalliseVeyMontThreadsError(st, "Statement not allowed in seq_program")
            }
            case _ => rewriteDefault(st)
          }
          case _: Assert[Pre] => Skip()(st.o)
          case _ => throw ParalliseVeyMontThreadsError(st, "Statement not allowed in seq_program")
        }
      } else rewriteDefault(st)
    }

  private def findChannelClassMethod(v: VeyMontCommExpression[Pre], channelClass: JavaClass[Post], methodName: String): JavaMethod[Post] = {
    val method = channelClass.decls.find {
      case jm: JavaMethod[Post] => jm.name == methodName
      case _ => false
    }
    method match {
      case Some(m : JavaMethod[Post]) => m
      case _ => throw ParalliseVeyMontThreadsError(v, "Could not find method `" + methodName + "' for channel class " + channelClass.name)
    }
  }

}
