package vct.rewrite.veymont

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers.{RewriteDeref, RewriteJavaClass, RewriteJavaConstructor, RewriteMethodInvocation}
import vct.col.ast.{Assert, Assign, Block, BooleanValue, Branch, Class, ClassDeclaration, Declaration, Deref, DerefVeyMontThread, Eval, Expr, InstanceField, InstanceMethod, JavaClass, JavaConstructor, JavaInvocation, JavaMethod, JavaNamedType, JavaTClass, Local, Loop, MethodInvocation, Node, Program, RunMethod, Scope, Statement, TClass, ThisObject, ThisSeqProg, Type, VeyMontAssignExpression, VeyMontCommExpression, VeyMontCondition, VeyMontSeqProg, VeyMontThread}
import vct.col.origin.Origin
import vct.col.resolve.ctx.RefJavaMethod
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilderArg, Rewritten}
import vct.col.util.SuccessionMap
import vct.result.VerificationError.UserError
import vct.rewrite.veymont.ParalleliseVeyMontThreads.{ChannelFieldOrigin, ParalliseVeyMontThreadsError, ThreadClassOrigin}

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

  case class ChannelFieldOrigin(channelName: String, assign: Statement[_]) extends Origin {
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
  private val channelClassSucc: SuccessionMap[Type[Pre],JavaClass[Post]] = SuccessionMap()
  private val channelClassName = "Channel"

  override def dispatch(decl : Declaration[Pre]) : Unit = {
    decl match {
      case seqProg: VeyMontSeqProg[Pre] => dispatchThreads(seqProg)
      case thread: VeyMontThread[Pre] => dispatchThread(thread)
      case other => rewriteDefault(other)
    }
  }

  private def dispatchThread(thread: VeyMontThread[Pre]): Unit = {
    if (threadBuildingBlocks.nonEmpty) {
      val threadRes: ThreadBuildingBlocks[Pre] = threadBuildingBlocks.top
      val threadMethods: Seq[ClassDeclaration[Post]] = createThreadMethod(thread, threadRes)
      createThreadClass(thread, threadRes, threadMethods)
    } else rewriteDefault(thread)
  }

  private def dispatchThreads(seqProg: VeyMontSeqProg[Pre]): Unit = {
    val (channelClasses,indexedChannelInfo) = extractChannelInfo(seqProg)
    channelClasses.foreach{ case (t,c) =>
      globalDeclarations.declare(c)
      channelClassSucc.update(t,c)
    }
    seqProg.threads.foreach(thread => {
      val threadField = new InstanceField[Post](dispatch(thread.threadType), Set.empty)(thread.o)
      val channelFields = getChannelFields(thread, indexedChannelInfo, channelClasses)
      threadBuildingBlocks.having(new ThreadBuildingBlocks(seqProg.runMethod, seqProg.methods, channelFields, channelClasses, thread, threadField)) {
        dispatch(thread)
      }
    })
  }

  private def extractChannelInfo(seqProg: VeyMontSeqProg[Pre]): (Map[Type[Pre], JavaClass[Post]], Seq[ChannelInfo[Pre]]) = {
    val channelInfo = collectChannelsFromRun(seqProg) ++ collectChannelsFromMethods(seqProg)
    val indexedChannelInfo: Seq[ChannelInfo[Pre]] = channelInfo.groupBy(_.channelName).values.flatMap(chanInfoSeq =>
      if (chanInfoSeq.size <= 1) chanInfoSeq
      else chanInfoSeq.zipWithIndex.map { case (chanInfo, index) => new ChannelInfo(chanInfo.comExpr, chanInfo.channelType, chanInfo.channelName + index) }).toSeq
    val channelClasses = generateChannelClasses(indexedChannelInfo)
    (channelClasses, indexedChannelInfo)
  }

  private def createThreadMethod(thread: VeyMontThread[Pre], threadRes: ThreadBuildingBlocks[Pre]) = {
    threadRes.methods.map { preMethod =>
      val postMethod = getThreadMethodFromDecl(thread)(preMethod)
      threadMethodSucc.update((thread, preMethod), postMethod)
      postMethod
    }
  }

  private def createThreadClass(thread: VeyMontThread[Pre], threadRes: ThreadBuildingBlocks[Pre], threadMethods: Seq[ClassDeclaration[Post]]): Unit = {
    val threadRun = getThreadRunFromDecl(thread, threadRes.runMethod)
    classDeclarations.scope {
      val threadClass = new Class[Post](
        (threadRes.threadField +: threadRes.channelFields.values.toSeq) ++ (threadRun +: threadMethods), Seq(), BooleanValue(true)(thread.o))(ThreadClassOrigin(thread))
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

  private def getChannelFields(thread: VeyMontThread[Pre], channelInfo: Seq[ChannelInfo[Pre]], channelClasses: Map[Type[Pre],JavaClass[Post]]): Map[(VeyMontCommExpression[Pre],Origin),InstanceField[Post]] = {
    channelInfo
      .filter( chanInfo => chanInfo.comExpr.receiver.decl == thread || chanInfo.comExpr.sender.decl == thread)
      .map { chanInfo =>
        val chanFieldOrigin = ChannelFieldOrigin(chanInfo.channelName,chanInfo.comExpr.assign)
        val chanField = new InstanceField[Post](JavaTClass(channelClassSucc.ref(chanInfo.channelType),Seq.empty), Set.empty)(chanFieldOrigin)
        ((chanInfo.comExpr, chanInfo.comExpr.o), chanField)
      }.toMap
  }

  private def getChannelClassName(channelType: Type[_]): String =
    channelType.toString.capitalize + channelClassName

  private def generateChannelClasses(channelInfo: Seq[ChannelInfo[Pre]]) : Map[Type[Pre],JavaClass[Post]] = {
    val channelTypes = channelInfo.map(_.channelType).toSet
    channelTypes.map(channelType =>
      channelType -> {
        val chanClassPre = channelClass.asInstanceOf[JavaClass[Pre]]
        val rw = ChannelClassGenerator(channelType)
        new RewriteJavaClass[Pre, Post](chanClassPre)(rw).rewrite(
          name = getChannelClassName(channelType),
          decls = classDeclarations.collect {
            chanClassPre.decls.foreach(d => rw.dispatch(d))
          }._1)
      }
    ).toMap
  }

  case class ChannelClassGenerator(channelType: Type[_]) extends Rewriter[Pre] {
    override val allScopes = outer.allScopes

    override def dispatch(t: Type[Pre]): Type[Post] = t match {
      case jnt: JavaNamedType[Pre] =>
        if (jnt.names.head._1 == "MessageType") {
          dispatch(channelType.asInstanceOf[Type[Pre]])
        } else rewriteDefault(jnt)
      case _ => rewriteDefault(t)
    }

    override def dispatch(decl: Declaration[Pre]): Unit = decl match {
      case jc: JavaConstructor[Pre] => classDeclarations.declare(jc.rewrite(name = getChannelClassName(channelType)))
      case other => rewriteDefault(other)
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
        case c: VeyMontCondition[Pre] => paralleliseThreadCondition(node, thread, c)
        case m: MethodInvocation[Pre] => updateThreadRefMethodInvoc(thread, m)
        case d: Deref[Pre] => updateThreadRefInDeref(node, thread, d)
        case t: DerefVeyMontThread[Pre] => updateThreadRefVeyMontDeref(node, thread, t)
        case _ => rewriteDefault(node)
      }
    } else rewriteDefault(node)
  }

  private def updateThreadRefVeyMontDeref(node: Expr[Pre], thread: VeyMontThread[Pre], t: DerefVeyMontThread[Pre]) = {
    if (t.ref.decl == thread) {
      getThisVeyMontDeref(thread, t.o, threadBuildingBlocks.top.threadField)
    } else rewriteDefault(node)
  }

  private def updateThreadRefInDeref(node: Expr[Pre], thread: VeyMontThread[Pre], d: Deref[Pre]) = {
    d.obj match {
      case t: DerefVeyMontThread[Pre] =>
        if (t.ref.decl == thread) {
          d.rewrite(
            obj = getThisVeyMontDeref(thread, d.o, threadBuildingBlocks.top.threadField)
          )
        }
        else rewriteDefault(node)
    }
  }

  private def updateThreadRefMethodInvoc(thread: VeyMontThread[Pre], m: MethodInvocation[Pre]) = {
    m.obj match {
      case threadRef: DerefVeyMontThread[Pre] => m.rewrite(obj = dispatch(threadRef))
      case _ => threadMethodSucc.get((thread, m.ref.decl)) match {
        case Some(postMethod) => m.rewrite(obj = dispatch(m.obj), ref = postMethod.ref, m.args.map(dispatch))
        case None => throw ParalliseVeyMontThreadsError(m, "No successor for this method found")
      }
    }
  }

  private def paralleliseThreadCondition(node: Expr[Pre], thread: VeyMontThread[Pre], c: VeyMontCondition[Pre]) = {
    c.condition.find { case (threadRef, _) =>
      threadRef.decl == thread
    } match {
      case Some((_, threadExpr)) => dispatch(threadExpr)
      case _ => throw ParalliseVeyMontThreadsError(node, "Condition of if statement or while loop must contain an expression for every thread")
    }
  }

  private def getThisVeyMontDeref(thread: VeyMontThread[Pre], o: Origin, threadField: InstanceField[Rewritten[Pre]]) = {
    Deref(ThisObject(threadClassSucc.ref[Post, Class[Post]](thread))(thread.o), threadField.ref[InstanceField[Post]])(null)(o)
  }

  override def dispatch(st : Statement[Pre]) : Statement[Post] = {
      if (threadBuildingBlocks.nonEmpty) {
        val thread = threadBuildingBlocks.top.thread
        st match {
          case v: VeyMontCommExpression[Pre] =>
            paralleliseVeyMontCommExpr(thread, v, createParComBlocks(thread, v))
          case v@VeyMontAssignExpression(threadRef, assign) =>
            if (threadRef.decl == thread)
              dispatch(assign)
            else Block(Seq.empty)(assign.o)
          case a: Assign[Pre] => Assign(dispatch(a.target),dispatch(a.value))(a.blame)(a.o)
          case Branch(_) => rewriteDefault(st)
          case Loop(_, _, _, _, _) => rewriteDefault(st)
          case Scope(_, _) => rewriteDefault(st)
          case Block(_) => rewriteDefault(st)
          case Eval(expr) => paralleliseMethodInvocation(st, thread, expr)
          case _: Assert[Pre] => Block(Seq.empty)(st.o)
          case _ => throw ParalliseVeyMontThreadsError(st, "Statement not allowed in seq_program")
        }
      } else rewriteDefault(st)
    }

  private def createParComBlocks(thread: VeyMontThread[Pre], v: VeyMontCommExpression[Pre]): ParallelCommExprBuildingBlocks[Pre] = {
    val channelField = threadBuildingBlocks.top.channelFields((v, v.o))
    val channelClass = threadBuildingBlocks.top.channelClasses(v.chanType)
    val thisChanField = Deref(ThisObject(threadClassSucc.ref[Post, Class[Post]](thread))(thread.o), channelField.ref[InstanceField[Post]])(null)(v.assign.o)
    val assignment = v.assign.asInstanceOf[Assign[Pre]]
    new ParallelCommExprBuildingBlocks(channelField, channelClass, thisChanField, assignment)
  }

  private def paralleliseMethodInvocation(st: Statement[Pre], thread: VeyMontThread[Pre], expr: Expr[Pre]): Statement[Post] = {
    expr match {
      case m: MethodInvocation[Pre] => m.obj match {
        case _: ThisSeqProg[Pre] => Eval(m.rewrite(obj = ThisObject(threadClassSucc.ref[Post, Class[Post]](thread))(thread.o), ref = threadMethodSucc.ref((thread, m.ref.decl))))(st.o)
        case d: DerefVeyMontThread[Pre] => if (d.ref.decl == thread) Eval(dispatch(expr))(st.o) else Block(Seq.empty)(st.o)
        case _ => throw ParalliseVeyMontThreadsError(st, "Statement not allowed in seq_program")
      }
      case _ => throw ParalliseVeyMontThreadsError(st, "Statement not allowed in seq_program")
    }
  }

  private def paralleliseVeyMontCommExpr(thread: VeyMontThread[Pre], v: VeyMontCommExpression[Pre], blocks: ParallelCommExprBuildingBlocks[Pre]): Statement[Post] = {
    if (v.receiver.decl == thread) {
      val readMethod = findChannelClassMethod(v, blocks.channelClass, "readValue")
      val assignValue = JavaInvocation(Some(blocks.thisChanField), Seq.empty, "readValue", Seq.empty, Seq.empty, Seq.empty)(null)(v.o)
      assignValue.ref = Some(RefJavaMethod(readMethod))
      Assign(dispatch(blocks.assign.target), assignValue)(null)(v.o)
    } else if (v.sender.decl == thread) {
      val writeMethod = findChannelClassMethod(v, blocks.channelClass, "writeValue")
      val writeInvoc = JavaInvocation(Some(blocks.thisChanField), Seq.empty, "writeValue", Seq(dispatch(blocks.assign.value)), Seq.empty, Seq.empty)(null)(v.o)
      writeInvoc.ref = Some(RefJavaMethod(writeMethod))
      Eval(writeInvoc)(v.o)
    }
    else Block(Seq.empty)(blocks.assign.o)
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
