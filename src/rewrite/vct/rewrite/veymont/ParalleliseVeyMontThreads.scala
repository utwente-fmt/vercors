package vct.rewrite.veymont

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers.{RewriteApplicableContract, RewriteClass, RewriteDeref, RewriteJavaClass, RewriteJavaConstructor, RewriteMethodInvocation}
import vct.col.ast.{AbstractRewriter, ApplicableContract, Assert, Assign, Block, BooleanValue, Branch, Class, ClassDeclaration, CommunicateX, Declaration, Deref, Endpoint, EndpointUse, Eval, Expr, InstanceField, InstanceMethod, JavaClass, JavaConstructor, JavaInvocation, JavaLocal, JavaMethod, JavaNamedType, JavaParam, JavaPublic, JavaTClass, Local, Loop, MethodInvocation, NewObject, Node, Procedure, Program, RunMethod, Scope, SeqProg, SeqRun, Statement, TClass, TVeyMontChannel, TVoid, ThisObject, ThisSeqProg, Type, UnitAccountedPredicate, Variable, VeyMontAssignExpression, VeyMontCondition}
import vct.col.origin.Origin
import vct.col.resolve.ctx.RefJavaMethod
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, RewriterBuilderArg, Rewritten}
import vct.col.util.SuccessionMap
import vct.result.VerificationError.{Unreachable, UserError}
import vct.rewrite.veymont.ParalleliseEndpoints.{ChannelFieldOrigin, ParalleliseEndpointsError, RunMethodOrigin, ThreadClassOrigin, getChannelClassName, getThreadClassName, getVarName}

import java.lang

object ParalleliseEndpoints extends RewriterBuilderArg[JavaClass[_]] {
  override def key: String = "ParalleliseEndpoints"

  override def desc: String = "Generate classes for VeyMont threads in parallel program"

  private val channelClassName = "Channel"
  private val threadClassName = "Thread"

  def getChannelClassName(channelType: Type[_]): String =
    channelType.toString.capitalize + channelClassName

  def getThreadClassName(thread: Endpoint[_]) : String =
    thread.o.getPreferredNameOrElse().ucamel + threadClassName

  def getVarName(v: Variable[_]) = v.o.getPreferredNameOrElse()

  case class ParalleliseEndpointsError(node : Node[_], msg: String) extends UserError {
    override def code: String = "paralleliseEndpointsError"
    override def text: String = node.o.messageInContext(msg)
  }

  private def ThreadClassOrigin(thread: Endpoint[_]): Origin =
    thread.o.where(name = getThreadClassName(thread))

  private def ChannelFieldOrigin(channelName: String, assign: Statement[_]): Origin =
    assign.o.where(name = channelName)

  private def RunMethodOrigin(runMethod: SeqRun[_]): Origin =
    runMethod.o.where(name = "run")
}

case class ParalleliseEndpoints[Pre <: Generation](channelClass: JavaClass[_]) extends Rewriter[Pre] { outer =>

  private val threadBuildingBlocks: ScopedStack[ThreadBuildingBlocks[Pre]] = ScopedStack()
  private val threadClassSucc: SuccessionMap[Endpoint[Pre],Class[Post]] = SuccessionMap()
  private val threadMethodSucc: SuccessionMap[(Endpoint[Pre],ClassDeclaration[Pre]),InstanceMethod[Post]] = SuccessionMap()
  private val givenClassSucc: SuccessionMap[Type[Pre],Class[Post]] = SuccessionMap()
  private val givenClassConstrSucc: SuccessionMap[Type[Pre],Procedure[Pre]] = SuccessionMap()

  override def dispatch(decl : Declaration[Pre]) : Unit = {
    decl match {
      case p: Procedure[Pre] => givenClassConstrSucc.update(p.returnType,p)
      case c : Class[Pre] => globalDeclarations.succeed(c, dispatchGivenClass(c))
      case seqProg: SeqProg[Pre] => dispatchThreads(seqProg)
      case thread: Endpoint[Pre] => dispatchThread(thread)
      case other => rewriteDefault(other)
    }
  }

  private def dispatchThread(thread: Endpoint[Pre]): Unit = {
    if (threadBuildingBlocks.nonEmpty) {
      val threadRes: ThreadBuildingBlocks[Pre] = threadBuildingBlocks.top
      val threadMethods: Seq[ClassDeclaration[Post]] = createThreadMethod(thread, threadRes)
      createThreadClass(thread, threadRes, threadMethods)
    } else rewriteDefault(thread)
  }

  private def dispatchThreads(seqProg: SeqProg[Pre]): Unit = {
    val (channelClasses,indexedChannelInfo) = extractChannelInfo(seqProg)
    channelClasses.foreach{ case (t,c) =>
      globalDeclarations.declare(c)
    }
    seqProg.endpoints.foreach(thread => {
      val threadField = new InstanceField[Post](TClass(givenClassSucc.ref(thread.t)), Set.empty)(thread.o)
      val channelFields = getChannelFields(thread, indexedChannelInfo, channelClasses)
      threadBuildingBlocks.having(new ThreadBuildingBlocks(seqProg.run, seqProg.decls, channelFields, channelClasses, thread, threadField)) {
        dispatch(thread)
      }
    })
  }

  private def dispatchGivenClass(c: Class[Pre]): Class[Post] = {
    val rw = GivenClassRewriter()
    val gc = new RewriteClass[Pre, Post](c)(rw).rewrite(
      declarations = classDeclarations.collect {
        (givenClassConstrSucc.get(TClass(c.ref)).get +: c.declarations).foreach(d => rw.dispatch(d))
      }._1)
    givenClassSucc.update(TClass(c.ref),gc)
    gc
  }

  case class GivenClassRewriter() extends Rewriter[Pre] {
    override val allScopes = outer.allScopes

    val rewritingConstr: ScopedStack[(Seq[Variable[Pre]],TClass[Pre])] = ScopedStack()

    override def dispatch(decl: Declaration[Pre]): Unit = decl match {
      case p: Procedure[Pre] => p.returnType match {
        case tc: TClass[Pre] => rewritingConstr.having(p.args,tc){ classDeclarations.declare(createClassConstructor(p)) };
        case _ => ??? //("This procedure is expected to have a class as return type");
      }
      case other => rewriteDefault(other)
    }

    // PB: from what I understand this restores a constructor from a generated procedure, so this should be refactored
    // once we make constructors first class.
    def createClassConstructor(p: Procedure[Pre]): JavaConstructor[Post] =
      new JavaConstructor[Post](Seq(JavaPublic[Post]()(p.o)),
        rewritingConstr.top._2.cls.decl.o.getPreferredNameOrElse().ucamel,
        p.args.map(createJavaParam),
        variables.dispatch(p.typeArgs),
        Seq.empty,
        p.body match {
          case Some(s: Scope[Pre]) => s.body match {
            case b: Block[Pre] => dispatch(Block(b.statements.tail.dropRight(1))(p.o))
            case other => dispatch(other)
          }
          case Some(_) => throw Unreachable("The body of a procedure always starts with a Scope.")
          case None => Block(Seq.empty)(p.o)
        },
        p.contract.rewrite(ensures = UnitAccountedPredicate[Post](BooleanValue(true)(p.o))(p.o)))(null)(p.o)

    def createJavaParam(v: Variable[Pre]): JavaParam[Post] =
      new JavaParam[Post](Seq.empty, getVarName(v).camel, dispatch(v.t))(v.o)

    override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
      case l: Local[Pre] =>
        if(rewritingConstr.nonEmpty && rewritingConstr.top._1.contains(l.ref.decl))
          JavaLocal[Post](getVarName(l.ref.decl).camel)(null)(e.o)
        else rewriteDefault(l)
      case t: ThisObject[Pre] =>
        val thisClassType = TClass(t.cls)
        if(rewritingConstr.nonEmpty && rewritingConstr.top._2 == thisClassType)
          ThisObject(givenClassSucc.ref[Post,Class[Post]](thisClassType))(t.o)
        else rewriteDefault(t)
      case d: Deref[Pre] =>
        if(rewritingConstr.nonEmpty)
          d.obj match {
            case _: Local[Pre] => d.rewrite(obj = ThisObject(givenClassSucc.ref[Post,Class[Post]](rewritingConstr.top._2))(d.o))
            case other => rewriteDefault(other)
          }
        else rewriteDefault(d)
      case other => rewriteDefault(other)
    }
  }

  private def extractChannelInfo(seqProg: SeqProg[Pre]): (Map[Type[Pre], JavaClass[Post]], Seq[ChannelInfo[Pre]]) = {
    val channelInfo = getChannelNamesAndTypes(seqProg.run.body) ++ collectChannelsFromMethods(seqProg)
    val indexedChannelInfo: Seq[ChannelInfo[Pre]] = channelInfo.groupBy(_.channelName).values.flatMap(chanInfoSeq =>
      if (chanInfoSeq.size <= 1) chanInfoSeq
      else chanInfoSeq.zipWithIndex.map { case (chanInfo, index) => new ChannelInfo(chanInfo.comExpr, chanInfo.channelType, chanInfo.channelName + index) }).toSeq
    val channelClasses = generateChannelClasses(indexedChannelInfo)
    (channelClasses, indexedChannelInfo)
  }

  private def createThreadMethod(thread: Endpoint[Pre], threadRes: ThreadBuildingBlocks[Pre]) = {
    threadRes.methods.map { preMethod =>
      val postMethod = getThreadMethodFromDecl(thread)(preMethod)
      threadMethodSucc.update((thread, preMethod), postMethod)
      postMethod
    }
  }

  private def createThreadClass(thread: Endpoint[Pre], threadRes: ThreadBuildingBlocks[Pre], threadMethods: Seq[ClassDeclaration[Post]]): Unit = {
    val threadConstr = createThreadClassConstructor(thread,threadRes.threadField)
    val threadRun = getThreadRunMethod(threadRes.runMethod)
    classDeclarations.scope {
      val threadClass = new Class[Post](
        (threadRes.threadField +: threadRes.channelFields.values.toSeq) ++ (threadConstr +: threadRun +: threadMethods), Seq(), BooleanValue(true)(thread.o))(ThreadClassOrigin(thread))
      globalDeclarations.declare(threadClass)
      threadClassSucc.update(thread, threadClass)
    }
  }

  private def createThreadClassConstructor(thread: Endpoint[Pre], threadField: InstanceField[Post]): JavaConstructor[Post] = {
    val threadConstrArgBlocks = thread.args.map{
      case l: Local[Pre] => (l.ref.decl.o.getPreferredNameOrElse(),dispatch(l.t))
      case other => throw ParalleliseEndpointsError(other,"This node is expected to be an argument of seq_prog, and have type Local")
    }
    val threadConstrArgs: Seq[JavaParam[Post]] =
      threadConstrArgBlocks.map{ case (a,t) => new JavaParam[Post](Seq.empty, a.camel, t)(ThreadClassOrigin(thread)) }
    val passedArgs = threadConstrArgs.map(a => JavaLocal[Post](a.name)(null)(ThreadClassOrigin(thread)))
    // TODO: The next check cannot fail anymore
    val threadTypeName = thread.t match { //TODO: replace by using givenClassSucc
      case tc: TClass[Pre] => tc.cls.decl.o.getPreferredNameOrElse()
      case _ => throw ParalleliseEndpointsError(thread, "This type is expected to be a class")
    }
    val threadConstrBody = {
      Assign(getThisVeyMontDeref(thread,ThreadClassOrigin(thread),threadField),
      JavaInvocation[Post](None, Seq.empty, "new " + threadTypeName, passedArgs, Seq.empty, Seq.empty)(null)(ThreadClassOrigin(thread)))(null)(ThreadClassOrigin(thread))
    }
    val threadConstrContract = new ApplicableContract[Post](
      UnitAccountedPredicate[Post](BooleanValue(true)(ThreadClassOrigin(thread)))(ThreadClassOrigin(thread)),
      UnitAccountedPredicate[Post](BooleanValue(true)(ThreadClassOrigin(thread)))(ThreadClassOrigin(thread)),
      BooleanValue(true)(ThreadClassOrigin(thread)),
      Seq.empty, Seq.empty, Seq.empty, None)(null)(ThreadClassOrigin(thread))
    new JavaConstructor[Post](
      Seq(JavaPublic[Post]()(ThreadClassOrigin(thread))),
      getThreadClassName(thread),
      threadConstrArgs,
      Seq.empty, Seq.empty,
      threadConstrBody,
      threadConstrContract)(ThreadClassOrigin(thread))(ThreadClassOrigin(thread))
  }

  private def getThreadMethodFromDecl(thread: Endpoint[Pre])(decl: ClassDeclaration[Pre]): InstanceMethod[Post]  = decl match {
    case m: InstanceMethod[Pre] => getThreadMethod(m)
    case _ => throw ParalleliseEndpointsError(thread, "Methods of seq_program need to be of type InstanceMethod")
  }

  private def getChannelFields(thread: Endpoint[Pre], channelInfo: Seq[ChannelInfo[Pre]], channelClasses: Map[Type[Pre],JavaClass[Post]]): Map[(CommunicateX[Pre],Origin),InstanceField[Post]] = {
    channelInfo
      .filter( chanInfo => chanInfo.comExpr.receiver.decl == thread || chanInfo.comExpr.sender.decl == thread)
      .map { chanInfo =>
        val chanFieldOrigin = ChannelFieldOrigin(chanInfo.channelName,chanInfo.comExpr.assign)
        val chanField = new InstanceField[Post](TVeyMontChannel(getChannelClassName(chanInfo.channelType)), Set.empty)(chanFieldOrigin)
        ((chanInfo.comExpr, chanInfo.comExpr.o), chanField)
      }.toMap
  }

  private def generateChannelClasses(channelInfo: Seq[ChannelInfo[Pre]]) : Map[Type[Pre],JavaClass[Post]] = {
    val channelTypes = channelInfo.map(_.channelType).toSet
    channelTypes.map(channelType =>
      channelType -> {
        val chanClassPre = channelClass.asInstanceOf[JavaClass[Pre]]
        val rw = ChannelClassGenerator(channelType)
        new RewriteJavaClass[Pre, Post](chanClassPre)(rw).rewrite(
          name = getChannelClassName(channelType),
          modifiers = Seq.empty,
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

  private def collectChannelsFromMethods(seqProg: SeqProg[Pre]) =
    seqProg.decls.flatMap {
      case m: InstanceMethod[Pre] =>
        m.body.map(getChannelNamesAndTypes).getOrElse(throw ParalleliseEndpointsError(m, "Abstract methods are not supported inside `seq_prog`."))
      case other => throw ParalleliseEndpointsError(other, "seq_program method expected")
    }

  private def getChannelNamesAndTypes(s: Statement[Pre]): Seq[ChannelInfo[Pre]] = {
    s.collect { case e@CommunicateX(recv, sender, chanType, assign) =>
      new ChannelInfo(e,chanType, recv.decl.o.getPreferredNameOrElse().ucamel
        + sender.decl.o.getPreferredNameOrElse().camel + "Channel")
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

  private def getThreadRunMethod(run: SeqRun[Pre]): InstanceMethod[Post] = {
    new InstanceMethod[Post](
      TVoid[Post](),
      Seq.empty,Seq.empty,Seq.empty,
      Some(dispatch(run.body)),
      dispatch(run.contract))(run.blame)(RunMethodOrigin(run))
  }

  override def dispatch(node: Expr[Pre]): Expr[Post] = {
    if(threadBuildingBlocks.nonEmpty) {
      val thread = threadBuildingBlocks.top.thread
      node match {
        case c: VeyMontCondition[Pre] => paralleliseThreadCondition(node, thread, c)
        case m: MethodInvocation[Pre] => updateThreadRefMethodInvoc(thread, m)
        case d: Deref[Pre] => updateThreadRefInDeref(node, thread, d)
        case t: EndpointUse[Pre] => updateThreadRefVeyMontDeref(node, thread, t)
        case _ => rewriteDefault(node)
      }
    } else rewriteDefault(node)
  }

  private def updateThreadRefVeyMontDeref(node: Expr[Pre], thread: Endpoint[Pre], t: EndpointUse[Pre]) = {
    if (t.ref.decl == thread) {
      getThisVeyMontDeref(thread, t.o, threadBuildingBlocks.top.threadField)
    } else rewriteDefault(node)
  }

  private def updateThreadRefInDeref(node: Expr[Pre], thread: Endpoint[Pre], d: Deref[Pre]) = {
    d.obj match {
      case t: EndpointUse[Pre] if t.ref.decl == thread =>
        d.rewrite(
          obj = getThisVeyMontDeref(thread, d.o, threadBuildingBlocks.top.threadField)
        )
      case _ => rewriteDefault(node)
    }
  }

  private def updateThreadRefMethodInvoc(thread: Endpoint[Pre], m: MethodInvocation[Pre]) = {
    m.obj match {
      case threadRef: EndpointUse[Pre] => m.rewrite(obj = dispatch(threadRef))
      case _ => threadMethodSucc.get((thread, m.ref.decl)) match {
        case Some(postMethod) => m.rewrite(obj = dispatch(m.obj), ref = postMethod.ref, m.args.map(dispatch))
        case None => throw ParalleliseEndpointsError(m, "No successor for this method found")
      }
    }
  }

  private def paralleliseThreadCondition(node: Expr[Pre], thread: Endpoint[Pre], c: VeyMontCondition[Pre]) = {
    c.condition.find { case (threadRef, _) =>
      threadRef.decl == thread
    } match {
      case Some((_, threadExpr)) => dispatch(threadExpr)
      case _ => throw ParalleliseEndpointsError(node, "Condition of if statement or while loop must contain an expression for every thread")
    }
  }

  private def getThisVeyMontDeref(thread: Endpoint[Pre], o: Origin, threadField: InstanceField[Rewritten[Pre]]) = {
    Deref(ThisObject(threadClassSucc.ref[Post, Class[Post]](thread))(thread.o), threadField.ref[InstanceField[Post]])(null)(o)
  }

  override def dispatch(st : Statement[Pre]) : Statement[Post] = {
      if (threadBuildingBlocks.nonEmpty) {
        val thread = threadBuildingBlocks.top.thread
        st match {
          case v: CommunicateX[Pre] =>
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
          case _ => throw ParalleliseEndpointsError(st, "Statement not allowed in seq_program")
        }
      } else rewriteDefault(st)
    }

  private def createParComBlocks(thread: Endpoint[Pre], v: CommunicateX[Pre]): ParallelCommExprBuildingBlocks[Pre] = {
    val channelField = threadBuildingBlocks.top.channelFields((v, v.o))
    val channelClass = threadBuildingBlocks.top.channelClasses(v.chanType)
    val thisChanField = Deref(ThisObject(threadClassSucc.ref[Post, Class[Post]](thread))(thread.o), channelField.ref[InstanceField[Post]])(null)(v.assign.o)
    val assignment = v.assign.asInstanceOf[Assign[Pre]]
    new ParallelCommExprBuildingBlocks(channelField, channelClass, thisChanField, assignment)
  }

  private def paralleliseMethodInvocation(st: Statement[Pre], thread: Endpoint[Pre], expr: Expr[Pre]): Statement[Post] = {
    expr match {
      case m: MethodInvocation[Pre] => m.obj match {
        case _: ThisSeqProg[Pre] => Eval(m.rewrite(obj = ThisObject(threadClassSucc.ref[Post, Class[Post]](thread))(thread.o), ref = threadMethodSucc.ref((thread, m.ref.decl))))(st.o)
        case d: EndpointUse[Pre] => if (d.ref.decl == thread) Eval(dispatch(expr))(st.o) else Block(Seq.empty)(st.o)
        case _ => throw ParalleliseEndpointsError(st, "Statement not allowed in seq_program")
      }
      case _ => throw ParalleliseEndpointsError(st, "Statement not allowed in seq_program")
    }
  }

  private def paralleliseVeyMontCommExpr(thread: Endpoint[Pre], v: CommunicateX[Pre], blocks: ParallelCommExprBuildingBlocks[Pre]): Statement[Post] = {
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

  private def findChannelClassMethod(v: CommunicateX[Pre], channelClass: JavaClass[Post], methodName: String): JavaMethod[Post] = {
    val method = channelClass.decls.find {
      case jm: JavaMethod[Post] => jm.name == methodName
      case _ => false
    }
    method match {
      case Some(m : JavaMethod[Post]) => m
      case _ => throw ParalleliseEndpointsError(v, "Could not find method `" + methodName + "' for channel class " + channelClass.name)
    }
  }

}
