package vct.rewrite.veymont

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast.{AbstractRewriter, ApplicableContract, Assert, Assign, Block, BooleanValue, Branch, ChorBranch, ChorGuard, ChorLoop, ChorRun, ChorStatement, Choreography, Class, ClassDeclaration, CommunicateX, ConstructorInvocation, Declaration, Deref, Endpoint, EndpointGuard, EndpointName, Eval, Expr, Fork, InstanceField, InstanceMethod, JavaClass, JavaConstructor, JavaInvocation, JavaLocal, JavaMethod, JavaNamedType, JavaParam, JavaPublic, JavaTClass, Join, Local, Loop, MethodInvocation, NewObject, Node, Null, Procedure, Program, RunMethod, Scope, Statement, TClass, TVeyMontChannel, TVoid, ThisChoreography, ThisObject, Type, UnitAccountedPredicate, Variable, VeyMontAssignExpression}
import vct.col.origin.{AssignLocalOk, Name, Origin, PanicBlame}
import vct.col.ref.Ref
import vct.col.resolve.ctx.RefJavaMethod
import vct.col.rewrite.adt.ImportADTImporter
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.SuccessionMap
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.{Unreachable, UserError}
import vct.rewrite.veymont.GenerateImplementation.{ChannelFieldOrigin, ParalleliseEndpointsError, RunMethodOrigin, ThreadClassOrigin, getChannelClassName, getThreadClassName, getVarName}

import scala.collection.mutable

object GenerateImplementation extends RewriterBuilder {
  override def key: String = "generateImplementation"

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

  private def RunMethodOrigin(runMethod: ChorRun[_]): Origin =
    runMethod.o.where(name = "run")
}

case class GenerateImplementation[Pre <: Generation]() extends Rewriter[Pre] with LazyLogging { outer =>

  val threadBuildingBlocks: ScopedStack[ThreadBuildingBlocks[Pre]] = ScopedStack()
  val threadClassSucc: SuccessionMap[Endpoint[Pre],Class[Post]] = SuccessionMap()
  val threadMethodSucc: SuccessionMap[(Endpoint[Pre],ClassDeclaration[Pre]),InstanceMethod[Post]] = SuccessionMap()
  val runSucc: mutable.LinkedHashMap[Choreography[Pre], Procedure[Post]] = mutable.LinkedHashMap()
  private val givenClassSucc: SuccessionMap[Type[Pre],Class[Post]] = SuccessionMap()
  private val givenClassConstrSucc: SuccessionMap[Type[Pre],Procedure[Pre]] = SuccessionMap()
  val endpointLocals: SuccessionMap[Endpoint[Pre], Variable[Post]] = SuccessionMap()

  // For each endpoint and input variable, there is a unique instance field (on the class of the endpoint)
  val endpointParamFields = SuccessionMap[(Endpoint[Pre], Variable[Pre]), InstanceField[Post]]()
  // For each endpoint and another endpoint, there is a unique instance field (on the class of the endpoint) with a reference to the second endpoint
  val endpointPeerFields = SuccessionMap[(Endpoint[Pre], Endpoint[Pre]), InstanceField[Post]]()

  var program: Program[Pre] = null
  lazy val choreographies: Seq[Choreography[Pre]] = program.declarations.collect { case chor: Choreography[Pre] => chor }
  lazy val allEndpoints = choreographies.flatMap { _.endpoints }
  lazy val endpointToChoreography: Map[Endpoint[Pre], Choreography[Pre]] =
    choreographies.flatMap { chor => chor.endpoints.map(ep => (ep, chor)) }.toMap
  lazy val endpointClassToEndpoint: Map[Class[Pre], Endpoint[Pre]] =
    choreographies.flatMap { chor => chor.endpoints.map(endpoint => (endpoint.cls.decl, endpoint)) }.toMap

  def isEndpointClass(c: Class[Pre]): Boolean = endpointClassToEndpoint.contains(c)
  def choreographyOf(c: Class[Pre]): Choreography[Pre] = endpointToChoreography(endpointClassToEndpoint(c))
  def endpointOf(c: Class[Pre]): Endpoint[Pre] = endpointClassToEndpoint(c)
  def isChoreographyParam(v: Variable[Pre]): Boolean = choreographies.exists { chor => chor.params.contains(v) }

  val currentChoreography = ScopedStack[Choreography[Pre]]()
  val currentEndpoint = ScopedStack[Endpoint[Pre]]()

  def inChoreography: Boolean = currentChoreography.nonEmpty && currentEndpoint.isEmpty
  def inEndpoint: Boolean = currentChoreography.nonEmpty && currentEndpoint.nonEmpty

  object InChor {
    def unapply[T](t: T): Option[(Choreography[Pre], T)] =
      if(inChoreography) Some((currentChoreography.top, t)) else None
    def unapply: Option[Choreography[Pre]] = if(inChoreography) currentChoreography.topOption else None
  }

  object InEndpoint {
    def unapply[T](t: T): Option[(Choreography[Pre], Endpoint[Pre], T)] =
      if(inEndpoint) Some((currentChoreography.top, currentEndpoint.top, t)) else None
    def unapply: Option[(Choreography[Pre], Endpoint[Pre])] = if(inEndpoint) Some((currentChoreography.top, currentEndpoint.top)) else None
  }

  val currentThis = ScopedStack[ThisObject[Post]]()

  override def dispatch(program: Program[Pre]): Program[Post] = {
    this.program = program
    super.dispatch(program)
  }

  override def dispatch(decl: Declaration[Pre]) : Unit = {
    decl match {
      case p: Procedure[Pre] => super.dispatch(p)
      case cls: Class[Pre] if isEndpointClass(cls) =>
        val chor = choreographyOf(cls)
        val endpoint = endpointOf(cls)
        currentThis.having(ThisObject[Post](succ(cls))(cls.o)) {
          globalDeclarations.succeed(cls, cls.rewrite(
            decls = classDeclarations.collect {
              cls.decls.foreach(dispatch)
              generateRunMethod(chor, endpointOf(cls))
              generateParamFields(chor, endpoint)
              generatePeerFields(chor, endpoint)
            }._1
          ))
        }
      case cls: Class[Pre] => super.dispatch(cls)
      case chor: Choreography[Pre] =>
        currentChoreography.having(chor) {
          chor.drop()
          chor.endpoints.foreach(_.drop())
          implicit val o = chor.o

          chor.endpoints.foreach(endpoint => endpointLocals(endpoint) = new Variable(dispatch(endpoint.t))(endpoint.o))

          val initEndpoints =
            chor.endpoints.map { endpoint =>
              assignLocal[Post](
                endpointLocals(endpoint).get,
                ConstructorInvocation[Post](
                  ref = succ(endpoint.constructor.decl),
                  classTypeArgs = endpoint.typeArgs.map(dispatch),
                  args = endpoint.args.map(dispatch),
                  outArgs = Seq(), typeArgs = Seq(), givenMap = Seq(), yields = Seq()
                )(PanicBlame("Should be safe"))
              )
            }

          // Initialize the fields on each endpoint class, representing the parameters of the choreography, and other endpoints
          val auxFieldAssigns = chor.endpoints.flatMap { endpoint =>
            chor.params.map { param =>
              assignField[Post](
                endpointLocals(endpoint).get,
                endpointParamFields.ref((endpoint, param)),
                Local(succ(param)),
                blame = PanicBlame("Should be safe")
              )
            } ++ chor.endpoints.map { peer =>
              assignField[Post](
                endpointLocals(endpoint).get,
                endpointPeerFields.ref((endpoint, peer)),
                endpointLocals(peer).get,
                blame = PanicBlame("Should be safe")
              )
            }
          }

          val forkJoins = chor.endpoints.map {
            endpoint => Fork[Post](endpointLocals(endpoint).get)(PanicBlame(""))
          } ++ chor.endpoints.map {
            endpoint => Join[Post](endpointLocals(endpoint).get)(PanicBlame(""))
          }

          val mainBody = Scope(
            chor.endpoints.map(endpointLocals(_)),
            Block(initEndpoints ++ auxFieldAssigns ++ forkJoins)(chor.o)
          )

          globalDeclarations.declare(procedure(
            args = variables.dispatch(chor.params),
            body = Some(mainBody),
            blame = PanicBlame("TODO: Procedure"),
            contractBlame = PanicBlame("TODO: Procedure contract"),
          )(chor.o))
        }
      case other => rewriteDefault(other)
    }
  }

  def generateRunMethod(chor: Choreography[Pre], endpoint: Endpoint[Pre]): Unit = {
    val run = chor.run
    implicit val o = run.o
    val body = currentChoreography.having(chor) {
      currentEndpoint.having(endpoint) {
        dispatch(run.body)
      }
    }
    classDeclarations.declare(new RunMethod(
      body = Some(body),
      contract = contract(PanicBlame("Trivial contract")) // , dispatch(run.contract)
    )(PanicBlame("")))
  }

  def generateParamFields(chor: Choreography[Pre], endpoint: Endpoint[Pre]): Unit =
    chor.params.foreach { param =>
      val f = new InstanceField(dispatch(param.t), Seq())(param.o.where(
        indirect = Name.names(chor.o.getPreferredNameOrElse(), Name("p"), param.o.getPreferredNameOrElse())
      ))
      classDeclarations.declare(f)
      endpointParamFields((endpoint, param)) = f
    }

  def generatePeerFields(chor: Choreography[Pre], endpoint: Endpoint[Pre]): Unit =
    chor.endpoints.foreach { peer =>
      val f = new InstanceField(dispatch(peer.t), Seq())(endpoint.o.where(
        indirect = Name.names(peer.o.getPreferredNameOrElse())
      ))
      classDeclarations.declare(f)
      endpointPeerFields((endpoint, peer)) = f
    }

  override def dispatch(statement: Statement[Pre]): Statement[Post] = {
    if (currentEndpoint.nonEmpty) projectStatement(statement)
    else super.dispatch(statement)
  }

  def projectExpression(guards: Seq[ChorGuard[Pre]])(implicit o: Origin): Expr[Post] = foldStar(guards.collect {
    case EndpointGuard(Ref(endpoint), cond) if endpoint == currentEndpoint.top => dispatch(cond)
  })

  def projectStatement(statement: Statement[Pre]): Statement[Post] = statement match {
    // Whitelist statements that do not need an endpoint context
    case ChorStatement(None, statement) => statement match {
      case _: Branch[Pre] | _: Loop[Pre] => projectStatement(statement)
      case _ => throw new Exception("Encountered ChorStatement without endpoint context")
    }
    case ChorStatement(Some(Ref(endpoint)), inner) if endpoint == currentEndpoint.top => inner match {
      case assign: Assign[Pre] => assign.rewriteDefault()
      case eval: Eval[Pre] => eval.rewriteDefault()
    }
    // Ignore statements that do not match the current endpoint
    case ChorStatement(_, _) => Block(Seq())(statement.o)
    case branch: ChorBranch[Pre] if branch.guards.map(_.endpointOpt.get).contains(currentEndpoint.top) =>
      implicit val o = branch.o
      Branch[Post](
        Seq((projectExpression(branch.guards)(branch.o), dispatch(branch.yes)))
        ++ branch.no.map(no => Seq((tt[Post], dispatch(no)))).getOrElse(Seq())
      )
    case chorLoop: ChorLoop[Pre] if chorLoop.guards.map(_.endpointOpt.get).contains(currentEndpoint.top) =>
      implicit val o = chorLoop.o
      loop(
        cond = projectExpression(chorLoop.guards)(chorLoop.o),
        body = dispatch(chorLoop.body),
        contract = dispatch(chorLoop.contract)
      )
    case _: ChorBranch[Pre] | _: ChorLoop[Pre] => Block(Seq())(statement.o)
    case block: Block[Pre] => block.rewriteDefault()
    case s =>
      throw new Exception(s"Unsupported: $s")
  }

  override def dispatch(expr: Expr[Pre]): Expr[Post] = expr match {
    case InChor(_, EndpointName(Ref(endpoint)))=>
      Local[Post](endpointLocals.ref(endpoint))(expr.o)
    case InEndpoint(_, endpoint, EndpointName(Ref(peer))) =>
      implicit val o = expr.o
      // TODO (RR): Also need to generate (read) permissions for all these fields!
      Deref[Post](currentThis.top, endpointPeerFields.ref((endpoint, peer)))(PanicBlame("Shouldn't happen"))
    case InEndpoint(_, endpoint, Local(Ref(v))) if currentChoreography.nonEmpty && currentEndpoint.nonEmpty && isChoreographyParam(v) =>
      implicit val o = expr.o
      Deref[Post](currentThis.top, endpointParamFields.ref((endpoint, v)))(PanicBlame("Shouldn't happen"))
    case _ => expr.rewriteDefault()
  }

  //
  // Old code after this
  //

  private def dispatchThread(thread: Endpoint[Pre]): Unit = {
    if (threadBuildingBlocks.nonEmpty) {
      val threadRes: ThreadBuildingBlocks[Pre] = threadBuildingBlocks.top
      val threadMethods: Seq[ClassDeclaration[Post]] = createThreadMethod(thread, threadRes)
      createThreadClass(thread, threadRes, threadMethods)
    } else rewriteDefault(thread)
  }

  private def dispatchThreads(seqProg: Choreography[Pre]): Unit = {
    val (channelClasses,indexedChannelInfo) = extractChannelInfo(seqProg)
    channelClasses.foreach{ case (t,c) =>
      globalDeclarations.declare(c)
    }
    seqProg.endpoints.foreach(thread => {
      val threadField = new InstanceField[Post](TClass(givenClassSucc.ref(thread.t), Seq()), Nil)(thread.o)
      val channelFields = getChannelFields(thread, indexedChannelInfo, channelClasses)
      threadBuildingBlocks.having(new ThreadBuildingBlocks(seqProg.run, seqProg.decls, channelFields, channelClasses, thread, threadField)) {
        dispatch(thread)
      }
    })
  }

  private def dispatchGivenClass(c: Class[Pre]): Class[Post] = {
    val rw = GivenClassRewriter()
    val gc = c.rewrite(
      decls = classDeclarations.collect {
        (givenClassConstrSucc.get(TClass(c.ref, Seq())).get +: c.declarations).foreach(d => rw.dispatch(d))
      }._1
    )(rw)
    givenClassSucc.update(TClass(c.ref, Seq()),gc)
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
        val thisClassType = TClass(t.cls, Seq())
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

  private def extractChannelInfo(seqProg: Choreography[Pre]): (Map[Type[Pre], JavaClass[Post]], Seq[ChannelInfo[Pre]]) = {
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
      val threadClass = new Class[Post](Seq(),
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
      JavaInvocation[Post](None, Seq.empty, "new " + threadTypeName.ucamel, passedArgs, Seq.empty, Seq.empty)(null)(ThreadClassOrigin(thread)))(null)(ThreadClassOrigin(thread))
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
        val chanField = new InstanceField[Post](TVeyMontChannel(getChannelClassName(chanInfo.channelType)), Nil)(chanFieldOrigin)
        ((chanInfo.comExpr, chanInfo.comExpr.o), chanField)
      }.toMap
  }

  private def generateChannelClasses(channelInfo: Seq[ChannelInfo[Pre]]) : Map[Type[Pre],JavaClass[Post]] = {
    val channelTypes = channelInfo.map(_.channelType).toSet
    channelTypes.map(channelType =>
      channelType -> {
        val chanClassPre = (/* channelClass */ ???).asInstanceOf[JavaClass[Pre]]
        val rw = ChannelClassGenerator(channelType)
        chanClassPre.rewrite(
          name = getChannelClassName(channelType),
          modifiers = Seq.empty,
          decls = classDeclarations.collect {
            chanClassPre.decls.foreach(d => rw.dispatch(d))
          }._1
        )(rw)
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

  private def collectChannelsFromMethods(seqProg: Choreography[Pre]) =
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

  private def getThreadRunMethod(run: ChorRun[Pre]): InstanceMethod[Post] = {
    new InstanceMethod[Post](
      TVoid[Post](),
      Seq.empty,Seq.empty,Seq.empty,
      Some(dispatch(run.body)),
      dispatch(run.contract))(PanicBlame("TODO: Convert InstanceMethod blame to SeqRun blame")/* run.blame */)(RunMethodOrigin(run))
  }

  def dispatchExpr(node: Expr[Pre]): Expr[Post] = {
    if(threadBuildingBlocks.nonEmpty) {
      val thread = threadBuildingBlocks.top.thread
      node match {
        // TODO: Disabled this because the AST changed, repair
        // case c: SeqGuard[Pre] => paralleliseThreadCondition(node, thread, c)
        case m: MethodInvocation[Pre] => updateThreadRefMethodInvoc(thread, m)
        case d: Deref[Pre] => updateThreadRefInDeref(node, thread, d)
        case t: EndpointName[Pre] => updateThreadRefVeyMontDeref(node, thread, t)
        case _ => rewriteDefault(node)
      }
    } else rewriteDefault(node)
  }

  private def updateThreadRefVeyMontDeref(node: Expr[Pre], thread: Endpoint[Pre], t: EndpointName[Pre]) = {
    if (t.ref.decl == thread) {
      getThisVeyMontDeref(thread, t.o, threadBuildingBlocks.top.threadField)
    } else rewriteDefault(node)
  }

  private def updateThreadRefInDeref(node: Expr[Pre], thread: Endpoint[Pre], d: Deref[Pre]) = {
    d.obj match {
      case t: EndpointName[Pre] if t.ref.decl == thread =>
        d.rewrite(
          obj = getThisVeyMontDeref(thread, d.o, threadBuildingBlocks.top.threadField)
        )
      case _ => rewriteDefault(node)
    }
  }

  private def updateThreadRefMethodInvoc(thread: Endpoint[Pre], m: MethodInvocation[Pre]) = {
    m.obj match {
      case threadRef: EndpointName[Pre] => ??? // m.rewrite(obj = EndpointNameExpr(dispatch(threadRef)))
      case _ => threadMethodSucc.get((thread, m.ref.decl)) match {
        case Some(postMethod) => m.rewrite(obj = dispatch(m.obj), ref = postMethod.ref[InstanceMethod[Post]], m.args.map(dispatch))
        case None => throw ParalleliseEndpointsError(m, "No successor for this method found")
      }
    }
  }

  private def paralleliseThreadCondition(node: Expr[Pre], thread: Endpoint[Pre], c: ChorGuard[Pre]) = {
    ???
    // TODO: Broke this because AST changed, repair
//    c.conditions.find { case (threadRef, _) =>
//      threadRef.decl == thread
//    } match {
//      case Some((_, threadExpr)) => dispatch(threadExpr)
//      case _ => throw ParalleliseEndpointsError(node, "Condition of if statement or while loop must contain an expression for every thread")
//    }
  }

  private def getThisVeyMontDeref(thread: Endpoint[Pre], o: Origin, threadField: InstanceField[Rewritten[Pre]]) = {
    Deref(ThisObject(threadClassSucc.ref[Post, Class[Post]](thread))(thread.o), threadField.ref[InstanceField[Post]])(null)(o)
  }

  def dispatchStatement(st : Statement[Pre]) : Statement[Post] = {
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
        case _: ThisChoreography[Pre] => Eval(m.rewrite(obj = ThisObject(threadClassSucc.ref[Post, Class[Post]](thread))(thread.o), ref = threadMethodSucc.ref[Post, InstanceMethod[Post]]((thread, m.ref.decl))))(st.o)
//        case EndpointNameExpr(d: EndpointName[Pre]) => ??? // if (d.ref.decl == thread) Eval(dispatch(expr))(st.o) else Block(Seq.empty)(st.o)
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
