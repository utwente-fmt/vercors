package vct.rewrite.veymont.generation

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast.{
  And,
  TVoid,
  ApplicableContract,
  Assert,
  Assign,
  Assume,
  Block,
  Branch,
  ChorPerm,
  ChorRun,
  ChorStatement,
  Choreography,
  Class,
  ClassDeclaration,
  Constructor,
  Declaration,
  Deref,
  Endpoint,
  EndpointExpr,
  EndpointName,
  EndpointStatement,
  Eval,
  Exhale,
  Expr,
  FieldLocation,
  Fork,
  IdleToken,
  Inhale,
  InstanceField,
  InstanceMethod,
  IterationContract,
  Join,
  Local,
  Location,
  Loop,
  LoopContract,
  LoopInvariant,
  Node,
  Or,
  Perm,
  Procedure,
  Program,
  RunMethod,
  Scope,
  Statement,
  ThisObject,
  Type,
  Value,
  Variable,
  WritePerm,
}
import vct.col.origin.{Name, Origin, PanicBlame, PostBlameSplit}
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap
import vct.result.Message
import vct.result.VerificationError.SystemError
import vct.rewrite.veymont.VeymontContext
import vct.rewrite.veymont.generation.GenerateImplementation.CannotProjectStatement

import scala.collection.mutable

object GenerateImplementation extends RewriterBuilder {
  override def key: String = "generateImplementation"

  override def desc: String =
    "Generate classes for VeyMont threads in parallel program"

  case class CannotProjectStatement(endpoint: Endpoint[_], s: Statement[_])
      extends SystemError {
    override def text: String = {
      Message.messagesInContext(
        (
          s.o,
          s"The following statement was not transformed into a supported statement...",
        ),
        (endpoint.o, "... while projecting for this endpoint."),
      )
    }
  }
}

case class GenerateImplementation[Pre <: Generation]()
    extends Rewriter[Pre] with VeymontContext[Pre] with LazyLogging {
  outer =>

  val runSucc: mutable.LinkedHashMap[Choreography[Pre], Procedure[Post]] =
    mutable.LinkedHashMap()
  private val givenClassSucc: SuccessionMap[Type[Pre], Class[Post]] =
    SuccessionMap()
  private val givenClassConstrSucc: SuccessionMap[Type[Pre], Procedure[Pre]] =
    SuccessionMap()
  val endpointLocals: SuccessionMap[Endpoint[Pre], Variable[Post]] =
    SuccessionMap()

  // For each endpoint and input variable, there is a unique instance field (on the class of the endpoint)
  val endpointParamFields =
    SuccessionMap[(Endpoint[Pre], Variable[Pre]), InstanceField[Post]]()
  // For each endpoint and another endpoint, there is a unique instance field (on the class of the endpoint)
  // with a reference to the second endpoint
  val endpointPeerFields =
    SuccessionMap[(Endpoint[Pre], Endpoint[Pre]), InstanceField[Post]]()

  val currentThis = ScopedStack[ThisObject[Post]]()

  override def dispatch(p: Program[Pre]): Program[Post] = {
    mappings.program = p
    super.dispatch(p)
  }

  override def dispatch(decl: Declaration[Pre]): Unit = {
    decl match {
      case p: Procedure[Pre] => super.dispatch(p)
      case cls: Class[Pre] if isEndpointClass(cls) =>
        val chor = choreographyOf(cls)
        val endpoint = endpointOf(cls)
        currentThis.having(ThisObject[Post](succ(cls))(cls.o)) {
          cls.rewrite(decls =
            classDeclarations.collect {
              cls.decls.foreach(dispatch)
              generateRunMethod(chor, endpointOf(cls))
              generateParamFields(chor, endpoint)
              generatePeerFields(chor, endpoint)
            }._1
          ).succeed(cls)
        }

      case cons: Constructor[Pre] if isEndpointClass(classOf(cons)) =>
        implicit val o = cons.o
        cons.rewrite(
          contract = cons.contract.rewrite(ensures =
            (IdleToken[Post](ThisObject(succ(classOf(cons)))) &*
              endpointContext(endpointOf(classOf(cons)), perm = WritePerm()))
              .accounted &* dispatch(cons.contract.ensures)
          ),
          blame = PostBlameSplit
            .left(PanicBlame("Automatically generated permissions"), cons.blame),
        ).succeed(cons)

      case cls: Class[Pre] => super.dispatch(cls)
      case chor: Choreography[Pre] =>
        currentChoreography.having(chor) {
          chor.drop()
          chor.endpoints.foreach(_.drop())
          implicit val o = chor.o

          chor.endpoints.foreach(endpoint =>
            endpointLocals(endpoint) =
              new Variable(dispatch(endpoint.t))(endpoint.o)
          )

          val initEndpoints = chor.endpoints.map { endpoint =>
            assignLocal[Post](
              endpointLocals(endpoint).get,
              dispatch(endpoint.init),
            )
          }

          // Initialize the fields on each endpoint class, representing the parameters of the choreography, and other endpoints
          val auxFieldAssigns = chor.endpoints.flatMap { endpoint =>
            chor.params.map { param =>
              assignField[Post](
                endpointLocals(endpoint).get,
                endpointParamFields.ref((endpoint, param)),
                Local(succ(param)),
                blame = PanicBlame("Should be safe"),
              )
            } ++ chor.endpoints.map { peer =>
              assignField[Post](
                endpointLocals(endpoint).get,
                endpointPeerFields.ref((endpoint, peer)),
                endpointLocals(peer).get,
                blame = PanicBlame("Should be safe"),
              )
            }
          }

          val forkJoins =
            chor.endpoints.map { endpoint =>
              Fork[Post](endpointLocals(endpoint).get)(PanicBlame(""))
            } ++ chor.endpoints.map { endpoint =>
              Join[Post](endpointLocals(endpoint).get)(PanicBlame(""))
            }

          val mainBody = Scope(
            chor.endpoints.map(endpointLocals(_)),
            Block(
              initEndpoints ++
                Seq(chor.preRun.map(dispatch).getOrElse(Block(Seq()))) ++
                auxFieldAssigns ++ forkJoins
            )(chor.o),
          )

          globalDeclarations.declare(
            new Procedure(
              contract = chor.contract.rewriteDefault(),
              returnType = TVoid[Post](),
              args = variables.dispatch(chor.params),
              body = Some(mainBody),
              outArgs = Seq(),
              typeArgs = Seq(),
            )(PanicBlame("TODO: Procedure"))(chor.o)
          )
        }
      case other => super.dispatch(other)
    }
  }

  def generateRunMethod(
      chor: Choreography[Pre],
      endpoint: Endpoint[Pre],
  ): Unit = {
    val run = chor.run
    implicit val o = run.o
    currentChoreography.having(chor) {
      currentEndpoint.having(endpoint) {
        classDeclarations.declare(
          new RunMethod(
            body = Some(projectStmt(run.body)(endpoint)),
            contract = projectContract(run.contract)(endpoint),
          )(PanicBlame(""))
        )
      }
    }
  }

  def generateParamFields(
      chor: Choreography[Pre],
      endpoint: Endpoint[Pre],
  ): Unit =
    chor.params.foreach { param =>
      val f =
        new InstanceField(dispatch(param.t), Seq())(param.o.where(indirect =
          Name.names(
            chor.o.getPreferredNameOrElse(),
            Name("p"),
            param.o.getPreferredNameOrElse(),
          )
        ))
      classDeclarations.declare(f)
      endpointParamFields((endpoint, param)) = f
    }

  def generatePeerFields(
      chor: Choreography[Pre],
      endpoint: Endpoint[Pre],
  ): Unit =
    chor.endpoints.foreach { peer =>
      val f =
        new InstanceField(dispatch(peer.t), Seq())(
          endpoint.o
            .where(indirect = Name.names(peer.o.getPreferredNameOrElse()))
        )
      classDeclarations.declare(f)
      endpointPeerFields((endpoint, peer)) = f
    }

  // All permissions that we want to be in scope within the run method of an endpoint:
  // the peer fields and param fields
  def endpointContext(
      endpoint: Endpoint[Pre],
      perm: Expr[Post] = null,
      value: Boolean = false,
  )(implicit o: Origin): Expr[Post] = {
    assert(value ^ (perm != null))

    def makePerm(loc: Location[Post]): Expr[Post] =
      if (perm != null)
        Perm(loc, perm)
      else
        Value(loc)

    val cls = endpoint.cls.decl
    val `this` = ThisObject[Post](succ(cls))
    val chor = choreographyOf(endpoint)
    val peerPerms = foldStar(chor.endpoints.map { peer =>
      val ref = endpointPeerFields
        .ref[Post, InstanceField[Post]]((endpoint, peer))
      makePerm(FieldLocation(`this`, ref))
    })

    val paramPerms = foldStar(chor.params.map { param =>
      val ref = endpointParamFields
        .ref[Post, InstanceField[Post]]((endpoint, param))
      makePerm(FieldLocation(`this`, ref))
    })

    peerPerms &* paramPerms
  }

  override def dispatch(statement: Statement[Pre]): Statement[Post] = {
    if (currentEndpoint.nonEmpty)
      projectStmt(statement)(currentEndpoint.top)
    else
      super.dispatch(statement)
  }

  override def dispatch(expr: Expr[Pre]): Expr[Post] =
    expr match {
      case InChor(_, EndpointName(Ref(endpoint))) =>
        Local[Post](endpointLocals.ref(endpoint))(expr.o)
      case InEndpoint(_, endpoint, EndpointName(Ref(peer))) =>
        implicit val o = expr.o
        Deref[Post](currentThis.top, endpointPeerFields.ref((endpoint, peer)))(
          PanicBlame("Shouldn't happen")
        )
      case InEndpoint(_, endpoint, Local(Ref(v)))
          if currentChoreography.nonEmpty && currentEndpoint.nonEmpty &&
            isChoreographyParam(v) =>
        implicit val o = expr.o
        Deref[Post](currentThis.top, endpointParamFields.ref((endpoint, v)))(
          PanicBlame("Shouldn't happen")
        )
      case InEndpoint(_, endpoint, expr) => projectExpr(expr)(endpoint)
      case _ => expr.rewriteDefault()
    }

  def projectStmt(
      statement: Statement[Pre]
  )(implicit endpoint: Endpoint[Pre]): Statement[Post] =
    statement match {
      case EndpointStatement(None, statement) =>
        statement match {
          case _ =>
            throw new Exception(
              "Encountered ChorStatement without endpoint context"
            )
        }
      case EndpointStatement(Some(Ref(other)), inner) if other == endpoint =>
        inner match {
          case assign: Assign[Pre] => assign.rewriteDefault()
          case eval: Eval[Pre] => eval.rewriteDefault()
        }
      // Ignore statements that do not match the current endpoint
      case EndpointStatement(_, _) => Block(Seq())(statement.o)
      // Specialize composite statements to the current endpoint
      case c @ ChorStatement(branch: Branch[Pre])
          if c.explicitEndpoints.contains(endpoint) =>
        implicit val o = branch.o
        Branch[Post](
          Seq((projectExpr(branch.cond), projectStmt(branch.yes))) ++
            branch.no.map(no => Seq((tt[Post], projectStmt(no))))
              .getOrElse(Seq())
        )
      case c @ ChorStatement(l: Loop[Pre])
          if c.explicitEndpoints.contains(endpoint) =>
        implicit val o = l.o
        loop(
          cond = projectExpr(l.cond),
          body = projectStmt(l.body),
          contract = projectContract(l.contract),
        )
      // Ignore loops, branches that the current endpoint doesn't participate in
      case c @ ChorStatement(_: Loop[Pre] | _: Branch[Pre]) => Block(Seq())(c.o)
      // Project assert-like statements as you'd expect
      case ChorStatement(assert: Assert[Pre]) =>
        assert.rewrite(res = projectExpr(assert.res))
      case ChorStatement(assume: Assume[Pre]) =>
        assume.rewrite(assn = projectExpr(assume.assn))
      case ChorStatement(inhale: Inhale[Pre]) =>
        inhale.rewrite(res = projectExpr(inhale.res))
      case ChorStatement(exhale: Exhale[Pre]) =>
        exhale.rewrite(res = projectExpr(exhale.res))
      // Rewrite blocks transparently
      case block: Block[Pre] => block.rewriteDefault()
      // Don't let any missed cases slip through
      case s => throw CannotProjectStatement(endpoint, s)
    }

  def projectContract(
      contract: LoopContract[Pre]
  )(implicit endpoint: Endpoint[Pre]): LoopContract[Post] = {
    implicit val o = contract.o
    contract match {
      case inv: LoopInvariant[Pre] =>
        inv.rewrite(invariant =
          endpointContext(endpoint, value = true) &* projectExpr(inv.invariant)
        )
      case it: IterationContract[Pre] =>
        it.rewrite(
          requires =
            endpointContext(endpoint, value = true) &* projectExpr(it.requires),
          ensures =
            endpointContext(endpoint, value = true) &* projectExpr(it.ensures),
        )
    }
  }

  def projectContract(
      contract: ApplicableContract[Pre]
  )(implicit endpoint: Endpoint[Pre]): ApplicableContract[Post] = {
    implicit val o = contract.o
    contract.rewrite(
      requires =
        endpointContext(endpoint, value = true).accounted &*
          mapPredicate(contract.requires, projectExpr),
      ensures =
        endpointContext(endpoint, value = true).accounted &*
          mapPredicate(contract.ensures, projectExpr),
      contextEverywhere = projectExpr(contract.contextEverywhere),
    )
  }

  def projectExpr(
      expr: Expr[Pre]
  )(implicit endpoint: Endpoint[Pre]): Expr[Post] =
    expr match {
      case ChorPerm(Ref(other), loc, perm) if endpoint == other =>
        Perm(dispatch(loc), dispatch(perm))(expr.o)
      case ChorPerm(Ref(other), _, _) if endpoint != other => tt
      case EndpointExpr(Ref(other), expr) if endpoint == other =>
        projectExpr(expr)
      case EndpointExpr(_, _) => tt
      // Define transparent projections for basic operators
      case and: And[Pre] =>
        and.rewrite(projectExpr(and.left), projectExpr(and.right))
      case or: Or[Pre] =>
        or.rewrite(projectExpr(or.left), projectExpr(or.right))
      // Actually this is kind of wrong and buggy. But it covers most default & correct cases so
      // I'll leave it for now
      case _ => expr.rewriteDefault()
    }

//  //
//  // Old code after this
//  //
//
//  private def dispatchThread(thread: Endpoint[Pre]): Unit = {
//    if (threadBuildingBlocks.nonEmpty) {
//      val threadRes: ThreadBuildingBlocks[Pre] = threadBuildingBlocks.top
//      val threadMethods: Seq[ClassDeclaration[Post]] = createThreadMethod(
//        thread,
//        threadRes,
//      )
//      createThreadClass(thread, threadRes, threadMethods)
//    } else
//      rewriteDefault(thread)
//  }
//
//  private def dispatchThreads(seqProg: Choreography[Pre]): Unit = {
//    val (channelClasses, indexedChannelInfo) = extractChannelInfo(seqProg)
//    channelClasses.foreach { case (t, c) => globalDeclarations.declare(c) }
//    seqProg.endpoints.foreach(thread => {
//      val threadField =
//        new InstanceField[Post](
//          TClass(givenClassSucc.ref(thread.t), Seq()),
//          Nil,
//        )(thread.o)
//      val channelFields = getChannelFields(
//        thread,
//        indexedChannelInfo,
//        channelClasses,
//      )
//      threadBuildingBlocks.having(new ThreadBuildingBlocks(
//        seqProg.run,
//        seqProg.decls,
//        channelFields,
//        channelClasses,
//        thread,
//        threadField,
//      )) { dispatch(thread) }
//    })
//  }
//
//  private def dispatchGivenClass(c: Class[Pre]): Class[Post] = {
//    val rw = GivenClassRewriter()
//    val gc =
//      c.rewrite(decls =
//        classDeclarations.collect {
//          (givenClassConstrSucc.get(TClass(c.ref, Seq())).get +: c.declarations)
//            .foreach(d => rw.dispatch(d))
//        }._1
//      )(rw)
//    givenClassSucc.update(TClass(c.ref, Seq()), gc)
//    gc
//  }
//
//  case class GivenClassRewriter() extends Rewriter[Pre] {
//    override val allScopes = outer.allScopes
//
//    val rewritingConstr: ScopedStack[(Seq[Variable[Pre]], TClass[Pre])] =
//      ScopedStack()
//
//    override def dispatch(decl: Declaration[Pre]): Unit =
//      decl match {
//        case p: Procedure[Pre] =>
//          p.returnType match {
//            case tc: TClass[Pre] =>
//              rewritingConstr.having(p.args, tc) {
//                classDeclarations.declare(createClassConstructor(p))
//              };
//            case _ =>
//              ??? // ("This procedure is expected to have a class as return type");
//          }
//        case other => rewriteDefault(other)
//      }
//
//    // PB: from what I understand this restores a constructor from a generated procedure, so this should be refactored
//    // once we make constructors first class.
//    def createClassConstructor(p: Procedure[Pre]): JavaConstructor[Post] =
//      new JavaConstructor[Post](
//        Seq(JavaPublic[Post]()(p.o)),
//        rewritingConstr.top._2.cls.decl.o.getPreferredNameOrElse().ucamel,
//        p.args.map(createJavaParam),
//        variables.dispatch(p.typeArgs),
//        Seq.empty,
//        p.body match {
//          case Some(s: Scope[Pre]) =>
//            s.body match {
//              case b: Block[Pre] =>
//                dispatch(Block(b.statements.tail.dropRight(1))(p.o))
//              case other => dispatch(other)
//            }
//          case Some(_) =>
//            throw Unreachable(
//              "The body of a procedure always starts with a Scope."
//            )
//          case None => Block(Seq.empty)(p.o)
//        },
//        p.contract.rewrite(ensures =
//          UnitAccountedPredicate[Post](BooleanValue(true)(p.o))(p.o)
//        ),
//      )(null)(p.o)
//
//    def createJavaParam(v: Variable[Pre]): JavaParam[Post] =
//      new JavaParam[Post](Seq.empty, getVarName(v).camel, dispatch(v.t))(v.o)
//
//    override def dispatch(e: Expr[Pre]): Expr[Post] =
//      e match {
//        case l: Local[Pre] =>
//          if (
//            rewritingConstr.nonEmpty &&
//            rewritingConstr.top._1.contains(l.ref.decl)
//          )
//            JavaLocal[Post](getVarName(l.ref.decl).camel)(null)(e.o)
//          else
//            rewriteDefault(l)
//        case t: ThisObject[Pre] =>
//          val thisClassType = TClass(t.cls, Seq())
//          if (
//            rewritingConstr.nonEmpty && rewritingConstr.top._2 == thisClassType
//          )
//            ThisObject(givenClassSucc.ref[Post, Class[Post]](thisClassType))(
//              t.o
//            )
//          else
//            rewriteDefault(t)
//        case d: Deref[Pre] =>
//          if (rewritingConstr.nonEmpty)
//            d.obj match {
//              case _: Local[Pre] =>
//                d.rewrite(obj =
//                  ThisObject(
//                    givenClassSucc
//                      .ref[Post, Class[Post]](rewritingConstr.top._2)
//                  )(d.o)
//                )
//              case other => rewriteDefault(other)
//            }
//          else
//            rewriteDefault(d)
//        case other => rewriteDefault(other)
//      }
//  }
//
//  private def extractChannelInfo(
//      seqProg: Choreography[Pre]
//  ): (Map[Type[Pre], JavaClass[Post]], Seq[ChannelInfo[Pre]]) = {
//    val channelInfo =
//      getChannelNamesAndTypes(seqProg.run.body) ++
//        collectChannelsFromMethods(seqProg)
//    val indexedChannelInfo: Seq[ChannelInfo[Pre]] =
//      channelInfo.groupBy(_.channelName).values.flatMap(chanInfoSeq =>
//        if (chanInfoSeq.size <= 1)
//          chanInfoSeq
//        else
//          chanInfoSeq.zipWithIndex.map { case (chanInfo, index) =>
//            new ChannelInfo(
//              chanInfo.comExpr,
//              chanInfo.channelType,
//              chanInfo.channelName + index,
//            )
//          }
//      ).toSeq
//    val channelClasses = generateChannelClasses(indexedChannelInfo)
//    (channelClasses, indexedChannelInfo)
//  }
//
//  private def createThreadMethod(
//      thread: Endpoint[Pre],
//      threadRes: ThreadBuildingBlocks[Pre],
//  ) = {
//    threadRes.methods.map { preMethod =>
//      val postMethod = getThreadMethodFromDecl(thread)(preMethod)
//      threadMethodSucc.update((thread, preMethod), postMethod)
//      postMethod
//    }
//  }
//
//  private def createThreadClass(
//      thread: Endpoint[Pre],
//      threadRes: ThreadBuildingBlocks[Pre],
//      threadMethods: Seq[ClassDeclaration[Post]],
//  ): Unit = {
//    val threadConstr = createThreadClassConstructor(
//      thread,
//      threadRes.threadField,
//    )
//    val threadRun = getThreadRunMethod(threadRes.runMethod)
//    classDeclarations.scope {
//      val threadClass =
//        new Class[Post](
//          Seq(),
//          (threadRes.threadField +: threadRes.channelFields.values.toSeq) ++
//            (threadConstr +: threadRun +: threadMethods),
//          Seq(),
//          BooleanValue(true)(thread.o),
//        )(ThreadClassOrigin(thread))
//      globalDeclarations.declare(threadClass)
//      threadClassSucc.update(thread, threadClass)
//    }
//  }
//
//  private def createThreadClassConstructor(
//      thread: Endpoint[Pre],
//      threadField: InstanceField[Post],
//  ): JavaConstructor[Post] = {
//    val threadConstrArgBlocks: Seq[(Name, Type[Post])] =
//      ??? /* thread.args.map {
//      case l: Local[Pre] =>
//        (l.ref.decl.o.getPreferredNameOrElse(), dispatch(l.t))
//      case other =>
//        throw ParalleliseEndpointsError(
//          other,
//          "This node is expected to be an argument of seq_prog, and have type Local",
//        )
//    } */
//    val threadConstrArgs: Seq[JavaParam[Post]] = threadConstrArgBlocks.map {
//      case (a, t) =>
//        new JavaParam[Post](Seq.empty, a.camel, t)(ThreadClassOrigin(thread))
//    }
//    val passedArgs = threadConstrArgs
//      .map(a => JavaLocal[Post](a.name)(null)(ThreadClassOrigin(thread)))
//    // TODO: The next check cannot fail anymore
//    val threadTypeName =
//      thread.t match { // TODO: replace by using givenClassSucc
//        case tc: TClass[Pre] => tc.cls.decl.o.getPreferredNameOrElse()
//        case _ =>
//          throw ParalleliseEndpointsError(
//            thread,
//            "This type is expected to be a class",
//          )
//      }
//    val threadConstrBody = {
//      Assign(
//        getThisVeyMontDeref(thread, ThreadClassOrigin(thread), threadField),
//        JavaInvocation[Post](
//          None,
//          Seq.empty,
//          "new " + threadTypeName.ucamel,
//          passedArgs,
//          Seq.empty,
//          Seq.empty,
//        )(null)(ThreadClassOrigin(thread)),
//      )(null)(ThreadClassOrigin(thread))
//    }
//    val threadConstrContract =
//      new ApplicableContract[Post](
//        UnitAccountedPredicate[Post](
//          BooleanValue(true)(ThreadClassOrigin(thread))
//        )(ThreadClassOrigin(thread)),
//        UnitAccountedPredicate[Post](
//          BooleanValue(true)(ThreadClassOrigin(thread))
//        )(ThreadClassOrigin(thread)),
//        BooleanValue(true)(ThreadClassOrigin(thread)),
//        Seq.empty,
//        Seq.empty,
//        Seq.empty,
//        None,
//      )(null)(ThreadClassOrigin(thread))
//    new JavaConstructor[Post](
//      Seq(JavaPublic[Post]()(ThreadClassOrigin(thread))),
//      getThreadClassName(thread),
//      threadConstrArgs,
//      Seq.empty,
//      Seq.empty,
//      threadConstrBody,
//      threadConstrContract,
//    )(ThreadClassOrigin(thread))(ThreadClassOrigin(thread))
//  }
//
//  private def getThreadMethodFromDecl(
//      thread: Endpoint[Pre]
//  )(decl: ClassDeclaration[Pre]): InstanceMethod[Post] =
//    decl match {
//      case m: InstanceMethod[Pre] => getThreadMethod(m)
//      case _ =>
//        throw ParalleliseEndpointsError(
//          thread,
//          "Methods of seq_program need to be of type InstanceMethod",
//        )
//    }
//
//  private def getChannelFields(
//      thread: Endpoint[Pre],
//      channelInfo: Seq[ChannelInfo[Pre]],
//      channelClasses: Map[Type[Pre], JavaClass[Post]],
//  ): Map[(CommunicateX[Pre], Origin), InstanceField[Post]] = {
//    channelInfo.filter(chanInfo =>
//      chanInfo.comExpr.receiver.decl == thread ||
//        chanInfo.comExpr.sender.decl == thread
//    ).map { chanInfo =>
//      val chanFieldOrigin = ChannelFieldOrigin(
//        chanInfo.channelName,
//        chanInfo.comExpr.assign,
//      )
//      val chanField =
//        new InstanceField[Post](
//          TVeyMontChannel(getChannelClassName(chanInfo.channelType)),
//          Nil,
//        )(chanFieldOrigin)
//      ((chanInfo.comExpr, chanInfo.comExpr.o), chanField)
//    }.toMap
//  }
//
//  private def generateChannelClasses(
//      channelInfo: Seq[ChannelInfo[Pre]]
//  ): Map[Type[Pre], JavaClass[Post]] = {
//    val channelTypes = channelInfo.map(_.channelType).toSet
//    channelTypes.map(channelType =>
//      channelType -> {
//        val chanClassPre = ( /* channelClass */ ???)
//          .asInstanceOf[JavaClass[Pre]]
//        val rw = ChannelClassGenerator(channelType)
//        chanClassPre.rewrite(
//          name = getChannelClassName(channelType),
//          modifiers = Seq.empty,
//          decls =
//            classDeclarations.collect {
//              chanClassPre.decls.foreach(d => rw.dispatch(d))
//            }._1,
//        )(rw)
//      }
//    ).toMap
//  }
//
//  case class ChannelClassGenerator(channelType: Type[_]) extends Rewriter[Pre] {
//    override val allScopes = outer.allScopes
//
//    override def dispatch(t: Type[Pre]): Type[Post] =
//      t match {
//        case jnt: JavaNamedType[Pre] =>
//          if (jnt.names.head._1 == "MessageType") {
//            dispatch(channelType.asInstanceOf[Type[Pre]])
//          } else
//            rewriteDefault(jnt)
//        case _ => rewriteDefault(t)
//      }
//
//    override def dispatch(decl: Declaration[Pre]): Unit =
//      decl match {
//        case jc: JavaConstructor[Pre] =>
//          classDeclarations
//            .declare(jc.rewrite(name = getChannelClassName(channelType)))
//        case other => rewriteDefault(other)
//      }
//  }
//
//  private def collectChannelsFromMethods(seqProg: Choreography[Pre]) =
//    seqProg.decls.flatMap {
//      case m: InstanceMethod[Pre] =>
//        m.body.map(getChannelNamesAndTypes).getOrElse(
//          throw ParalleliseEndpointsError(
//            m,
//            "Abstract methods are not supported inside a choreography.",
//          )
//        )
//      case other =>
//        throw ParalleliseEndpointsError(other, "choreography method expected")
//    }
//
//  private def getChannelNamesAndTypes(
//      s: Statement[Pre]
//  ): Seq[ChannelInfo[Pre]] = {
//    s.collect { case e @ CommunicateX(recv, sender, chanType, assign) =>
//      new ChannelInfo(
//        e,
//        chanType,
//        recv.decl.o.getPreferredNameOrElse().ucamel +
//          sender.decl.o.getPreferredNameOrElse().camel + "Channel",
//      )
//    }
//  }
//
//  private def getThreadMethod(
//      method: InstanceMethod[Pre]
//  ): InstanceMethod[Post] = {
//    new InstanceMethod[Post](
//      dispatch(method.returnType),
//      variables.dispatch(method.args),
//      variables.dispatch(method.outArgs),
//      variables.dispatch(method.typeArgs),
//      method.body.map(dispatch),
//      dispatch(method.contract),
//    )(method.blame)(method.o)
//  }
//
//  private def getThreadRunMethod(run: ChorRun[Pre]): InstanceMethod[Post] = {
//    new InstanceMethod[Post](
//      TVoid[Post](),
//      Seq.empty,
//      Seq.empty,
//      Seq.empty,
//      Some(dispatch(run.body)),
//      dispatch(run.contract),
//    )(PanicBlame(
//      "TODO: Convert InstanceMethod blame to SeqRun blame"
//    ) /* run.blame */ )(RunMethodOrigin(run))
//  }
//
//  def dispatchExpr(node: Expr[Pre]): Expr[Post] = {
//    if (threadBuildingBlocks.nonEmpty) {
//      val thread = threadBuildingBlocks.top.thread
//      node match {
//        // TODO: Disabled this because the AST changed, repair
//        // case c: SeqGuard[Pre] => paralleliseThreadCondition(node, thread, c)
//        case m: MethodInvocation[Pre] => updateThreadRefMethodInvoc(thread, m)
//        case d: Deref[Pre] => updateThreadRefInDeref(node, thread, d)
//        case t: EndpointName[Pre] =>
//          updateThreadRefVeyMontDeref(node, thread, t)
//        case _ => rewriteDefault(node)
//      }
//    } else
//      rewriteDefault(node)
//  }
//
//  private def updateThreadRefVeyMontDeref(
//      node: Expr[Pre],
//      thread: Endpoint[Pre],
//      t: EndpointName[Pre],
//  ) = {
//    if (t.ref.decl == thread) {
//      getThisVeyMontDeref(thread, t.o, threadBuildingBlocks.top.threadField)
//    } else
//      rewriteDefault(node)
//  }
//
//  private def updateThreadRefInDeref(
//      node: Expr[Pre],
//      thread: Endpoint[Pre],
//      d: Deref[Pre],
//  ) = {
//    d.obj match {
//      case t: EndpointName[Pre] if t.ref.decl == thread =>
//        d.rewrite(obj =
//          getThisVeyMontDeref(thread, d.o, threadBuildingBlocks.top.threadField)
//        )
//      case _ => rewriteDefault(node)
//    }
//  }
//
//  private def updateThreadRefMethodInvoc(
//      thread: Endpoint[Pre],
//      m: MethodInvocation[Pre],
//  ) = {
//    m.obj match {
//      case threadRef: EndpointName[Pre] =>
//        ??? // m.rewrite(obj = EndpointNameExpr(dispatch(threadRef)))
//      case _ =>
//        threadMethodSucc.get((thread, m.ref.decl)) match {
//          case Some(postMethod) =>
//            m.rewrite(
//              obj = dispatch(m.obj),
//              ref = postMethod.ref[InstanceMethod[Post]],
//              m.args.map(dispatch),
//            )
//          case None =>
//            throw ParalleliseEndpointsError(
//              m,
//              "No successor for this method found",
//            )
//        }
//    }
//  }
//
////  private def paralleliseThreadCondition(
////      node: Expr[Pre],
////      thread: Endpoint[Pre],
////      c: ChorGuard[Pre],
////  ) = {
////    ???
//  // TODO: Broke this because AST changed, repair
////    c.conditions.find { case (threadRef, _) =>
////      threadRef.decl == thread
////    } match {
////      case Some((_, threadExpr)) => dispatch(threadExpr)
////      case _ => throw ParalleliseEndpointsError(node, "Condition of if statement or while loop must contain an expression for every thread")
////    }
////  }
//
//  private def getThisVeyMontDeref(
//      thread: Endpoint[Pre],
//      o: Origin,
//      threadField: InstanceField[Rewritten[Pre]],
//  ) = {
//    Deref(
//      ThisObject(threadClassSucc.ref[Post, Class[Post]](thread))(thread.o),
//      threadField.ref[InstanceField[Post]],
//    )(null)(o)
//  }
//
//  def dispatchStatement(st: Statement[Pre]): Statement[Post] = {
//    if (threadBuildingBlocks.nonEmpty) {
//      val thread = threadBuildingBlocks.top.thread
//      st match {
//        case v: CommunicateX[Pre] =>
//          paralleliseVeyMontCommExpr(thread, v, createParComBlocks(thread, v))
//        case v @ VeyMontAssignExpression(threadRef, assign) =>
//          if (threadRef.decl == thread)
//            dispatch(assign)
//          else
//            Block(Seq.empty)(assign.o)
//        case a: Assign[Pre] =>
//          Assign(dispatch(a.target), dispatch(a.value))(a.blame)(a.o)
//        case Branch(_) => rewriteDefault(st)
//        case Loop(_, _, _, _, _) => rewriteDefault(st)
//        case Scope(_, _) => rewriteDefault(st)
//        case Block(_) => rewriteDefault(st)
//        case Eval(expr) => paralleliseMethodInvocation(st, thread, expr)
//        case _: Assert[Pre] => Block(Seq.empty)(st.o)
//        case _ =>
//          throw ParalleliseEndpointsError(
//            st,
//            "Statement not allowed in choreography",
//          )
//      }
//    } else
//      rewriteDefault(st)
//  }
//
//  private def createParComBlocks(
//      thread: Endpoint[Pre],
//      v: CommunicateX[Pre],
//  ): ParallelCommExprBuildingBlocks[Pre] = {
//    val channelField = threadBuildingBlocks.top.channelFields((v, v.o))
//    val channelClass = threadBuildingBlocks.top.channelClasses(v.chanType)
//    val thisChanField =
//      Deref(
//        ThisObject(threadClassSucc.ref[Post, Class[Post]](thread))(thread.o),
//        channelField.ref[InstanceField[Post]],
//      )(null)(v.assign.o)
//    val assignment = v.assign.asInstanceOf[Assign[Pre]]
//    new ParallelCommExprBuildingBlocks(
//      channelField,
//      channelClass,
//      thisChanField,
//      assignment,
//    )
//  }
//
//  private def paralleliseMethodInvocation(
//      st: Statement[Pre],
//      thread: Endpoint[Pre],
//      expr: Expr[Pre],
//  ): Statement[Post] = {
//    expr match {
//      case m: MethodInvocation[Pre] =>
//        m.obj match {
//          case _: ThisChoreography[Pre] =>
//            Eval(m.rewrite(
//              obj =
//                ThisObject(threadClassSucc.ref[Post, Class[Post]](thread))(
//                  thread.o
//                ),
//              ref = threadMethodSucc
//                .ref[Post, InstanceMethod[Post]]((thread, m.ref.decl)),
//            ))(st.o)
////        case EndpointNameExpr(d: EndpointName[Pre]) => ??? // if (d.ref.decl == thread) Eval(dispatch(expr))(st.o) else Block(Seq.empty)(st.o)
//          case _ =>
//            throw ParalleliseEndpointsError(
//              st,
//              "Statement not allowed in seq_program",
//            )
//        }
//      case _ =>
//        throw ParalleliseEndpointsError(
//          st,
//          "Statement not allowed in seq_program",
//        )
//    }
//  }
//
//  private def paralleliseVeyMontCommExpr(
//      thread: Endpoint[Pre],
//      v: CommunicateX[Pre],
//      blocks: ParallelCommExprBuildingBlocks[Pre],
//  ): Statement[Post] = {
//    if (v.receiver.decl == thread) {
//      val readMethod = findChannelClassMethod(
//        v,
//        blocks.channelClass,
//        "readValue",
//      )
//      val assignValue =
//        JavaInvocation(
//          Some(blocks.thisChanField),
//          Seq.empty,
//          "readValue",
//          Seq.empty,
//          Seq.empty,
//          Seq.empty,
//        )(null)(v.o)
//      assignValue.ref = Some(RefJavaMethod(readMethod))
//      Assign(dispatch(blocks.assign.target), assignValue)(null)(v.o)
//    } else if (v.sender.decl == thread) {
//      val writeMethod = findChannelClassMethod(
//        v,
//        blocks.channelClass,
//        "writeValue",
//      )
//      val writeInvoc =
//        JavaInvocation(
//          Some(blocks.thisChanField),
//          Seq.empty,
//          "writeValue",
//          Seq(dispatch(blocks.assign.value)),
//          Seq.empty,
//          Seq.empty,
//        )(null)(v.o)
//      writeInvoc.ref = Some(RefJavaMethod(writeMethod))
//      Eval(writeInvoc)(v.o)
//    } else
//      Block(Seq.empty)(blocks.assign.o)
//  }
//
//  private def findChannelClassMethod(
//      v: CommunicateX[Pre],
//      channelClass: JavaClass[Post],
//      methodName: String,
//  ): JavaMethod[Post] = {
//    val method = channelClass.decls.find {
//      case jm: JavaMethod[Post] => jm.name == methodName
//      case _ => false
//    }
//    method match {
//      case Some(m: JavaMethod[Post]) => m
//      case _ =>
//        throw ParalleliseEndpointsError(
//          v,
//          "Could not find method `" + methodName + "' for channel class " +
//            channelClass.name,
//        )
//    }
//  }
}
