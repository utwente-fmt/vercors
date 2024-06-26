package vct.rewrite.veymont

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast.{
  Assign,
  Block,
  ChorRun,
  EndpointStatement,
  Choreography,
  Class,
  Communicate,
  CommunicateStatement,
  Constructor,
  ConstructorInvocation,
  Declaration,
  Deref,
  Endpoint,
  EndpointName,
  Eval,
  Expr,
  FieldLocation,
  InstanceField,
  InstanceMethod,
  InstancePredicate,
  IterationContract,
  Local,
  LoopContract,
  LoopInvariant,
  Message,
  Program,
  Receiver,
  Result,
  Scope,
  Sender,
  Statement,
  TClass,
  TVar,
  ThisObject,
  Type,
  Value,
  Variable,
}
import vct.col.origin.{Name, Origin, PanicBlame, SourceName}
import vct.col.ref.Ref
import vct.col.rewrite.adt.ImportADTImporter
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilderArg}
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap

import scala.collection.mutable
import scala.reflect.ClassTag

object EncodeChannels extends RewriterBuilderArg[ImportADTImporter] {
  override def key: String = "encodeChannels"
  override def desc: String =
    "Encodes VeyMont channels as fields on endpoints, and communicate statements as method invocations on endpoints."
}

case class EncodeChannels[Pre <: Generation](importer: ImportADTImporter)
    extends Rewriter[Pre] with LazyLogging with VeymontContext[Pre] {

  private lazy val channelPre =
    importer.loadAdt[Pre]("genericChannel").declarations

  lazy val genericChannelClass = find[Pre, Class[Pre]](channelPre, "Channel")
  lazy val genericChannelDecls = genericChannelClass.decls
  lazy val genericChannelConstructor = find[Pre, Constructor[Pre]](
    genericChannelDecls
  )
  lazy val genericChannelWrite = find[Pre, InstanceMethod[Pre]](
    genericChannelDecls,
    "writeValue",
  )
  lazy val genericChannelRead = find[Pre, InstanceMethod[Pre]](
    genericChannelDecls,
    "readValue",
  )
  lazy val genericChannelHasMsg = find[Pre, InstanceField[Pre]](
    genericChannelDecls,
    "hasMsg",
  )
  lazy val genericChannelExchangeValue = find[Pre, InstanceField[Pre]](
    genericChannelDecls,
    "exchangeValue",
  )

  val channelClassSucc = SuccessionMap[Communicate[Pre], Class[Post]]()
  val channelConstructorSucc =
    SuccessionMap[Communicate[Pre], Constructor[Post]]()
  val channelWriteSucc = SuccessionMap[Communicate[Pre], InstanceMethod[Post]]()
  val channelReadSucc = SuccessionMap[Communicate[Pre], InstanceMethod[Post]]()
  val channelHasMsgSucc = SuccessionMap[Communicate[Pre], InstanceField[Post]]()
  val channelExchangeValueSucc =
    SuccessionMap[Communicate[Pre], InstanceField[Post]]()
  val senderFieldSucc = SuccessionMap[Communicate[Pre], InstanceField[Post]]()
  val receiverFieldSucc = SuccessionMap[Communicate[Pre], InstanceField[Post]]()
  val senderParamSucc = SuccessionMap[Communicate[Pre], Variable[Post]]()
  val receiverParamSucc = SuccessionMap[Communicate[Pre], Variable[Post]]()

  protected def find[G, T](decls: Seq[Declaration[G]], name: String = null)(
      implicit tag: ClassTag[T]
  ): T =
    decls.collectFirst {
      case decl: T with Declaration[G]
          if name == null ||
            decl.o.find[SourceName].contains(SourceName(name)) =>
        decl
    }.get

  val currentCommunicate = ScopedStack[Communicate[Pre]]()
  val currentMsgTVar = ScopedStack[Variable[Pre]]()
  val currentMsgExpr = ScopedStack[Expr[Post]]()

  val fieldOfCommunicate =
    SuccessionMap[(Endpoint[Pre], Communicate[Pre]), InstanceField[Post]]()
  val localOfCommunicate = mutable
    .LinkedHashMap[Communicate[Pre], Variable[Post]]()

  def channelType(comm: Communicate[Pre]): Type[Post] =
    TClass[Post](channelClassSucc.ref(comm), Seq())

  def generateChannel(comm: Communicate[Pre]): Unit =
    currentCommunicate.having(comm) { dispatch(genericChannelClass) }

  def channelName(comm: Communicate[_]): Name =
    Name.names(
      comm.sender.get.decl.o.getPreferredNameOrElse(),
      comm.receiver.get.decl.o.getPreferredNameOrElse(),
    )

  def senderField(
      implicit comm: Communicate[Pre]
  ): Ref[Post, InstanceField[Post]] = senderFieldSucc.ref(comm)
  def receiverField(
      implicit comm: Communicate[Pre]
  ): Ref[Post, InstanceField[Post]] = receiverFieldSucc.ref(comm)
  def thisSender(implicit comm: Communicate[Pre]): Expr[Post] =
    Deref(channelThis, senderField)(PanicBlame("Should be safe"))(comm.o)
  def thisReceiver(implicit comm: Communicate[Pre]): Expr[Post] =
    Deref(channelThis, receiverField)(PanicBlame("Should be safe"))(comm.o)
  def getSender(endpoint: Endpoint[Pre], comm: Communicate[Pre])(
      implicit o: Origin
  ): Deref[Post] =
    Deref[Post](
      Deref[Post](
        EndpointName(succ(endpoint)),
        fieldOfCommunicate.ref((endpoint, comm)),
      )(PanicBlame("Should be safe")),
      senderField(comm),
    )(PanicBlame("Should be safe"))
  def getReceiver(endpoint: Endpoint[Pre], comm: Communicate[Pre])(
      implicit o: Origin
  ): Deref[Post] =
    Deref[Post](
      Deref[Post](
        EndpointName(succ(endpoint)),
        fieldOfCommunicate.ref((endpoint, comm)),
      )(PanicBlame("Should be safe")),
      receiverField(comm),
    )(PanicBlame("Should be safe"))
  def thisHasMsg(implicit comm: Communicate[Pre]): Expr[Post] =
    Deref[Post](channelThis, channelHasMsgSucc.ref(comm))(PanicBlame(
      "Should be safe"
    ))(comm.o)
  def valueSender(implicit comm: Communicate[Pre]): Expr[Post] =
    value(channelThis, senderField)(comm.o)
  def valueReceiver(implicit comm: Communicate[Pre]): Expr[Post] =
    value(channelThis, receiverField)(comm.o)
  def channelThis(implicit comm: Communicate[Pre]): Expr[Post] =
    ThisObject[Post](channelClassSucc.ref(comm))(comm.o)
  def endpointComm(endpoint: Endpoint[Pre], comm: Communicate[Pre])(
      implicit o: Origin
  ): Deref[Post] =
    Deref[Post](
      EndpointName(succ(endpoint)),
      fieldOfCommunicate.ref((endpoint, comm)),
    )(PanicBlame("Shouldn't happen"))

  override def dispatch(p: Program[Pre]): Program[Post] = {
    mappings.program = p
    p.rewriteDefault()
  }

  override def dispatch(decl: Declaration[Pre]): Unit =
    decl match {
      case chor: Choreography[Pre] =>
        implicit val o = chor.o
        def commVar(comm: Communicate[Pre]): Variable[Post] = {
          val t = channelType(comm)
          val v = new Variable(t)(chor.o.where(indirect = channelName(comm)))
          localOfCommunicate(comm) = v
          v
        }

        def instantiateComm(comm: Communicate[Pre]): Statement[Post] = {
          val v = localOfCommunicate(comm)
          assignLocal(
            local = Local[Post](v.ref),
            value =
              ConstructorInvocation[Post](
                ref = channelConstructorSucc(comm).ref,
                args = Seq(
                  EndpointName(succ(comm.sender.get.decl)),
                  EndpointName(succ(comm.receiver.get.decl)),
                ),
                outArgs = Seq(),
                typeArgs = Seq(),
                givenMap = Seq(),
                yields = Seq(),
                classTypeArgs = Seq(),
              )(PanicBlame("Should be safe")),
          )
        }

        def assignComm(
            comm: Communicate[Pre],
            endpoint: Endpoint[Pre],
        ): Statement[Post] = {
          assignField(
            obj = EndpointName[Post](succ(endpoint)),
            field = fieldOfCommunicate.ref((endpoint, comm)),
            value = localOfCommunicate(comm).get,
            blame = PanicBlame("Should be safe"),
          )
        }

        communicatesOf(chor).foreach(generateChannel)

        currentChoreography.having(chor) {
          chor.rewrite(preRun = {
            val vars = communicatesOf(chor).map(commVar)
            val instantiatedComms: Seq[Statement[Post]] = communicatesOf(chor)
              .map(instantiateComm)
            val assignComms: Seq[Statement[Post]] = chor.endpoints
              .flatMap { endpoint =>
                communicatesOf(endpoint).map { comm =>
                  assignComm(comm, endpoint)
                }
              }
            Some(Scope(vars, Block(instantiatedComms ++ assignComms)))
          }).succeed(chor)
        }

      case cls: Class[Pre] if isEndpointClass(cls) =>
        cls.rewrite(decls =
          classDeclarations.collect {
            cls.decls.foreach(dispatch)
            communicatesOf(choreographyOf(cls)).foreach { comm =>
              val f =
                new InstanceField[Post](channelType(comm), Seq())(
                  comm.o.where(indirect = channelName(comm))
                )
              fieldOfCommunicate((endpointOf(cls), comm)) = f
              f.declare()
            }
          }._1
        ).succeed(cls)

      case cls: Class[Pre] if cls == genericChannelClass =>
        implicit val comm = currentCommunicate.top
        implicit val o = comm.o
        globalDeclarations.scope {
          classDeclarations.scope {
            variables.scope {
              currentMsgTVar.having(cls.typeArgs.head) {
                channelClassSucc(comm) = cls.rewrite[Post](
                  typeArgs = Seq(),
                  decls =
                    classDeclarations.collect {
                      senderFieldSucc(comm) = instanceField(
                        dispatch(comm.sender.get.decl.t)
                      )(o.where(name = "sender")).declare()
                      receiverFieldSucc(comm) = instanceField(
                        dispatch(comm.receiver.get.decl.t)
                      )(o.where(name = "receiver")).declare()
                      cls.decls.foreach(dispatch)
                    }._1,
                  intrinsicLockInvariant =
                    valueSender &* valueReceiver &*
                      dispatch(cls.intrinsicLockInvariant) &*
                      thisHasMsg ==> dispatch(comm.invariant),
                ).succeed(cls)
              }
            }
          }
        }

      case cons: Constructor[Pre] if cons == genericChannelConstructor =>
        implicit val comm = currentCommunicate.top
        implicit val o = comm.o
        val sender =
          new Variable(dispatch(comm.t.sender))(o.where(name = "sender"))
        val receiver =
          new Variable(dispatch(comm.t.receiver))(o.where(name = "receiver"))
        channelConstructorSucc(currentCommunicate.top) = cons.rewrite(
          args =
            variables.collect {
              cons.args.foreach(dispatch)
              senderParamSucc(comm) = sender.declare()
              receiverParamSucc(comm) = receiver.declare()
            }._1,
          body = Some(Block(
            Seq(
              Assign(thisSender, sender.get)(PanicBlame("Should be safe")),
              Assign(thisReceiver, receiver.get)(PanicBlame("Should be safe")),
            ) :+ cons.body.map(dispatch).getOrElse(skip)
          )),
          contract = cons.contract.rewrite(ensures =
            (valueSender &* valueReceiver &* (sender.get === thisSender) &*
              (receiver.get === thisReceiver)).accounted &*
              dispatch(cons.contract.ensures)
          ),
        ).succeed(cons)

      case m: InstanceMethod[Pre] if m == genericChannelWrite =>
        implicit val comm = currentCommunicate.top
        implicit val o = comm.o
        currentMsgExpr.having(Local(succ(m.args.head))) {
          channelWriteSucc(currentCommunicate.top) = m.rewrite(contract =
            m.contract.rewrite(requires =
              (valueSender &* valueReceiver &* dispatch(comm.invariant))
                .accounted &* dispatch(m.contract.requires)
            )
          ).succeed(m)
        }

      case m: InstanceMethod[Pre] if m == genericChannelRead =>
        implicit val comm = currentCommunicate.top
        implicit val o = comm.o
        currentMsgExpr.having(Result(succ(m))) {
          channelReadSucc(currentCommunicate.top) = m.rewrite[Post](contract =
            m.contract.rewrite(
              requires = (valueSender &* valueReceiver).accounted &*
                dispatch(m.contract.requires),
              ensures =
                (valueSender &* valueReceiver &* dispatch(comm.invariant))
                  .accounted &* dispatch(m.contract.requires),
            )
          ).succeed(m)
        }

      case f: InstanceField[Pre] if f == genericChannelHasMsg =>
        channelHasMsgSucc(currentCommunicate.top) = f.rewriteDefault()
          .succeed(f)
      case f: InstanceField[Pre] if f == genericChannelExchangeValue =>
        channelExchangeValueSucc(currentCommunicate.top) = f.rewriteDefault()
          .succeed(f)

      case _ => super.dispatch(decl)
    }

  override def dispatch(expr: Expr[Pre]): Expr[Post] =
    if (currentCommunicate.nonEmpty) {
      rewriteChannelExpr(currentCommunicate.top, expr)
    } else
      super.dispatch(expr)

  def rewriteChannelExpr(
      comm: Communicate[Pre],
      expr: Expr[Pre],
  ): Expr[Post] = {
    implicit val c = comm
    implicit val o = comm.o
    expr match {
      case Sender(_) => thisSender
      case Receiver(_) => thisReceiver
      case Message(_) if currentMsgExpr.nonEmpty => currentMsgExpr.top
      case Message(_) =>
        Deref[Post](channelThis, channelExchangeValueSucc.ref(comm))(PanicBlame(
          "Should be safe"
        ))
      case _ => expr.rewriteDefault()
    }
  }

  def channelContext(chor: Choreography[Pre])(implicit o: Origin): Expr[Post] =
    foldStar(chor.endpoints.flatMap { endpoint =>
      communicatesOf(endpoint).map { comm =>
        endpointComm(endpoint, comm).value &* getSender(endpoint, comm).value &*
          getReceiver(endpoint, comm).value &*
          (getSender(endpoint, comm) ===
            EndpointName(succ(comm.sender.get.decl))) &*
          (getReceiver(endpoint, comm) ===
            EndpointName(succ(comm.receiver.get.decl)))
      }
    })

  override def dispatch(run: ChorRun[Pre]): ChorRun[Post] =
    run.rewrite(contract =
      run.contract.rewrite(requires =
        (channelContext(choreographyOf(run))(run.o).accounted(run.o) &*
          dispatch(run.contract.requires))(run.o)
      )
    )

  override def dispatch(contract: LoopContract[Pre]): LoopContract[Post] =
    contract match {
      case InChor(chor, inv: LoopInvariant[Pre]) =>
        inv.rewrite(invariant =
          (channelContext(chor)(chor.o) &* dispatch(inv.invariant))(chor.o)
        )
      case InChor(chor, iter: IterationContract[Pre]) =>
        iter.rewrite(requires =
          (channelContext(chor)(chor.o) &* dispatch(iter.requires))(chor.o)
        )
      case _ => contract.rewriteDefault()
    }

  override def dispatch(t: Type[Pre]): Type[Post] =
    t match {
      case TVar(Ref(v)) if currentMsgTVar.topOption.contains(v) =>
        dispatch(currentCommunicate.top.msg.t)
      case _ => t.rewriteDefault()
    }

  override def dispatch(stmt: Statement[Pre]): Statement[Post] =
    stmt match {
      case CommunicateStatement(comm: Communicate[Pre]) =>
        implicit val o = comm.o
        Block[Post](Seq(sendOf(comm), receiveOf(comm)))(comm.o)
      case _ => stmt.rewriteDefault()
    }

  def sendOf(comm: Communicate[Pre]): Statement[Post] = {
    implicit val o = comm.o
    val Some(Ref(sender)) = comm.sender
    EndpointStatement[Post](
      Some(succ(sender)),
      Eval(methodInvocation[Post](
        obj =
          Deref[Post](
            EndpointName(succ(sender)),
            fieldOfCommunicate.ref[Post, InstanceField[Post]]((sender, comm)),
          )(PanicBlame(
            "Permission for fields should be propagated in entire choreography"
          )),
        ref = channelWriteSucc.ref[Post, InstanceMethod[Post]](comm),
        args = Seq(dispatch(comm.msg)),
        blame = PanicBlame("TODO: sending should be safe"),
      )),
    )(PanicBlame("TODO: ChorStatement blame?"))
  }

  def receiveOf(comm: Communicate[Pre]): Statement[Post] = {
    implicit val o = comm.o
    val Some(Ref(receiver)) = comm.receiver
    EndpointStatement[Post](
      Some(succ[Endpoint[Post]](receiver)),
      Assign(
        dispatch(comm.target),
        methodInvocation[Post](
          obj =
            Deref[Post](
              EndpointName[Post](succ(receiver)),
              fieldOfCommunicate.ref((receiver, comm)),
            )(PanicBlame("Should be safe")),
          ref = channelReadSucc.ref[Post, InstanceMethod[Post]](comm),
          blame = PanicBlame("Should be safe"),
        ),
      )(PanicBlame("TODO 2")),
    )(PanicBlame("TODO: ChorStatement blame?"))
  }
}
