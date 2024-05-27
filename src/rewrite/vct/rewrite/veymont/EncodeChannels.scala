package vct.rewrite.veymont

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast.{Assign, Block, ChorStatement, Choreography, Class, Communicate, CommunicateStatement, Constructor, ConstructorInvocation, Declaration, Deref, Endpoint, EndpointName, Eval, InstanceField, InstanceMethod, Local, Program, Scope, Statement, TClass, TVar, Type, Variable}
import vct.col.origin.{Name, PanicBlame, SourceName}
import vct.col.ref.Ref
import vct.col.rewrite.adt.ImportADTImporter
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilderArg}
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap

import scala.collection.mutable
import scala.reflect.ClassTag

object EncodeChannels extends RewriterBuilderArg[ImportADTImporter] {
  override def key: String = "encodeChannels"
  override def desc: String = "Encodes VeyMont channels as fields on endpoints, and communicate statements as method invocations on endpoints."
}

case class EncodeChannels[Pre <: Generation](importer: ImportADTImporter) extends Rewriter[Pre] with LazyLogging with VeymontContext[Pre] {

  private lazy val channelPre = importer.loadAdt[Pre]("genericChannel").declarations

  lazy val genericChannelClass = find[Pre, Class[Pre]](channelPre, "Channel")
  lazy val genericChannelDecls = genericChannelClass.decls
  lazy val genericChannelConstructor = find[Pre, Constructor[Pre]](genericChannelDecls)
  lazy val genericChannelWrite = find[Pre, InstanceMethod[Pre]](genericChannelDecls, "writeValue")
  lazy val genericChannelRead = find[Pre, InstanceMethod[Pre]](genericChannelDecls, "readValue")

  val channelClassSucc = SuccessionMap[Communicate[Pre], Class[Post]]()
  val channelConstructorSucc = SuccessionMap[Communicate[Pre], Constructor[Post]]()
  val channelWriteSucc = SuccessionMap[Communicate[Pre], InstanceMethod[Post]]()
  val channelReadSucc = SuccessionMap[Communicate[Pre], InstanceMethod[Post]]()

  protected def find[G, T](decls: Seq[Declaration[G]], name: String = null)(implicit tag: ClassTag[T]): T =
    decls.collectFirst {
      case decl: T if name == null || decl.o.find[SourceName].contains(SourceName(name)) =>
        decl
    }.get

  def channelType(comm: Communicate[Pre]): Type[Post] =
    TClass[Post](channelClassSucc.ref(comm), Seq())

  val currentCommunicate = ScopedStack[Communicate[Pre]]()
  val currentMsgTVar = ScopedStack[Variable[Pre]]()

  def generateChannel(comm: Communicate[Pre]): Unit =
    currentCommunicate.having(comm) { dispatch(genericChannelClass) }

  val fieldOfCommunicate = SuccessionMap[(Endpoint[Pre], Communicate[Pre]), InstanceField[Post]]()
  val localOfCommunicate = mutable.LinkedHashMap[Communicate[Pre], Variable[Post]]()

  override def dispatch(p: Program[Pre]): Program[Post] = {
    mappings.program = p
    p.rewriteDefault()
  }

  def channelName(comm: Communicate[_]): Name =
    Name.names(comm.sender.get.decl.o.getPreferredNameOrElse(),
      comm.receiver.get.decl.o.getPreferredNameOrElse())

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case chor: Choreography[Pre] =>
      implicit val o = chor.o
      currentChoreography.having(chor) {
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
            value = ConstructorInvocation[Post](
              ref = channelConstructorSucc(comm).ref,
              classTypeArgs = Seq(dispatch(comm.msg.t)),
              args = Seq(), outArgs = Seq(), typeArgs = Seq(), givenMap = Seq(), yields = Seq()
            )(PanicBlame("Should be safe")))
        }

        def assignComm(comm: Communicate[Pre], endpoint: Endpoint[Pre]): Statement[Post] = {
          assignField(
            obj = EndpointName[Post](succ(endpoint)),
            field = fieldOfCommunicate.ref((endpoint, comm)),
            value = localOfCommunicate(comm).get,
            blame = PanicBlame("Should be safe")
          )
        }

        chor.rewrite(preRun = {
          communicatesOf(chor).foreach(generateChannel)
          val vars = communicatesOf(chor).map(commVar)
          val instantiatedComms: Seq[Statement[Post]] = communicatesOf(chor).map(instantiateComm)
          val assignComms: Seq[Statement[Post]] = chor.endpoints.flatMap { endpoint => communicatesOf(chor).map { comm => assignComm(comm, endpoint) } }
          Some(Scope(vars, Block(instantiatedComms ++ assignComms)))
        }).succeed(chor)
      }

    case cls: Class[Pre] if isEndpointClass(cls) =>
      cls.rewrite(
        decls = classDeclarations.collect {
          cls.decls.foreach(dispatch)
          communicatesOf(choreographyOf(cls)).foreach { comm =>
            val f = new InstanceField[Post](channelType(comm), Seq())(comm.o.where(indirect = channelName(comm)))
            fieldOfCommunicate((endpointOf(cls), comm)) = f
            f.declare()
          }
        }._1
      ).succeed(cls)

    case cls: Class[Pre] if cls == genericChannelClass =>
      globalDeclarations.scope {
        classDeclarations.scope {
          variables.scope {
            currentMsgTVar.having(cls.typeArgs.head) {
              channelClassSucc(currentCommunicate.top) = cls.rewrite(typeArgs = Seq()).succeed(cls)
            }
          }
        }
      }
    case cons: Constructor[Pre] if cons == genericChannelConstructor =>
      channelConstructorSucc(currentCommunicate.top) = cons.rewriteDefault().succeed(cons)
    case m: InstanceMethod[Pre] if m == genericChannelWrite =>
      channelWriteSucc(currentCommunicate.top) = m.rewriteDefault().succeed(m)
    case m: InstanceMethod[Pre] if m == genericChannelRead =>
      channelReadSucc(currentCommunicate.top) = m.rewriteDefault().succeed(m)

    case _ => super.dispatch(decl)
  }

  override def dispatch(t: Type[Pre]): Type[Post] = t match {
    case TVar(Ref(v)) if currentMsgTVar.topOption.contains(v) => dispatch(currentCommunicate.top.msg.t)
    case _ => t.rewriteDefault()
  }

  override def dispatch(stmt: Statement[Pre]): Statement[Post] = stmt match {
    case CommunicateStatement(comm: Communicate[Pre]) =>
      implicit val o = comm.o
      Block[Post](Seq(sendOf(comm), receiveOf(comm)))(comm.o)
    case _ => stmt.rewriteDefault()
  }

  def sendOf(comm: Communicate[Pre]): Statement[Post] = {
    implicit val o = comm.o
    val Some(Ref(sender)) = comm.sender
    ChorStatement[Post](
      Some(succ(sender)),
      Eval(methodInvocation[Post](
        obj = Deref[Post](
          EndpointName(succ(sender)),
          fieldOfCommunicate.ref[Post, InstanceField[Post]]((sender, comm)))(PanicBlame("Permission for fields should be propagated in entire choreography")),
        ref = channelWriteSucc.ref[Post, InstanceMethod[Post]](comm),
        args = Seq(dispatch(comm.msg)),
        blame = PanicBlame("TODO: sending should be safe")
      )))(PanicBlame("TODO: ChorStatement blame?"))
  }

  def receiveOf(comm: Communicate[Pre]): Statement[Post] = {
    implicit val o = comm.o
    val Some(Ref(receiver)) = comm.receiver
    ChorStatement[Post](Some(succ[Endpoint[Post]](receiver)),
      Assign(
        dispatch(comm.target),
        methodInvocation[Post](
          obj = Deref[Post](
            EndpointName[Post](succ(receiver)),
            fieldOfCommunicate.ref((receiver, comm)))(PanicBlame("Should be safe")),
          ref = channelReadSucc.ref[Post, InstanceMethod[Post]](comm),
          blame = PanicBlame("Should be safe")),
      )(PanicBlame("TODO 2"))
    )(PanicBlame("TODO: ChorStatement blame?"))
  }
}
