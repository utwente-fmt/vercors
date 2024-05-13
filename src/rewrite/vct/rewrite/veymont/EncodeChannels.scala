package vct.rewrite.veymont

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast.util.Declarator
import vct.col.ast.{AbstractRewriter, Access, ApplicableContract, Assert, Assign, Block, BooleanValue, Branch, ChorStatement, Class, ClassDeclaration, Communicate, CommunicateX, Constructor, ConstructorInvocation, Declaration, Deref, Endpoint, EndpointName, EndpointUse, Eval, Expr, GlobalDeclaration, InstanceField, InstanceMethod, JavaClass, JavaConstructor, JavaInvocation, JavaLocal, JavaMethod, JavaNamedType, JavaParam, JavaPublic, JavaTClass, Local, LocalDecl, Loop, MethodInvocation, NewObject, Node, Procedure, Program, RunMethod, Scope, ChorGuard, Choreography, ChorRun, Statement, TClass, TVeyMontChannel, TVoid, ThisObject, ThisChoreography, Type, UnitAccountedPredicate, Variable, VeyMontAssignExpression, WritePerm}
import vct.col.origin.{Name, Origin, PanicBlame, SourceName}
import vct.col.ref.Ref
import vct.col.resolve.ctx.RefJavaMethod
import vct.col.rewrite.adt.{ImportADT, ImportADTImporter}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, RewriterBuilderArg, Rewritten}
import vct.col.util.SuccessionMap
import vct.result.VerificationError.{Unreachable, UserError}
import vct.col.util.AstBuildHelpers._

import scala.collection.mutable
import scala.reflect.ClassTag

object EncodeChannels extends RewriterBuilderArg[ImportADTImporter] {
  override def key: String = "encodeChannels"
  override def desc: String = "Encodes VeyMont channels as fields on endpoints, and communicate statements as method invocations on endpoints."
}

case class EncodeChannels[Pre <: Generation](importer: ImportADTImporter) extends Rewriter[Pre] with LazyLogging with VeymontContext[Pre] {
  private lazy val channelFile = parse("genericChannel")

  private lazy val genericChannelClass = find[Class[Post]](channelFile, "Channel")
  private lazy val genericChannelDecls = genericChannelClass.decls
  private lazy val genericConstructor = find[Constructor[Post]](genericChannelDecls)
  private lazy val genericWrite = find[InstanceMethod[Post]](genericChannelDecls, "writeValue")
  private lazy val genericRead = find[InstanceMethod[Post]](genericChannelDecls, "readValue")

  protected def parse(name: String): Seq[GlobalDeclaration[Post]] = {
    val program = importer.loadAdt[Pre](name)
    program.declarations.foreach(dispatch)
    program.declarations.map(succProvider.computeSucc).map(_.get)
  }

  protected def find[T](decls: Seq[Declaration[Post]], name: String = null)(implicit tag: ClassTag[T]): T =
    decls.collectFirst {
      case decl: T if name == null || decl.o.find[SourceName].contains(SourceName(name)) =>
        decl
    }.get

  def channelType(t: Type[Post]): TClass[Post] = TClass[Post](genericChannelClass.ref, Seq(t))

  val fieldOfCommunicate = SuccessionMap[(Endpoint[Pre], Communicate[Pre]), InstanceField[Post]]()
  val localOfCommunicate = mutable.LinkedHashMap[Communicate[Pre], Variable[Post]]()

  override def dispatch(p: Program[Pre]): Program[Post] = {
    mappings.program = p
    p.rewriteDefault()
  }

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case chor: Choreography[Pre] =>
      implicit val o = chor.o
      currentChoreography.having(chor) {
        def commVar(comm: Communicate[Pre]): Variable[Post] = {
          val t = channelType(dispatch(comm.msgType))
          val v = new Variable(t)
          localOfCommunicate(comm) = v
          v
        }
        def instantiateComm(comm: Communicate[Pre]): Statement[Post] = {
          val v = localOfCommunicate(comm)
          assignLocal(
            local = Local[Post](v.ref),
            value = ConstructorInvocation[Post](
              ref = genericConstructor.ref,
              classTypeArgs = Seq(dispatch(comm.msgType)),
              args = Seq(), outArgs = Seq(), typeArgs = Seq(), givenMap = Seq(), yields = Seq()
            )(PanicBlame("Should be safe")))
        }
        def assignComm(comm: Communicate[Pre], endpoint: Endpoint[Pre]): Statement[Post] = {
          assignField(
            obj = EndpointUse[Post](succ(endpoint)),
            field = fieldOfCommunicate.ref((endpoint, comm)),
            value = localOfCommunicate(comm).get,
            blame = PanicBlame("Should be safe")
          )
        }
        allScopes.anySucceed(chor, chor.rewrite(preRun = {
          val vars = communicatesOf(chor).map(commVar)
          val instantiatedComms: Seq[Statement[Post]] = communicatesOf(chor).map(instantiateComm)
          val assignComms: Seq[Statement[Post]] = chor.endpoints.flatMap { endpoint => communicatesOf(chor).map { comm => assignComm(comm, endpoint) } }
          Some(Scope(vars, Block(instantiatedComms ++ assignComms)))
        }))
      }
    case cls: Class[Pre] if isEndpointClass(cls) =>
      val endpoint = endpointOf(cls)
      cls.rewrite(
        decls = classDeclarations.collect {
          cls.decls.foreach(dispatch)
          communicatesOf(currentChoreography.top).foreach { comm =>
            val EndpointName(Ref(sender)) = comm.sender.subject
            val EndpointName(Ref(receiver)) = comm.receiver.subject
            val f = new InstanceField[Post](channelType(dispatch(comm.msgType)), Seq())(
              comm.o.where(indirect = Name.names(sender.o.getPreferredNameOrElse(), receiver.o.getPreferredNameOrElse()))
            )
            fieldOfCommunicate((endpoint, comm)) = f
            f.declare()
          }
        }._1
      )

    case _ => super.dispatch(decl)
  }

  override def dispatch(stmt: Statement[Pre]): Statement[Post] = stmt match {
    case comm: Communicate[Pre] =>
      implicit val o = comm.o
      Block[Post](Seq(sendOf(comm), receiveOf(comm)))(comm.o)
    case _ => stmt.rewriteDefault()
  }

  def sendOf(comm: Communicate[Pre]): Statement[Post] = {
    implicit val o = comm.o
    /* TODO (RR): The Access here should be replaced with a general expression, as this will usually be a nested
        dereference. As soon as that refactor is done, this refactor here can be finished.
     */
    val Access(EndpointName(Ref(sender)), Ref(field)) = comm.sender
    ChorStatement[Post](Some(succ(sender)),
      Eval(methodInvocation(
        obj = Deref(
          EndpointUse[Post](succ(sender)),
          fieldOfCommunicate.ref[Post, InstanceField[Post]]((sender, comm)))(PanicBlame("Permission for fields should be propagated in entire choreography")),
        ref = genericWrite.ref[InstanceMethod[Post]],
        args = Seq(
          ???
//          Deref[Post](EndpointUse[Post](succ(sender)), succ(field))(PanicBlame("TODO: translate to blame from communicate"))
        ),
        blame = PanicBlame("TODO 1")
      ))
    )(PanicBlame("TODO: ChorStatement blame?"))
  }

  def receiveOf(comm: Communicate[Pre]): Statement[Post] = {
    implicit val o = comm.o
    val Access(EndpointName(Ref(receiver)), Ref(field)) = comm.receiver
    ChorStatement[Post](Some(succ(receiver)),
      assignField[Post](
        obj = ???, // Deref[Post](EndpointUse[Post](succ(receiver)), implFieldOfEndpoint.ref(receiver))(PanicBlame("Should be safe")),
        field = ???, // succ(field),
        value = methodInvocation(
          obj = Deref[Post](EndpointUse(succ(receiver)), fieldOfCommunicate.ref((receiver, comm)))(PanicBlame("Should be safe")),
          ref = genericRead.ref,
          blame = PanicBlame("Should be safe"),
        ),
        blame = PanicBlame("TODO 2")
      ))(PanicBlame("TODO: ChorStatement blame?"))
  }
}
