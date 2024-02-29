package vct.rewrite.veymont

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast.util.Declarator
import vct.col.ast.{AbstractRewriter, Access, ApplicableContract, Assert, Assign, Block, BooleanValue, Branch, Class, ClassDeclaration, Communicate, CommunicateX, Constructor, ConstructorInvocation, Declaration, Deref, Endpoint, EndpointName, EndpointUse, Eval, Expr, GlobalDeclaration, InstanceField, InstanceMethod, JavaClass, JavaConstructor, JavaInvocation, JavaLocal, JavaMethod, JavaNamedType, JavaParam, JavaPublic, JavaTClass, Local, LocalDecl, Loop, MethodInvocation, NewObject, Node, Procedure, Program, RunMethod, Scope, SeqGuard, SeqProg, SeqRun, Statement, TClass, TVeyMontChannel, TVoid, ThisObject, ThisSeqProg, Type, UnitAccountedPredicate, Variable, VeyMontAssignExpression, WritePerm}
import vct.col.origin.{Origin, PanicBlame, SourceName}
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

case class EncodeChannels[Pre <: Generation](importer: ImportADTImporter) extends Rewriter[Pre] with LazyLogging {
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

  var program: Program[Pre] = null
  lazy val choreographies: Seq[SeqProg[Pre]] = program.collect { case c: SeqProg[Pre] => c }.toIndexedSeq
  lazy val communicates: Map[SeqProg[Pre], Seq[Communicate[Pre]]] = choreographies.map { c =>
    (c, c.collect { case comm: Communicate[Pre] => comm }.toIndexedSeq)
  }.toMap

  val currentChoreography = ScopedStack[SeqProg[Pre]]()

  val classOfEndpoint = SuccessionMap[Endpoint[Pre], Class[Post]]()
  val fieldOfCommunicate = SuccessionMap[(Endpoint[Pre], Communicate[Pre]), InstanceField[Post]]()
  val implFieldOfEndpoint = SuccessionMap[Endpoint[Pre], InstanceField[Post]]()
  val localOfCommunicate = mutable.LinkedHashMap[Communicate[Pre], Variable[Post]]()

  override def dispatch(p: Program[Pre]): Program[Post] = {
    program = p
    p.rewriteDefault()
  }
  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case prog: SeqProg[Pre] =>
      implicit val o = prog.o
      currentChoreography.having(prog) {
        def instantiateComm(comm: Communicate[Pre]): Seq[Statement[Post]] = {
          val t = channelType(dispatch(comm.msgType))
          val v = new Variable(t)
          localOfCommunicate(comm) = v
          Seq(
           LocalDecl(v),
           assignLocal(
             local = Local[Post](v.ref),
             value = ConstructorInvocation[Post](
               ref = genericConstructor.ref,
               classTypeArgs = Seq(dispatch(comm.msgType)),
               args = Seq(), outArgs = Seq(), typeArgs = Seq(), givenMap = Seq(), yields = Seq()
             )(PanicBlame("Should be safe"))
         ))
        }
        def assignComm(comm: Communicate[Pre], endpoint: Endpoint[Pre]): Statement[Post] = {
          assignField(
            obj = EndpointUse[Post](succ(endpoint)),
            field = fieldOfCommunicate.ref((endpoint, comm)),
            value = localOfCommunicate(comm).get,
            blame = PanicBlame("Should be safe")
          )
        }
        allScopes.anySucceed(prog, prog.rewrite(preRun = {
          val instantiatedComms: Seq[Statement[Post]] = communicates(prog).flatMap(instantiateComm)
          val assignComms: Seq[Statement[Post]] = prog.endpoints.flatMap { endpoint => communicates(prog).map { comm => assignComm(comm, endpoint) } }
          Some(Block(instantiatedComms ++ assignComms))
        }))
      }
    case endpoint: Endpoint[Pre] =>
      // Replace with endpoint with specialized class. Has members: impl of old type, and field for each communciate
      // leading to channel of proper type
      implicit val o = endpoint.o

      val commFields = communicates(currentChoreography.top).map { comm =>
        val EndpointName(Ref(sender)) = comm.sender.subject
        val EndpointName(Ref(receiver)) = comm.receiver.subject
        val f = new InstanceField[Post](channelType(dispatch(comm.msgType)), Seq())(
          o.where(name = s"${sender.o.debugName()}_${receiver.o.debugName()}")
        )
        fieldOfCommunicate((endpoint, comm)) = f
        f
      }
      val implField = new InstanceField[Post](dispatch(endpoint.t), Seq())
      implFieldOfEndpoint(endpoint) = implField

      val constructor: Constructor[Post] = {
        val implArg = new Variable(dispatch(endpoint.t))
        val `this` = new ThisObject[Post](classOfEndpoint.ref(endpoint))
        new Constructor[Post](
          cls = classOfEndpoint.ref(endpoint),
          args = Seq(implArg),
          contract = contract[Post](
            blame = PanicBlame("TODO"),
            ensures = UnitAccountedPredicate(
              foldStar[Post]((implField +: commFields).map { f => fieldPerm[Post](`this`, f.ref, WritePerm()) }) &*
                (Deref[Post](`this`, implField.ref)(PanicBlame("Deref cannot fail")) === implArg.get))
          ),
          outArgs = Seq(), typeArgs = Seq(), body = None
        )(PanicBlame("Postcondition cannot fail"))
      }

      val wrapperClass = new Class[Post](
        typeArgs = Seq(), supports = Seq(), intrinsicLockInvariant = tt,
        decls = Seq(
          implField,
          constructor
        ) ++ commFields,
      )(o.where(name = endpoint.o.debugName("Endpoint")))
      classOfEndpoint(endpoint) = wrapperClass
      globalDeclarations.declare(wrapperClass)

      allScopes.anySucceed(endpoint, endpoint.rewrite[Post](
        cls = wrapperClass.ref,
        typeArgs = Seq(),
        constructor = constructor.ref,
        args = Seq(constructorInvocation[Post](
          ref = constructor.ref,
          classTypeArgs = endpoint.typeArgs.map(dispatch),
          args = endpoint.args.map(dispatch),
          blame = PanicBlame("Not implemented")
        )),
        blame = PanicBlame("Unreachable")
      ))
    case _ =>
      super.dispatch(decl)
  }

  override def dispatch(stmt: Statement[Pre]): Statement[Post] = stmt match {
    case comm: Communicate[Pre] =>
      implicit val o = comm.o
      Block[Post](Seq(sendOf(comm), receiveOf(comm)))(comm.o)
    case _ => stmt.rewriteDefault()
  }

  def sendOf(comm: Communicate[Pre]): Eval[Post] = {
    implicit val o = comm.o
    val Access(EndpointName(Ref(sender)), Ref(field)) = comm.sender
    Eval(methodInvocation(
      obj = Deref(
        EndpointUse(succ[Endpoint[Post]](sender)),
        fieldOfCommunicate.ref[Post, InstanceField[Post]]((sender, comm)))(PanicBlame("Permission for fields should be propagated in entire choreography")),
      ref = genericWrite.ref[InstanceMethod[Post]],
      args = Seq(
        Deref[Post](
          Deref[Post](EndpointUse[Post](succ(sender)), implFieldOfEndpoint.ref(sender))(PanicBlame("Should be safe")),
          succ(field)
        )(PanicBlame("TODO: translate to blame from communicate"))
      ),
      blame = PanicBlame("TODO 1")
    ))
  }

  def receiveOf(comm: Communicate[Pre]): Assign[Post] = {
    implicit val o = comm.o
    val Access(EndpointName(Ref(receiver)), Ref(field)) = comm.receiver
    assignField[Post](
      obj = Deref[Post](EndpointUse[Post](succ(receiver)), implFieldOfEndpoint.ref(receiver))(PanicBlame("Should be safe")),
      field = succ(field),
      value = methodInvocation(
        obj = Deref[Post](EndpointUse(succ(receiver)), fieldOfCommunicate.ref((receiver, comm)))(PanicBlame("Should be safe")),
        ref = genericRead.ref,
        blame = PanicBlame("Should be safe"),
      ),
      blame = PanicBlame("TODO 2")
    )
  }

  override def dispatch(expr: Expr[Pre]): Expr[Post] = expr match {
    case use @ EndpointUse(Ref(endpoint)) =>
      implicit val o = use.o
      Deref[Post](use.rewriteDefault(), implFieldOfEndpoint.ref(endpoint))(PanicBlame("Should be safe"))
    case _ => expr.rewriteDefault()
  }
}
