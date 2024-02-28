package vct.rewrite.veymont

import hre.util.ScopedStack
import vct.col.ast.{AbstractRewriter, ApplicableContract, Assert, Assign, Block, BooleanValue, Branch, Class, ClassDeclaration, Communicate, CommunicateX, Declaration, Deref, Endpoint, EndpointUse, Eval, Expr, InstanceField, InstanceMethod, JavaClass, JavaConstructor, JavaInvocation, JavaLocal, JavaMethod, JavaNamedType, JavaParam, JavaPublic, JavaTClass, Local, Loop, MethodInvocation, NewObject, Node, Procedure, Program, RunMethod, Scope, SeqGuard, SeqProg, SeqRun, Statement, TClass, TVeyMontChannel, TVoid, ThisObject, ThisSeqProg, Type, UnitAccountedPredicate, Variable, VeyMontAssignExpression}
import vct.col.origin.{Origin, PanicBlame}
import vct.col.resolve.ctx.RefJavaMethod
import vct.col.rewrite.adt.{ImportADT, ImportADTImporter}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, RewriterBuilderArg, Rewritten}
import vct.col.util.SuccessionMap
import vct.result.VerificationError.{Unreachable, UserError}

object EncodeChannels extends RewriterBuilderArg[ImportADTImporter] {
  override def key: String = "encodeChannels"
  override def desc: String = "Encodes VeyMont channels as fields on endpoints, and communicate statements as method invocations on endpoints."
}

case class EncodeChannels[Pre <: Generation](importer: ImportADTImporter) extends ImportADT[Pre](importer) {
  private lazy val channelFile = parse("genericChannel")

  private lazy val genericChannelClass = find[Class[Post]](channelFile, "Channel")
  private lazy val genericWrite = find[InstanceMethod[Post]](channelFile, "writeValue")
  private lazy val genericRead = find[InstanceMethod[Post]](channelFile, "readValue")

  def channelType(t: Type[Post]): TClass[Post] = TClass[Post](genericChannelClass.ref, Seq(t))

  var program: Program[Pre] = null
  lazy val choreographies: Seq[SeqProg[Pre]] = program.collect { case c: SeqProg[Pre] => c }.toIndexedSeq
  lazy val communicates: Map[SeqProg[Pre], Seq[Communicate[Pre]]] = choreographies.map { c =>
    (c, c.collect { case comm: Communicate[Pre] => comm }.toIndexedSeq)
  }.toMap

  override def postCoerce(p: Program[Pre]): Program[Post] = {
    program = p
    super.postCoerce(p)
  }

  // probably don't need to transform the seqprog itself?
  override def postCoerce(decl: Declaration[Pre]): Unit = decl match {
//    case prog: SeqProg[Pre] =>
//      val endpoints = prog.endpoints
//      val communicates = prog.collect { case comm: Communicate[Pre] => comm }
//
//      ???
    case endpoint: Endpoint[Pre] =>
      // Replace with endpoint with specialized class. Has members: impl of old type, and field for each communciate
      // leading to channel of proper type
      ???
    case _ =>
      decl.rewriteDefault()
      ??? // Succeed?
  }

  override def postCoerce(stmt: Statement[Pre]): Statement[Post] = stmt match {
    case comm: Communicate[Pre] =>
      implicit val o = comm.o
      Block[Post](Seq(
        Eval(???), // Send
        Assign(???, ???)(???), // Receive
      ))(comm.o)
    case _ => stmt.rewriteDefault()
  }

  override def postCoerce(expr: Expr[Pre]): Expr[Post] = expr match {
    case EndpointUse(_) => ???
    case _ => expr.rewriteDefault()
  }
}
