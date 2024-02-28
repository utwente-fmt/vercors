package vct.rewrite.veymont

import hre.util.ScopedStack
import vct.col.ast.{AbstractRewriter, ApplicableContract, Assert, Assign, Block, BooleanValue, Branch, Class, ClassDeclaration, CommunicateX, Declaration, Deref, Endpoint, EndpointUse, Eval, Expr, InstanceField, InstanceMethod, JavaClass, JavaConstructor, JavaInvocation, JavaLocal, JavaMethod, JavaNamedType, JavaParam, JavaPublic, JavaTClass, Local, Loop, MethodInvocation, NewObject, Node, Procedure, Program, RunMethod, Scope, SeqGuard, SeqProg, SeqRun, Statement, TClass, TVeyMontChannel, TVoid, ThisObject, ThisSeqProg, Type, UnitAccountedPredicate, Variable, VeyMontAssignExpression}
import vct.col.origin.{Origin, PanicBlame}
import vct.col.resolve.ctx.RefJavaMethod
import vct.col.rewrite.adt.{ImportADTImporter}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, RewriterBuilderArg, Rewritten}
import vct.col.util.SuccessionMap
import vct.result.VerificationError.{Unreachable, UserError}

object EncodeChannels extends RewriterBuilderArg[ImportADTImporter] {
  override def key: String = "encodeChannels"
  override def desc: String = "Encodes VeyMont channels as fields on endpoints, and communicate statements as method invocations on endpoints."
}

case class EncodeChannels[Pre <: Generation](importer: ImportADTImporter) extends Rewriter[Pre] {
  override def dispatch(p: Program[Pre]): Program[Post] = {
    ???
  }
}
