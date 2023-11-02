package vct.rewrite.runtime.resolution

import vct.col.ast.{ADTDeclaration, AbstractRewriter, AccountedPredicate, AllScopes, ApplicableContract, BipGlueAccepts, BipGlueDataWire, BipGlueRequires, BipPortType, BipTransitionSignature, CDeclaration, CDeclarationSpecifier, CDeclarator, CInit, CLocalDeclaration, CPPAddressing, CPPDeclaration, CPPDeclarationSpecifier, CPPDeclarator, CPPInit, CPPLocalDeclaration, CPPParam, CParam, CPointer, CTypeQualifier, CatchClause, ClassDeclaration, Coercion, Declaration, DecreasesClause, Endpoint, EnumConstant, Expr, FieldFlag, GlobalDeclaration, GpuMemoryFence, IterVariable, JavaBipGlueElement, JavaBipGlueName, JavaImport, JavaLocal, JavaLocalDeclaration, JavaModifier, JavaName, JavaNewClass, JavaParam, JavaTClass, JavaVariableDeclaration, LabelDecl, LlvmFunctionContract, LlvmLoopContract, Location, LoopContract, ModelDeclaration, Operator, PVLCommunicateAccess, PVLCommunicateSubject, ParBlockDecl, ParInvariantDecl, ParRegion, Program, ProverLanguage, SendDecl, SignalsClause, SmtlibFunctionSymbol, Statement, SuccessorsProvider, Type, Variable, Verification, VerificationContext}
import vct.col.origin.{Blame, Origin, VerificationFailure}
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.Scopes

import scala.reflect.ClassTag

object FixResolution extends RewriterBuilder{
  override def key: String = "createArrayPermissions"

  override def desc: String = "Create permissions for items in arrays"
}


case class FixResolution[Pre<: Generation]() extends Rewriter[Pre]{


  override def rewriteDefault(node: Program[Pre]): Program[Rewritten[Pre]] = {
    //TODO: fix references being removed by making them the same as normal references using lazy ref

    val test = super.rewriteDefault(node)
    test

  }


}