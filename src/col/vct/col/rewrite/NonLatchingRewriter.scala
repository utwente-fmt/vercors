package vct.col.rewrite

import vct.col.ast._
import vct.col.util.CurrentRewriteProgramContext
import vct.result.VerificationError

class NonLatchingRewriter[Pre, Post]() extends AbstractRewriter[Pre, Post] {
  override def dispatch(context: Verification[Pre]): Verification[Post] = rewriteDefault(context)
  override def dispatch(context: VerificationContext[Pre]): VerificationContext[Post] = rewriteDefault(context)
  override def dispatch(program: Program[Pre]): Program[Post] =
    VerificationError.withContext(CurrentRewriteProgramContext(program)) {
      rewriteDefault(program)
    }

  override def dispatch(stat: Statement[Pre]): Statement[Post] = rewriteDefault(stat)
  override def dispatch(e: Expr[Pre]): Expr[Post] = rewriteDefault(e)
  override def dispatch(t: Type[Pre]): Type[Post] = rewriteDefault(t)
  override def dispatch(decl: Declaration[Pre]): Unit = rewriteDefault(decl)

  override def dispatch(node: DecreasesClause[Pre]): DecreasesClause[Post] = rewriteDefault(node)
  override def dispatch(node: AccountedPredicate[Pre]): AccountedPredicate[Post] = rewriteDefault(node)
  override def dispatch(node: ApplicableContract[Pre]): ApplicableContract[Post] = rewriteDefault(node)
  override def dispatch(node: GpuMemoryFence[Pre]): GpuMemoryFence[Post] = rewriteDefault(node)
  override def dispatch(node: LoopContract[Pre]): LoopContract[Post] = rewriteDefault(node)

  override def dispatch(parRegion: ParRegion[Pre]): ParRegion[Post] = rewriteDefault(parRegion)
  override def dispatch(catchClause: CatchClause[Pre]): CatchClause[Post] = rewriteDefault(catchClause)
  override def dispatch(node: SignalsClause[Pre]): SignalsClause[Post] = rewriteDefault(node)
  override def dispatch(fieldFlag: FieldFlag[Pre]): FieldFlag[Post] = rewriteDefault(fieldFlag)
  override def dispatch(iterVariable: IterVariable[Pre]): IterVariable[Post] = rewriteDefault(iterVariable)
  override def dispatch(location: Location[Pre]): Location[Post] = rewriteDefault(location)

  override def dispatch(language: ProverLanguage[Pre]): ProverLanguage[Post] = rewriteDefault(language)
  override def dispatch(func: SmtlibFunctionSymbol[Pre]): SmtlibFunctionSymbol[Post] = rewriteDefault(func)

  override def dispatch(node: CDeclarator[Pre]): CDeclarator[Post] = rewriteDefault(node)
  override def dispatch(cDeclSpec: CDeclarationSpecifier[Pre]): CDeclarationSpecifier[Post] = rewriteDefault(cDeclSpec)
  override def dispatch(node: CTypeQualifier[Pre]): CTypeQualifier[Post] = rewriteDefault(node)
  override def dispatch(node: CPointer[Pre]): CPointer[Post] = rewriteDefault(node)
  override def dispatch(node: CInit[Pre]): CInit[Post] = rewriteDefault(node)
  override def dispatch(node: CDeclaration[Pre]): CDeclaration[Post] = rewriteDefault(node)

  override def dispatch(node: CPPDeclarator[Pre]): CPPDeclarator[Post] = rewriteDefault(node)
  override def dispatch(node: CPPDeclarationSpecifier[Pre]): CPPDeclarationSpecifier[Post] = rewriteDefault(node)
  override def dispatch(node: CPPAddressing[Pre]): CPPAddressing[Post] = rewriteDefault(node)
  override def dispatch(node: CPPInit[Pre]): CPPInit[Post] = rewriteDefault(node)
  override def dispatch(node: CPPDeclaration[Pre]): CPPDeclaration[Post] = rewriteDefault(node)

  override def dispatch(node: JavaVariableDeclaration[Pre]): JavaVariableDeclaration[Post] = rewriteDefault(node)
  override def dispatch(node: JavaModifier[Pre]): JavaModifier[Post] = rewriteDefault(node)
  override def dispatch(node: JavaImport[Pre]): JavaImport[Post] = rewriteDefault(node)
  override def dispatch(node: JavaName[Pre]): JavaName[Post] = rewriteDefault(node)

  override def dispatch(node: JavaBipGlueName[Pre]): JavaBipGlueName[Post] = rewriteDefault(node)
  override def dispatch(node: JavaBipGlueElement[Pre]): JavaBipGlueElement[Post] = rewriteDefault(node)
  override def dispatch(node: BipGlueDataWire[Pre]): BipGlueDataWire[Post] = rewriteDefault(node)
  override def dispatch(node: BipGlueAccepts[Pre]): BipGlueAccepts[Post] = rewriteDefault(node)
  override def dispatch(node: BipGlueRequires[Pre]): BipGlueRequires[Post] = rewriteDefault(node)
  override def dispatch(node: BipPortType[Pre]): BipPortType[Post] = rewriteDefault(node)
  override def dispatch(node: BipTransitionSignature[Pre]): BipTransitionSignature[Post] = rewriteDefault(node)

  override def dispatch(node: Coercion[Pre]): Coercion[Post] = rewriteDefault(node)
  override def dispatch(node: Operator[Pre]): Operator[Post] = rewriteDefault(node)

  override def dispatch(node: LlvmFunctionContract[Pre]): LlvmFunctionContract[Post] = rewriteDefault(node)
  override def dispatch(node: LlvmLoopContract[Pre]): LlvmLoopContract[Post] = rewriteDefault(node)

  override def dispatch(node: PVLCommunicateAccess[Pre]): PVLCommunicateAccess[Post] = rewriteDefault(node)
  override def dispatch(node: PVLCommunicateSubject[Pre]): PVLCommunicateSubject[Post] = rewriteDefault(node)
  override def dispatch(node: SeqRun[Pre]): SeqRun[Post] = rewriteDefault(node)
  override def dispatch(node: Access[Pre]): Access[Post] = rewriteDefault(node)
  override def dispatch(node: Subject[Pre]): Subject[Post] = rewriteDefault(node)
}