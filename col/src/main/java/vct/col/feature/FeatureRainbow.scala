package vct.col.feature

import vct.col.ast._
import vct.col.origin.DiagnosticOrigin
import vct.col.util.AstBuildHelpers._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class FeatureRainbow[G] {
  val features: mutable.Set[Feature] = mutable.Set()
  val examples: mutable.Map[Feature, ArrayBuffer[Node[G]]] = mutable.Map()

  private val topLevelInvocations: mutable.Set[Invocation[G]] = mutable.Set()

  def scan(node: Node[G]): Unit =
    node.transSubnodes.foreach(node => scanFlatly(node).foreach(f => {
      features += f
      examples.getOrElseUpdate(f, ArrayBuffer()) += node
    }))

  def scanFlatly(node: Node[G]): Seq[Feature] = Seq(node match {
    case node: Verification[G] => return Nil
    case node: VerificationContext[G] => return Nil
    case node: Program[G] => return Nil
    case node: IntegerValue[G] => return Nil
    case node: BooleanValue[G] => return Nil
    case node: LiteralSeq[G] => return Nil
    case node: LiteralSet[G] => return Nil
    case node: LiteralBag[G] => return Nil
    case node: LiteralTuple[G] => AxiomaticLibraryType
    case node: LiteralMap[G] => AxiomaticLibraryType
    case node: UntypedLiteralSeq[G] => DynamicallyTypedCollection
    case node: UntypedLiteralSet[G] => DynamicallyTypedCollection
    case node: UntypedLiteralBag[G] => DynamicallyTypedCollection
    case node: Void[G] => return Nil
    case node: AmbiguousThis[G] => ContextSensitiveNode
    case node: AmbiguousResult[G] => ContextSensitiveNode
    case node: ThisObject[G] => return Nil
    case node: ThisModel[G] => return Nil
    case node: Result[G] => return Nil
    case node: CurrentThreadId[G] => CurrentThread
    case node: Null[G] => return Nil
    case node: Any[G] => StarSubscript
    case node: NoPerm[G] => return Nil
    case node: ReadPerm[G] => WildcardReadPermission
    case node: WritePerm[G] => return Nil
    case node: Range[G] => SequenceRange
    case node: Values[G] => BuiltinArrayOperators
    case node: OptSome[G] => AxiomaticLibraryType
    case node: OptNone[G] => AxiomaticLibraryType
    case node: EitherLeft[G] => AxiomaticLibraryType
    case node: EitherRight[G] => AxiomaticLibraryType
    case node: MapCons[G] => AxiomaticLibraryType
    case node: MapEq[G] => AxiomaticLibraryType
    case node: MapDisjoint[G] => AxiomaticLibraryType
    case node: MapKeySet[G] => AxiomaticLibraryType
    case node: MapValueSet[G] => AxiomaticLibraryType
    case node: MapItemSet[G] => AxiomaticLibraryType
    case node: MapRemove[G] => AxiomaticLibraryType
    case node: Forall[G] => return Nil
    case node: Starall[G] => return Nil
    case node: Exists[G] => return Nil
    case node: Sum[G] => NumericReductionOperator
    case node: Product[G] => NumericReductionOperator
    case node: Let[G] => return Nil
    case node: InlinePattern[G] => InlineQuantifierPattern
    case node: Local[G] => return Nil
    case node: Deref[G] => Classes
    case node: ModelDeref[G] => Models
    case node: DerefPointer[G] => Pointers
    case node: AddrOf[G] => Pointers
    case node: PredicateApply[G] => return Nil
    case node: InstancePredicateApply[G] => Classes
    case node: CoalesceInstancePredicateApply[G] => Classes
    case node: ADTFunctionInvocation[G] => return Nil
    case node: ProcedureInvocation[G] => return Nil
    case node: InvokeProcedure[G] => return Nil
    case node: FunctionInvocation[G] => return Nil
    case node: MethodInvocation[G] => return Nil
    case node: InvokeMethod[G] => return Nil
    case node: InstanceFunctionInvocation[G] => Classes
    case node: UMinus[G] => return Nil
    case node: BitNot[G] => BitOperators
    case node: Not[G] => return Nil
    case node: AmbiguousMult[G] =>
      return AmbiguousOperators +: (
        if(node.isProcessOp) Seq(Models)
        else Nil
      )
    case node: AmbiguousPlus[G] =>
      return AmbiguousOperators +: (
        if(node.isProcessOp) Seq(Models)
        else if(node.isPointerOp) Seq(Pointers)
        else Nil
      )
    case node: AmbiguousMinus[G] => AmbiguousOperators
    case node: AmbiguousOr[G] =>
      return AmbiguousOperators +: (
        if(node.isProcessOp) Seq(Models)
        else Nil
      )
    case node: BitOp[G] =>
      return AmbiguousOperators +: (
        if(node.isBoolOp) Nil
        else Seq(BitOperators)
      )
    case node: ComputationalOr[G] => ComputationalLogicOperator
    case node: ComputationalXor[G] => ComputationalLogicOperator
    case node: ComputationalAnd[G] => ComputationalLogicOperator
    case node: Exp[G] => Exponents
    case node: Plus[G] => return Nil
    case node: Minus[G] => return Nil
    case node: Mult[G] => return Nil
    case node: Div[G] => return Nil
    case node: FloorDiv[G] => return Nil
    case node: Mod[G] => return Nil
    case node: BitAnd[G] => BitOperators
    case node: BitOr[G] => BitOperators
    case node: BitXor[G] => BitOperators
    case node: BitShl[G] => BitOperators
    case node: BitShr[G] => BitOperators
    case node: BitUShr[G] => BitOperators
    case node: And[G] => return Nil
    case node: Or[G] => return Nil
    case node: Implies[G] => return Nil
    case node: Star[G] => return Nil
    case node: Wand[G] => MagicWand
    case node: Scale[G] => return Nil
    case node: Unfolding[G] => return Nil
    case node: Perm[G] => return Nil
    case node: HPerm[G] => Models
    case node: APerm[G] => Models
    case node: PointsTo[G] => SugarPermissionOperator
    case node: CurPerm[G] => return Nil
    case node: ValidArray[G] => return Seq(SugarPermissionOperator, Arrays)
    case node: ValidMatrix[G] => return Seq(SugarPermissionOperator, Arrays)
    case node: PermPointer[G] => return Seq(SugarPermissionOperator, Pointers)
    case node: PermPointerIndex[G] => return Seq(SugarPermissionOperator, Pointers)
    case node: Eq[G] => return Nil
    case node: Neq[G] => return Nil
    case node: AmbiguousGreater[G] => AmbiguousOperators
    case node: AmbiguousLess[G] => AmbiguousOperators
    case node: AmbiguousGreaterEq[G] => AmbiguousOperators
    case node: AmbiguousLessEq[G] => AmbiguousOperators
    case node: Greater[G] => return Nil
    case node: Less[G] => return Nil
    case node: GreaterEq[G] => return Nil
    case node: LessEq[G] => return Nil
    case node: Select[G] => return Nil
    case node: NewObject[G] => Classes
    case node: NewArray[G] => Arrays
    case node: Old[G] => return Nil
    case node: AmbiguousSubscript[G] =>
      return AmbiguousOperators +: (
        if(node.isPointerOp) Seq(Pointers)
        else if(node.isMapOp) Seq(AxiomaticLibraryType)
        else if(node.isArrayOp) Seq(Arrays)
        else Nil
      )
    case node: SeqSubscript[G] => return Nil
    case node: ArraySubscript[G] => Arrays
    case node: PointerAdd[G] => Pointers
    case node: PointerSubscript[G] => Pointers
    case node: Length[G] => Arrays
    case node: Size[G] => return Nil
    case node: Cons[G] => SugarCollectionOperator
    case node: Head[G] => SugarCollectionOperator
    case node: Tail[G] => SugarCollectionOperator
    case node: Drop[G] => return Nil
    case node: Take[G] => return Nil
    case node: Slice[G] => SugarCollectionOperator
    case node: SeqUpdate[G] => return Nil
    case node: Concat[G] => return Nil
    case node: RemoveAt[G] => SugarCollectionOperator
    case node: Empty[G] => SugarCollectionOperator
    case node: AmbiguousMember[G] =>
      return AmbiguousOperators +: (
        if(node.isMapOp) Seq(AxiomaticLibraryType)
        else Nil
      )
    case node: SetMember[G] => return Nil
    case node: SeqMember[G] => return Nil
    case node: MapMember[G] => AxiomaticLibraryType
    case node: BagMemberCount[G] => return Nil
    case node: SubSet[G] => return Nil
    case node: SubSetEq[G] => SugarCollectionOperator
    case node: SubBag[G] => return Nil
    case node: SubBagEq[G] => SugarCollectionOperator
    case node: SetIntersection[G] => return Nil
    case node: BagLargestCommon[G] => return Nil
    case node: SetMinus[G] => return Nil
    case node: BagMinus[G] => return Nil
    case node: SetUnion[G] => return Nil
    case node: BagAdd[G] => return Nil
    case node: Permutation[G] => PermutationOperator
    case node: OptGet[G] => AxiomaticLibraryType
    case node: OptGetOrElse[G] => AxiomaticLibraryType
    case node: GetLeft[G] => AxiomaticLibraryType
    case node: GetRight[G] => AxiomaticLibraryType
    case node: IsLeft[G] => AxiomaticLibraryType
    case node: IsRight[G] => AxiomaticLibraryType
    case node: MapGet[G] => AxiomaticLibraryType
    case node: TupGet[G] => AxiomaticLibraryType
    case node: VectorSum[G] => MatrixVector
    case node: VectorCompare[G] => MatrixVector
    case node: VectorRepeat[G] => MatrixVector
    case node: MatrixSum[G] => MatrixVector
    case node: MatrixCompare[G] => MatrixVector
    case node: MatrixRepeat[G] => MatrixVector
    case node: TypeValue[G] => TypeValuesAndGenerics
    case node: TypeOf[G] => TypeValuesAndGenerics
    case node: InstanceOf[G] => TypeValuesAndGenerics
    case node: Cast[G] => TypeValuesAndGenerics
    case node: SubType[G] => TypeValuesAndGenerics
    case node: SuperType[G] => TypeValuesAndGenerics
    case node: PreAssignExpression[G] => ExpressionWithSideEffects
    case node: PostAssignExpression[G] => ExpressionWithSideEffects
    case node: With[G] => ExpressionWithSideEffects
    case node: Then[G] => ExpressionWithSideEffects
    case node: Held[G] => IntrinsicLocks
    case node: IdleToken[G] => JavaThreads
    case node: JoinToken[G] => JavaThreads
    case node: EmptyProcess[G] => Models
    case node: ActionApply[G] => Models
    case node: ProcessApply[G] => Models
    case node: ProcessSeq[G] => Models
    case node: ProcessChoice[G] => Models
    case node: ProcessPar[G] => Models
    case node: ProcessSelect[G] => Models
    case node: ModelNew[G] => Models
    case node: ModelState[G] => Models
    case node: ModelAbstractState[G] => Models
    case node: ModelCreate[G] => Models
    case node: ModelDestroy[G] => Models
    case node: ModelSplit[G] => Models
    case node: ModelMerge[G] => Models
    case node: ModelChoose[G] => Models
    case node: ModelPerm[G] => Models
    case node: ActionPerm[G] => Models
    case node: Eval[G] => node.expr match {
      case node: MethodInvocation[G] =>
        topLevelInvocations += node
        return Nil
      case node: ProcedureInvocation[G] =>
        topLevelInvocations += node
        return Nil
      // Function invocations do not count as a statement
      case _ => NonMethodInvocationEvaluation
    }
    case node: LocalDecl[G] => UnscopedDeclaration
    case node: Return[G] =>
      if(node.result == Void[G]()(DiagnosticOrigin)) return Nil
      else return Seq(NonVoidReturn)
    case node: Assign[G] => return Nil
    case node: Block[G] => return Nil
    case node: Scope[G] => return Nil
    case node: ScopedExpr[G] => return Nil
    case node: LoopInvariant[G] => return Nil
    case node: IterationContract[G] => LoopIterationContract
    case node: IndetBranch[G] => NonTrivialBranch
    case node: Branch[G] =>
      node.branches match {
        case Seq((_, _), (BooleanValue(true), _)) => return Nil
        case _ => NonTrivialBranch
      }
    case node: Switch[G] => SwitchStatement
    case node: Loop[G] => node match {
      case Loop(Block(Nil), _, Block(Nil), _, _) => return Nil
      case _ => NonWhileLoop
    }
    case node: CatchClause[G] => return Nil
    case node: TryCatchFinally[G] => TryCatchStatement
    case node: Synchronized[G] => IntrinsicLocks
    case node: ParInvariant[G] => ParallelRegion
    case node: ParAtomic[G] => ParallelRegion
    case node: ParBarrier[G] => ParallelRegion
    case node: IterVariable[G] => return Nil
    case node: ParBlock[G] => ParallelRegion
    case node: ScaleByParBlock[G] => ParallelRegion
    case node: ParParallel[G] => ParallelRegion
    case node: ParRegion[G] => ParallelRegion
    case node: VecBlock[G] => ParallelRegion
    case node: ParStatement[G] => ParallelRegion
    case node: Send[G] => SendRecv
    case node: Recv[G] => SendRecv
    case node: SendDecl[G] => SendRecv
    case node: DefaultCase[G] => SwitchStatement
    case node: Case[G] => SwitchStatement
    case node: Label[G] => node.stat match {
      case Block(Nil) => return Nil
      case _ => NonTrivialLabel
    }
    case node: Goto[G] => return Nil
    case node: Exhale[G] => return Nil
    case node: Assert[G] => return Nil
    case node: Refute[G] => return Nil
    case node: Inhale[G] => return Nil
    case node: Assume[G] => return Nil
    case node: SpecIgnoreStart[G] => SpecIgnore
    case node: SpecIgnoreEnd[G] => SpecIgnore
    case node: Throw[G] => Exceptions
    case node: Wait[G] => WaitNotify
    case node: Notify[G] => WaitNotify
    case node: RunMethod[G] => JavaThreads
    case node: Fork[G] => JavaThreads
    case node: Join[G] => JavaThreads
    case node: Lock[G] => IntrinsicLocks
    case node: Unlock[G] => IntrinsicLocks
    case node: Fold[G] => return Nil
    case node: Unfold[G] => return Nil
    case node: WandCreate[G] => MagicWand
    case node: WandApply[G] => MagicWand
    case node: ModelDo[G] => Models
    case node: Havoc[G] => return Nil
    case node: Break[G] => ExceptionalLoopControl
    case node: Continue[G] => ExceptionalLoopControl
    case node: TNotAValue[G] => return Nil
    case node: TAny[G] => ExoticTypes
    case node: TNothing[G] => ExoticTypes
    case node: TUnion[G] => ExoticTypes
    case node: TVoid[G] => return Nil
    case node: TNull[G] => return Nil
    case node: TBool[G] => return Nil
    case node: TResource[G] => return Nil
    case node: TInt[G] => return Nil
    case node: TBoundedInt[G] => return Nil
    case node: TFloat[G] => return Nil
    case node: TRational[G] => return Nil
    case node: TFraction[G] => AxiomaticLibraryType
    case node: TZFraction[G] => AxiomaticLibraryType
    case node: TChar[G] => TextTypes
    case node: TString[G] => TextTypes
    case node: TRef[G] => return Nil
    case node: TOption[G] => AxiomaticLibraryType
    case node: TEither[G] => AxiomaticLibraryType
    case node: TTuple[G] => AxiomaticLibraryType
    case node: TSeq[G] => return Nil
    case node: TSet[G] => return Nil
    case node: TBag[G] => return Nil
    case node: TMatrix[G] => MatrixVector
    case node: TArray[G] => Arrays
    case node: TPointer[G] => Pointers
    case node: TMap[G] => AxiomaticLibraryType
    case node: TProcess[G] => Models
    case node: TModel[G] => Models
    case node: TClass[G] => Classes
    case node: TAxiomatic[G] => return Nil
    case node: TType[G] => TypeValuesAndGenerics
    case node: TVar[G] => TypeValuesAndGenerics
    case node: Variable[G] => return Nil
    case node: LabelDecl[G] => return Nil
    case node: ParBlockDecl[G] => ParallelRegion
    case node: ParInvariantDecl[G] => ParallelRegion
    case node: SimplificationRule[G] => TermRewriteRules
    case node: FunctionOf[G] => TermRewriteRules
    case node: AxiomaticDataType[G] => return Nil
    case node: ADTAxiom[G] => return Nil
    case node: SignalsClause[G] => Exceptions
    case node: DecreasesClause[G] => return Nil
    case node: ApplicableContract[G] => return Nil
    case node: SplitAccountedPredicate[G] => return Nil
    case node: UnitAccountedPredicate[G] => return Nil
    case node: Function[G] =>
      return (if(node.inline) Seq(ApplicableToBeInlined) else Nil) ++
        (if(node.typeArgs.nonEmpty) Seq(TypeValuesAndGenerics) else Nil)
    case node: Procedure[G] =>
      return (if(node.inline) Seq(ApplicableToBeInlined) else Nil) ++
        (if(node.typeArgs.nonEmpty) Seq(TypeValuesAndGenerics) else Nil) ++
        (if(node.pure) Seq(MethodToBePurified) else Nil)
    case node: Predicate[G] =>
      return (if(node.inline) Seq(ApplicableToBeInlined) else Nil)
    case node: InstanceFunction[G] =>
      return Seq(Classes) ++
        (if(node.typeArgs.nonEmpty) Seq(TypeValuesAndGenerics) else Nil) ++
        (if(node.inline) Seq(ApplicableToBeInlined) else Nil)
    case node: InstanceMethod[G] =>
      return Seq(Classes) ++
        (if(node.typeArgs.nonEmpty) Seq(TypeValuesAndGenerics) else Nil) ++
        (if(node.inline) Seq(ApplicableToBeInlined) else Nil) ++
        (if(node.pure) Seq(MethodToBePurified) else Nil)
    case node: InstancePredicate[G] =>
      return Seq(Classes) ++
        (if(node.inline) Seq(ApplicableToBeInlined) else Nil)
    case node: ADTFunction[G] => return Nil
    case node: Final[G] => return Nil
    case node: InstanceField[G] => FinalField
    case node: Class[G] => Classes
    case node: ModelField[G] => Models
    case node: ModelProcess[G] => Models
    case node: ModelAction[G] => Models
    case node: Model[G] => Models
    case node: SilverDeref[G] => return Nil
    case node: SilverCurFieldPerm[G] => return Nil
    case node: SilverCurPredPerm[G] => return Nil
    case node: SilverNewRef[G] => return Nil
    case node: SilverFieldAssign[G] => return Nil
    case node: SilverLocalAssign[G] => return Nil
    case node: SilverField[G] => return Nil
    case node: SilverIntToRat[G] => return Nil
    case node: SilverNull[G] => return Nil
    case node: SilverSeqSize[G] => return Nil
    case node: SilverSetSize[G] => return Nil
    case node: SilverBagSize[G] => return Nil
    case node: SilverMapSize[G] => return Nil
    case node: CPure[G] => return Nil
    case node: CInline[G] => return Nil
    case node: CTypedef[G] => return Nil
    case node: CExtern[G] => return Nil
    case node: CStatic[G] => return Nil
    case node: CVoid[G] => return Nil
    case node: CChar[G] => return Nil
    case node: CShort[G] => return Nil
    case node: CInt[G] => return Nil
    case node: CLong[G] => return Nil
    case node: CFloat[G] => return Nil
    case node: CDouble[G] => return Nil
    case node: CSigned[G] => return Nil
    case node: CUnsigned[G] => return Nil
    case node: CBool[G] => return Nil
    case node: CTypedefName[G] => return Nil
    case node: CSpecificationType[G] => return Nil
    case node: CTypeQualifierDeclarationSpecifier[G] => return Nil
    case node: CConst[G] => return Nil
    case node: CRestrict[G] => return Nil
    case node: CVolatile[G] => return Nil
    case node: CAtomic[G] => return Nil
    case node: CKernel[G] => return Nil
    case node: CPointer[G] => return Nil
    case node: CParam[G] => return Nil
    case node: CPointerDeclarator[G] => return Nil
    case node: CArrayDeclarator[G] => return Nil
    case node: CTypedFunctionDeclarator[G] => return Nil
    case node: CAnonymousFunctionDeclarator[G] => return Nil
    case node: CName[G] => return Nil
    case node: CInit[G] => return Nil
    case node: CDeclaration[G] => return Nil
    case node: CFunctionDefinition[G] => return Nil
    case node: CTranslationUnit[G] => return Nil
    case node: CGlobalDeclaration[G] => return Nil
    case node: CDeclarationStatement[G] => return Nil
    case node: CGoto[G] => return Nil
    case node: GpgpuLocalBarrier[G] => return Nil
    case node: GpgpuGlobalBarrier[G] => return Nil
    case node: GpgpuAtomic[G] => return Nil
    case node: CLocal[G] => return Nil
    case node: CInvocation[G] => return Nil
    case node: CStructAccess[G] => return Nil
    case node: CStructDeref[G] => return Nil
    case node: GpgpuCudaKernelInvocation[G] => return Nil
    case node: CPrimitiveType[G] => return Nil
    case node: JavaName[G] => return Nil
    case node: JavaImport[G] => return Nil
    case node: JavaPublic[G] => return Nil
    case node: JavaProtected[G] => return Nil
    case node: JavaPrivate[G] => return Nil
    case node: JavaStatic[G] => return Nil
    case node: JavaAbstract[G] => return Nil
    case node: JavaFinal[G] => return Nil
    case node: JavaStrictFP[G] => return Nil
    case node: JavaNative[G] => return Nil
    case node: JavaSynchronized[G] => return Nil
    case node: JavaTransient[G] => return Nil
    case node: JavaVolatile[G] => return Nil
    case node: JavaPure[G] => return Nil
    case node: JavaInline[G] => return Nil
    case node: JavaNamespace[G] => return Nil
    case node: JavaClass[G] => return Nil
    case node: JavaInterface[G] => return Nil
    case node: JavaSharedInitialization[G] => return Nil
    case node: JavaFields[G] => return Nil
    case node: JavaConstructor[G] => return Nil
    case node: JavaMethod[G] => return Nil
    case node: JavaLocalDeclaration[G] => return Nil
    case node: JavaLocalDeclarationStatement[G] => return Nil
    case node: JavaVariableDeclaration[G] => return Nil
    case node: JavaNamedType[G] => return Nil
    case node: JavaTClass[G] => return Nil
    case node: JavaLocal[G] => return Nil
    case node: JavaDeref[G] => return Nil
    case node: JavaLiteralArray[G] => return Nil
    case node: JavaInvocation[G] => return Nil
    case node: JavaNewClass[G] => return Nil
    case node: JavaNewLiteralArray[G] => return Nil
    case node: JavaNewDefaultArray[G] => return Nil
    case node: PVLNamedType[G] => return Nil
    case node: PVLLocal[G] => return Nil
    case node: PVLDeref[G] => return Nil
    case node: PVLInvocation[G] => return Nil
    case node: PVLNew[G] => return Nil
    case node: PVLConstructor[G] => return Nil
    case node: Commit[G] => IntrinsicLocks
    case node: FramedProof[G] => return Nil
  })
}
