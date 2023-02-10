package vct.col.feature

import vct.col.ast._
import vct.col.origin.DiagnosticOrigin
import vct.col.util.AstBuildHelpers._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class FeatureRainbow[G] {
  val features: mutable.Set[Feature] = mutable.Set()
  val examples: mutable.Map[Feature, ArrayBuffer[Node[G]]] = mutable.Map()

  private var returnTypes: Seq[Type[G]] = Nil
  private var returnValues: Seq[Expr[G]] = Nil

  def scan(node: Node[G]): Unit =
    node.transSubnodes.foreach(node => scanFlatly(node).foreach(f => {
      features += f
      examples.getOrElseUpdate(f, ArrayBuffer()) += node
    }))

  def scanFlatly(node: Node[G]): Seq[Feature] = Seq(node match {
    case node: AmbiguousLocation[G] => AmbiguousOperators
    case node: AmbiguousGreater[G] => AmbiguousOperators
    case node: AmbiguousLess[G] => AmbiguousOperators
    case node: AmbiguousGreaterEq[G] => AmbiguousOperators
    case node: AmbiguousLessEq[G] => AmbiguousOperators

    case node: ArrayLocation[G] => Arrays
    case node: NewArray[G] => Arrays
    case node: ArraySubscript[G] => Arrays
    case node: Length[G] => Arrays
    case node: TArray[G] => Arrays

    case node: Assign[G] => Assignment
    case node: Havoc[G] => Assignment
    case node: SilverLocalAssign[G] => Assignment

    case node: ADTAxiom[G] => AxiomaticDataTypes
    case node: ADTFunction[G] => AxiomaticDataTypes
    case node: ADTFunctionInvocation[G] => AxiomaticDataTypes
    case node: AxiomaticDataType[G] => AxiomaticDataTypes
    case node: TAxiomatic[G] => AxiomaticDataTypes

    case node: LiteralTuple[G] => AxiomaticLibraryType
    case node: LiteralMap[G] => AxiomaticLibraryType
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
    case node: MapMember[G] => AxiomaticLibraryType
    case node: OptGet[G] => AxiomaticLibraryType
    case node: OptGetOrElse[G] => AxiomaticLibraryType
    case node: GetLeft[G] => AxiomaticLibraryType
    case node: GetRight[G] => AxiomaticLibraryType
    case node: IsLeft[G] => AxiomaticLibraryType
    case node: IsRight[G] => AxiomaticLibraryType
    case node: MapGet[G] => AxiomaticLibraryType
    case node: TupGet[G] => AxiomaticLibraryType
    case node: TFraction[G] => AxiomaticLibraryType
    case node: TZFraction[G] => AxiomaticLibraryType
    case node: TOption[G] => AxiomaticLibraryType
    case node: TEither[G] => AxiomaticLibraryType
    case node: TTuple[G] => AxiomaticLibraryType
    case node: TMap[G] => AxiomaticLibraryType
    case node: OptEmpty[G] => AxiomaticLibraryType
    case node: OptNoneTyped[G] => AxiomaticLibraryType
    case node: OptSomeTyped[G] => AxiomaticLibraryType
    case node: TNull[G] => AxiomaticLibraryType

    case node: Assert[G] => BasicStatement
    case node: Assume[G] => BasicStatement
    case node: Block[G] => BasicStatement
    case node: Scope[G] => BasicStatement

    case node: BitNot[G] => BitOperators
    case node: BitAnd[G] => BitOperators
    case node: BitOr[G] => BitOperators
    case node: BitXor[G] => BitOperators
    case node: BitShl[G] => BitOperators
    case node: BitShr[G] => BitOperators
    case node: BitUShr[G] => BitOperators

    case node: Values[G] => BuiltinArrayOperators

    case node: Deref[G] => Classes
    case node: InstancePredicateApply[G] => Classes
    case node: CoalesceInstancePredicateApply[G] => Classes
    case node: InstanceFunctionInvocation[G] => Classes
    case node: InstancePredicateLocation[G] => Classes
    case node: NewObject[G] => Classes
    case node: TClass[G] => Classes
    case node: Class[G] => Classes
    case node: FieldLocation[G] => Classes
    case node: InvokeMethod[G] => Classes
    case node: TAnyClass[G] => Classes
    case node: ThisObject[G] => Classes
    case node: InstanceField[G] => Classes

    case node: ApplyCoercion[G] => Coercions
    case node: CoerceBoolResource[G] => Coercions
    case node: CoerceBoundIntFrac[G] => Coercions
    case node: CoerceBoundIntZFrac[G] => Coercions
    case node: CoerceClassAnyClass[G] => Coercions
    case node: CoerceColToCPrimitive[G] => Coercions
    case node: CoerceCPrimitiveToCol[G] => Coercions
    case node: CoerceFloatRat[G] => Coercions
    case node: CoerceFracZFrac[G] => Coercions
    case node: CoerceIdentity[G] => Coercions
    case node: CoerceIncreasePrecision[G] => Coercions
    case node: CoerceIntRat[G] => Coercions
    case node: CoerceJavaClassAnyClass[G] => Coercions
    case node: CoerceJavaSupports[G] => Coercions
    case node: CoerceJoinUnion[G] => Coercions
    case node: CoerceMapBag[G] => Coercions
    case node: CoerceMapEither[G] => Coercions
    case node: CoerceMapMap[G] => Coercions
    case node: CoerceMapMatrix[G] => Coercions
    case node: CoerceMapOption[G] => Coercions
    case node: CoerceMapSeq[G] => Coercions
    case node: CoerceMapSet[G] => Coercions
    case node: CoerceMapTuple[G] => Coercions
    case node: CoerceMapType[G] => Coercions
    case node: CoerceNothingSomething[G] => Coercions
    case node: CoerceNullAnyClass[G] => Coercions
    case node: CoerceNullArray[G] => Coercions
    case node: CoerceNullClass[G] => Coercions
    case node: CoerceNullJavaClass[G] => Coercions
    case node: CoerceNullPointer[G] => Coercions
    case node: CoerceNullRef[G] => Coercions
    case node: CoerceRatZFrac[G] => Coercions
    case node: CoerceSelectUnion[G] => Coercions
    case node: CoerceSomethingAny[G] => Coercions
    case node: CoerceSupports[G] => Coercions
    case node: CoerceUnboundInt[G] => Coercions
    case node: CoerceWidenBound[G] => Coercions
    case node: CoerceZFracFrac[G] => Coercions
    case node: CoerceZFracRat[G] => Coercions
    case node: CoercionSequence[G] => Coercions

    case node: ComputationalOr[G] => ComputationalLogicOperator
    case node: ComputationalXor[G] => ComputationalLogicOperator
    case node: ComputationalAnd[G] => ComputationalLogicOperator

    case node: AmbiguousThis[G] => ContextSensitiveNode
    case node: AmbiguousResult[G] => ContextSensitiveNode

    case node: ApplicableContract[G] => Contracts
    case node: LoopInvariant[G] => Contracts
    case node: SplitAccountedPredicate[G] => Contracts
    case node: UnitAccountedPredicate[G] => Contracts

    case node: CAnonymousFunctionDeclarator[G] => CSpecific
    case node: CArrayDeclarator[G] => CSpecific
    case node: CAtomic[G] => CSpecific
    case node: CBool[G] => CSpecific
    case node: CConst[G] => CSpecific
    case node: CDeclaration[G] => CSpecific
    case node: CExtern[G] => CSpecific
    case node: CFunctionDefinition[G] => CSpecific
    case node: CGlobalDeclaration[G] => CSpecific
    case node: CInit[G] => CSpecific
    case node: CInline[G] => CSpecific
    case node: CInt[G] => CSpecific
    case node: CInvocation[G] => CSpecific
    case node: CLocal[G] => CSpecific
    case node: CLocalDeclaration[G] => CSpecific
    case node: CLong[G] => CSpecific
    case node: CName[G] => CSpecific
    case node: CParam[G] => CSpecific
    case node: CPrimitiveType[G] => CSpecific
    case node: CPure[G] => CSpecific
    case node: CRestrict[G] => CSpecific
    case node: CShort[G] => CSpecific
    case node: CSigned[G] => CSpecific
    case node: CSpecificationType[G] => CSpecific
    case node: CStatic[G] => CSpecific
    case node: CStructAccess[G] => CSpecific
    case node: CStructDeref[G] => CSpecific
    case node: CTCudaVec[G] => CSpecific
    case node: CTranslationUnit[G] => CSpecific
    case node: CTypedef[G] => CSpecific
    case node: CTypedefName[G] => CSpecific
    case node: CTypedFunctionDeclarator[G] => CSpecific
    case node: CTypeQualifierDeclarationSpecifier[G] => CSpecific
    case node: CUDAKernel[G] => CSpecific
    case node: CUnsigned[G] => CSpecific
    case node: CVoid[G] => CSpecific
    case node: CVolatile[G] => CSpecific
    case node: GlobalThreadId[G] => CSpecific
    case node: GpgpuAtomic[G] => CSpecific
    case node: GpgpuBarrier[G] => CSpecific
    case node: GpgpuCudaKernelInvocation[G] => CSpecific
    case node: GPUGlobal[G] => CSpecific
    case node: GpuGlobalMemoryFence[G] => CSpecific
    case node: GPULocal[G] => CSpecific
    case node: GpuLocalMemoryFence[G] => CSpecific
    case node: GpuZeroMemoryFence[G] => CSpecific
    case node: LocalThreadId[G] => CSpecific
    case node: OpenCLKernel[G] => CSpecific
    case node: SharedMemSize[G] => CSpecific

    case node: CurrentThreadId[G] => CurrentThread

    case node: UntypedLiteralSeq[G] => DynamicallyTypedCollection
    case node: UntypedLiteralSet[G] => DynamicallyTypedCollection
    case node: UntypedLiteralBag[G] => DynamicallyTypedCollection

    case node: Eval[G] => Evaluation

    case node: Break[G] => ExceptionalLoopControl
    case node: Continue[G] => ExceptionalLoopControl

    case node: Throw[G] => Exceptions
    case node: SignalsClause[G] => Exceptions

    case node: TAny[G] => ExoticTypes
    case node: TNothing[G] => ExoticTypes
    case node: TUnion[G] => ExoticTypes
    case node: TBoundedInt[G] => ExoticTypes
    case node: TNotAValue[G] => ExoticTypes

    case node: Exp[G] => Exponents

    case node: PreAssignExpression[G] => ExpressionWithSideEffects
    case node: PostAssignExpression[G] => ExpressionWithSideEffects
    case node: With[G] => ExpressionWithSideEffects
    case node: Then[G] => ExpressionWithSideEffects
    case node: ScopedExpr[G] => ExpressionWithSideEffects

    case node: CastFloat[G] => Floats
    case node: FloatValue[G] => Floats
    case node: TFloat[G] => Floats

    case node: Final[G] => FinalField

    case node: Goto[G] => Gotos
    case node: LabelDecl[G] => Gotos

    case node: InlinePattern[G] => InlineQuantifierPattern

    case node: Held[G] => IntrinsicLocks
    case node: Synchronized[G] => IntrinsicLocks
    case node: Lock[G] => IntrinsicLocks
    case node: Unlock[G] => IntrinsicLocks
    case node: Commit[G] => IntrinsicLocks
    case node: Committed[G] => IntrinsicLocks

    case node: JavaAbstract[G] => JavaSpecific
    case node: JavaAnnotation[G] => JavaSpecific
    case node: JavaAnnotationInterface[G] => JavaSpecific
    case node: JavaAnnotationMethod[G] => JavaSpecific
    case node: JavaClass[G] => JavaSpecific
    case node: JavaConstructor[G] => JavaSpecific
    case node: JavaDeref[G] => JavaSpecific
    case node: JavaFields[G] => JavaSpecific
    case node: JavaFinal[G] => JavaSpecific
    case node: JavaImport[G] => JavaSpecific
    case node: JavaInline[G] => JavaSpecific
    case node: JavaInterface[G] => JavaSpecific
    case node: JavaInvocation[G] => JavaSpecific
    case node: JavaLiteralArray[G] => JavaSpecific
    case node: JavaLocal[G] => JavaSpecific
    case node: JavaLocalDeclaration[G] => JavaSpecific
    case node: JavaLocalDeclarationStatement[G] => JavaSpecific
    case node: JavaMethod[G] => JavaSpecific
    case node: JavaName[G] => JavaSpecific
    case node: JavaNamedType[G] => JavaSpecific
    case node: JavaNamespace[G] => JavaSpecific
    case node: JavaNative[G] => JavaSpecific
    case node: JavaNewClass[G] => JavaSpecific
    case node: JavaNewDefaultArray[G] => JavaSpecific
    case node: JavaNewLiteralArray[G] => JavaSpecific
    case node: JavaPrivate[G] => JavaSpecific
    case node: JavaProtected[G] => JavaSpecific
    case node: JavaPublic[G] => JavaSpecific
    case node: JavaPure[G] => JavaSpecific
    case node: JavaSharedInitialization[G] => JavaSpecific
    case node: JavaStatic[G] => JavaSpecific
    case node: JavaStrictFP[G] => JavaSpecific
    case node: JavaSynchronized[G] => JavaSpecific
    case node: JavaTClass[G] => JavaSpecific
    case node: JavaTransient[G] => JavaSpecific
    case node: JavaVariableDeclaration[G] => JavaSpecific
    case node: JavaVolatile[G] => JavaSpecific

    case node: IdleToken[G] => JavaThreads
    case node: JoinToken[G] => JavaThreads
    case node: RunMethod[G] => JavaThreads
    case node: Fork[G] => JavaThreads
    case node: Join[G] => JavaThreads

    case node: IterationContract[G] => LoopIterationContract

    case node: Wand[G] => MagicWand
    case node: WandPackage[G] => MagicWand
    case node: WandApply[G] => MagicWand

    case node: VectorSum[G] => MatrixVector
    case node: VectorCompare[G] => MatrixVector
    case node: VectorRepeat[G] => MatrixVector
    case node: MatrixSum[G] => MatrixVector
    case node: MatrixCompare[G] => MatrixVector
    case node: MatrixRepeat[G] => MatrixVector
    case node: TMatrix[G] => MatrixVector

    case node: InvokeProcedure[G] => Methods
    case node: MethodInvocation[G] => Methods
    case node: ProcedureInvocation[G] => Methods

    case node: ModelDeref[G] => Models
    case node: ModelLocation[G] => Models
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
    case node: ModelDo[G] => Models
    case node: TProcess[G] => Models
    case node: TModel[G] => Models
    case node: ModelField[G] => Models
    case node: ModelProcess[G] => Models
    case node: ModelAction[G] => Models
    case node: Model[G] => Models
    case node: ThisModel[G] => Models

    case node: IndetBranch[G] => NonTrivialBranch

    case node: Sum[G] => NumericReductionOperator
    case node: Product[G] => NumericReductionOperator

    case node: ParInvariant[G] => ParallelRegion
    case node: ParAtomic[G] => ParallelRegion
    case node: ParBarrier[G] => ParallelRegion
    case node: ParBlock[G] => ParallelRegion
    case node: ScaleByParBlock[G] => ParallelRegion
    case node: ParParallel[G] => ParallelRegion
    case node: ParRegion[G] => ParallelRegion
    case node: VecBlock[G] => ParallelRegion
    case node: ParStatement[G] => ParallelRegion
    case node: ParBlockDecl[G] => ParallelRegion
    case node: ParInvariantDecl[G] => ParallelRegion
    case node: IterVariable[G] => ParallelRegion
    case node: ParSequential[G] => ParallelRegion

    case node: Permutation[G] => PermutationOperator

    case node: DerefPointer[G] => Pointers
    case node: AddrOf[G] => Pointers
    case node: PointerLocation[G] => Pointers
    case node: PointerAdd[G] => Pointers
    case node: PointerSubscript[G] => Pointers
    case node: PointerBlockLength[G] => Pointers
    case node: PointerBlockOffset[G] => Pointers
    case node: PointerLength[G] => Pointers
    case node: TPointer[G] => Pointers

    case node: Extract[G] => ProofHelpers
    case node: FramedProof[G] => ProofHelpers
    case node: IndeterminateInteger[G] => ProofHelpers
    case node: Refute[G] => ProofHelpers

    case node: PVLConstructor[G] => PvlSpecific
    case node: PVLDeref[G] => PvlSpecific
    case node: PVLInvocation[G] => PvlSpecific
    case node: PVLLocal[G] => PvlSpecific
    case node: PVLNamedType[G] => PvlSpecific
    case node: PVLNew[G] => PvlSpecific

    case node: CurPerm[G] => Resources
    case node: Exhale[G] => Resources
    case node: Fold[G] => Resources
    case node: Inhale[G] => Resources
    case node: Null[G] => Resources
    case node: Old[G] => Resources
    case node: Perm[G] => Resources
    case node: PredicateApply[G] => Resources
    case node: PredicateLocation[G] => Resources
    case node: SilverDeref[G] => Resources
    case node: SilverField[G] => Resources
    case node: SilverFieldAssign[G] => Resources
    case node: SilverFieldLocation[G] => Resources
    case node: SilverNull[G] => Resources
    case node: Star[G] => Resources
    case node: Starall[G] => Resources
    case node: TRef[G] => Resources
    case node: TResource[G] => Resources
    case node: Unfold[G] => Resources
    case node: Unfolding[G] => Resources
    case node: Value[G] => Resources

    case node: Program[G] => RootStructure
    case node: Verification[G] => RootStructure
    case node: VerificationContext[G] => RootStructure

    case node: Send[G] => SendRecv
    case node: Recv[G] => SendRecv
    case node: SendDecl[G] => SendRecv

    case node: Range[G] => SequenceRange

    case node: BagAdd[G] => SilverAxiomaticLibraryType
    case node: BagLargestCommon[G] => SilverAxiomaticLibraryType
    case node: BagMemberCount[G] => SilverAxiomaticLibraryType
    case node: BagMinus[G] => SilverAxiomaticLibraryType
    case node: Concat[G] => SilverAxiomaticLibraryType
    case node: Drop[G] => SilverAxiomaticLibraryType
    case node: LiteralBag[G] => SilverAxiomaticLibraryType
    case node: LiteralSeq[G] => SilverAxiomaticLibraryType
    case node: LiteralSet[G] => SilverAxiomaticLibraryType
    case node: SeqMember[G] => SilverAxiomaticLibraryType
    case node: SeqSubscript[G] => SilverAxiomaticLibraryType
    case node: SeqUpdate[G] => SilverAxiomaticLibraryType
    case node: SetIntersection[G] => SilverAxiomaticLibraryType
    case node: SetMember[G] => SilverAxiomaticLibraryType
    case node: SetMinus[G] => SilverAxiomaticLibraryType
    case node: SetUnion[G] => SilverAxiomaticLibraryType
    case node: SilverBagSize[G] => SilverAxiomaticLibraryType
    case node: SilverMapSize[G] => SilverAxiomaticLibraryType
    case node: SilverSeqSize[G] => SilverAxiomaticLibraryType
    case node: SilverSetSize[G] => SilverAxiomaticLibraryType
    case node: Size[G] => SilverAxiomaticLibraryType
    case node: SubBag[G] => SilverAxiomaticLibraryType
    case node: SubSet[G] => SilverAxiomaticLibraryType
    case node: Take[G] => SilverAxiomaticLibraryType
    case node: TBag[G] => SilverAxiomaticLibraryType
    case node: TSeq[G] => SilverAxiomaticLibraryType
    case node: TSet[G] => SilverAxiomaticLibraryType

    case node: SilverCurFieldPerm[G] => SilverSpecific
    case node: SilverCurPredPerm[G] => SilverSpecific
    case node: SilverPartialADTFunctionInvocation[G] => SilverSpecific
    case node: SilverPartialTAxiomatic[G] => SilverSpecific
    case node: SilverUntypedNonemptyLiteralMap[G] => SilverSpecific

    case node: FunctionInvocation[G] => SmtDeclarations
    case node: Variable[G] => SmtDeclarations

    case node: And[G] => SmtOperators
    case node: BooleanValue[G] => SmtOperators
    case node: Div[G] => SmtOperators
    case node: Eq[G] => SmtOperators
    case node: Exists[G] => SmtOperators
    case node: FloorDiv[G] => SmtOperators
    case node: Forall[G] => SmtOperators
    case node: Greater[G] => SmtOperators
    case node: GreaterEq[G] => SmtOperators
    case node: Implies[G] => SmtOperators
    case node: IntegerValue[G] => SmtOperators
    case node: Less[G] => SmtOperators
    case node: LessEq[G] => SmtOperators
    case node: Let[G] => SmtOperators
    case node: Local[G] => SmtOperators
    case node: Minus[G] => SmtOperators
    case node: Mod[G] => SmtOperators
    case node: Mult[G] => SmtOperators
    case node: Neq[G] => SmtOperators
    case node: NoPerm[G] => SmtOperators
    case node: Not[G] => SmtOperators
    case node: Or[G] => SmtOperators
    case node: Plus[G] => SmtOperators
    case node: Select[G] => SmtOperators
    case node: SilverIntToRat[G] => SmtOperators
    case node: UMinus[G] => SmtOperators
    case node: WritePerm[G] => SmtOperators

    case node: TBool[G] => SmtTypes
    case node: TInt[G] => SmtTypes
    case node: TRational[G] => SmtTypes

    case node: SpecIgnoreStart[G] => SpecIgnore
    case node: SpecIgnoreEnd[G] => SpecIgnore

    case node: Any[G] => StarSubscript

    case node: Cons[G] => SugarCollectionOperator
    case node: Head[G] => SugarCollectionOperator
    case node: Tail[G] => SugarCollectionOperator
    case node: Slice[G] => SugarCollectionOperator
    case node: RemoveAt[G] => SugarCollectionOperator
    case node: Empty[G] => SugarCollectionOperator
    case node: SubSetEq[G] => SugarCollectionOperator
    case node: SubBagEq[G] => SugarCollectionOperator

    case node: PointsTo[G] => SugarPermissionOperator
    case node: Scale[G] => SugarPermissionOperator

    case node: Switch[G] => SwitchStatement
    case node: DefaultCase[G] => SwitchStatement
    case node: Case[G] => SwitchStatement

    case node: DecreasesClauseAssume[G] => TerminationMeasure
    case node: DecreasesClauseNoRecursion[G] => TerminationMeasure
    case node: DecreasesClauseTuple[G] => TerminationMeasure

    case node: SimplificationRule[G] => TermRewriteRules
    case node: FunctionOf[G] => TermRewriteRules

    case node: TChar[G] => TextTypes
    case node: TString[G] => TextTypes

    case node: TryCatchFinally[G] => TryCatchStatement
    case node: CatchClause[G] => TryCatchStatement

    case node: TypeValue[G] => TypeValuesAndGenerics
    case node: TypeOf[G] => TypeValuesAndGenerics
    case node: InstanceOf[G] => TypeValuesAndGenerics
    case node: Cast[G] => TypeValuesAndGenerics
    case node: SubType[G] => TypeValuesAndGenerics
    case node: SuperType[G] => TypeValuesAndGenerics
    case node: TType[G] => TypeValuesAndGenerics
    case node: TVar[G] => TypeValuesAndGenerics

    case node: LocalDecl[G] => UnscopedDeclaration

    case node: Wait[G] => WaitNotify
    case node: Notify[G] => WaitNotify

    case node: ReadPerm[G] => WildcardReadPermission

    case node: AmbiguousComputationalAnd[G] =>
      return AmbiguousOperators +: (
        if(node.isBoolOp) Seq(ComputationalLogicOperator)
        else Seq(BitOperators)
      )
    case node: AmbiguousComputationalOr[G] =>
      return AmbiguousOperators +: (
        if (node.isBoolOp) Seq(ComputationalLogicOperator)
        else Seq(BitOperators)
        )
    case node: AmbiguousComputationalXor[G] =>
      return AmbiguousOperators +: (
        if (node.isBoolOp) Seq(ComputationalLogicOperator)
        else Seq(BitOperators)
        )
    case node: AmbiguousMult[G] =>
      return AmbiguousOperators +: (
        if(node.isProcessOp) Seq(Models)
        else Seq(SmtOperators)
      )
    case node: AmbiguousPlus[G] =>
      return AmbiguousOperators +: (
        if(node.isProcessOp) Seq(Models)
        else if(node.isPointerOp) Seq(Pointers)
        else Seq(SmtOperators)
      )
    case node: AmbiguousMinus[G] => AmbiguousOperators
    case node: AmbiguousOr[G] =>
      return AmbiguousOperators +: (
        if(node.isProcessOp) Seq(Models)
        else Seq(SmtOperators)
      )
    case node: AmbiguousSubscript[G] =>
      return AmbiguousOperators +: (
        if (node.isPointerOp) Seq(Pointers)
        else if (node.isMapOp) Seq(AxiomaticLibraryType)
        else if (node.isArrayOp) Seq(Arrays)
        else Seq(SilverAxiomaticLibraryType)
      )
    case node: AmbiguousMember[G] =>
      return AmbiguousOperators +: (
        if (node.isMapOp) Seq(AxiomaticLibraryType)
        else Seq(SilverAxiomaticLibraryType)
      )

    case node: Return[G] =>
      returnValues :+= node.result
      if (node.result == Void[G]()(DiagnosticOrigin)) return Seq(Methods)
      else return Seq(NonVoidReturn, Methods)
    case node: Branch[G] =>
      node.branches match {
        case Seq((_, _), (BooleanValue(true), _)) => BasicStatement
        case _ => NonTrivialBranch
      }
    case node: Loop[G] => node match {
      case Loop(Block(Nil), _, Block(Nil), _, _) => WhileLoops
      case _ => NonWhileLoop
    }
    case node: Function[G] =>
      return Seq(SmtDeclarations) ++
        (if (node.inline) Seq(ApplicableToBeInlined) else Nil) ++
        (if (node.typeArgs.nonEmpty) Seq(TypeValuesAndGenerics) else Nil)
    case node: Procedure[G] =>
      returnTypes :+= node.returnType
      return Seq(Methods) ++
        (if (node.inline) Seq(ApplicableToBeInlined) else Nil) ++
        (if (node.typeArgs.nonEmpty) Seq(TypeValuesAndGenerics) else Nil) ++
        (if (node.pure) Seq(MethodToBePurified) else Nil)
    case node: Predicate[G] =>
      return Seq(Resources) ++
        (if (node.inline) Seq(ApplicableToBeInlined) else Nil)
    case node: InstanceFunction[G] =>
      return Seq(Classes) ++
        (if (node.typeArgs.nonEmpty) Seq(TypeValuesAndGenerics) else Nil) ++
        (if (node.inline) Seq(ApplicableToBeInlined) else Nil)
    case node: InstanceMethod[G] =>
      returnTypes :+= node.returnType
      return Seq(Classes, Methods) ++
        (if (node.typeArgs.nonEmpty) Seq(TypeValuesAndGenerics) else Nil) ++
        (if (node.inline) Seq(ApplicableToBeInlined) else Nil) ++
        (if (node.pure) Seq(MethodToBePurified) else Nil)
    case node: InstancePredicate[G] =>
      return Seq(Classes) ++
        (if (node.inline) Seq(ApplicableToBeInlined) else Nil)
    case node: Label[G] => node.stat match {
      case Block(Nil) => Gotos
      case _ => return Seq(NonTrivialLabel, Gotos)
    }
    case node: TVoid[G] =>
      if (returnTypes.exists(_ eq node)) return Nil
      else AxiomaticLibraryType
    case node: Void[G] =>
      if (returnValues.exists(_ eq node)) return Nil
      else AxiomaticLibraryType

    case node: ValidArray[G] => return Seq(SugarPermissionOperator, Arrays)
    case node: ValidMatrix[G] => return Seq(SugarPermissionOperator, Arrays)
    case node: PermPointer[G] => return Seq(SugarPermissionOperator, Pointers)
    case node: PermPointerIndex[G] => return Seq(SugarPermissionOperator, Pointers)
    case node: CCast[G] => return Seq(CSpecific, TypeValuesAndGenerics)
    case node: CChar[G] => return Seq(CSpecific, TextTypes)
    case node: CDeclarationStatement[G] => return Seq(CSpecific, UnscopedDeclaration)
    case node: CGoto[G] => return Seq(CSpecific, Gotos)
    case node: CPointer[G] => return Seq(CSpecific, Pointers)
    case node: CPointerDeclarator[G] => return Seq(CSpecific, Pointers)
    case node: Result[G] => return scanFlatly(node.applicable.decl)
    case node: SilverNewRef[G] => return Seq(Assignment, Resources)
  })
}
