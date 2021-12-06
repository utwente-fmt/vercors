package vct.col.feature

import vct.col.ast._
import vct.col.origin.DiagnosticOrigin
import vct.col.util.AstBuildHelpers._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class FeatureRainbow {
  val features: mutable.Set[Feature] = mutable.Set()
  val examples: mutable.Map[Feature, ArrayBuffer[Node]] = mutable.Map()

  private val topLevelInvocations: mutable.Set[Invocation] = mutable.Set()

  def scan(node: Node): Unit =
    node.transSubnodes.foreach(node => scanFlatly(node).foreach(f => {
      features += f
      examples.getOrElseUpdate(f, ArrayBuffer()) += node
    }))

  def scanFlatly(node: Node): Seq[Feature] = Seq(node match {
    case node: Program => return Nil
    case node: IntegerValue => return Nil
    case node: BooleanValue => return Nil
    case node: LiteralSeq => return Nil
    case node: LiteralSet => return Nil
    case node: LiteralBag => return Nil
    case node: LiteralTuple => AxiomaticLibraryType
    case node: LiteralMap => AxiomaticLibraryType
    case node: UntypedLiteralSeq => DynamicallyTypedCollection
    case node: UntypedLiteralSet => DynamicallyTypedCollection
    case node: UntypedLiteralBag => DynamicallyTypedCollection
    case node: Void => return Nil
    case node: AmbiguousThis => ContextSensitiveNode
    case node: AmbiguousResult => ContextSensitiveNode
    case node: ThisObject => return Nil
    case node: ThisModel => return Nil
    case node: Result => return Nil
    case node: CurrentThreadId => CurrentThread
    case node: Null => return Nil
    case node: Any => StarSubscript
    case node: NoPerm => return Nil
    case node: ReadPerm => WildcardReadPermission
    case node: WritePerm => return Nil
    case node: Range => SequenceRange
    case node: Values => BuiltinArrayOperators
    case node: OptSome => AxiomaticLibraryType
    case node: OptNone => AxiomaticLibraryType
    case node: EitherLeft => AxiomaticLibraryType
    case node: EitherRight => AxiomaticLibraryType
    case node: MapCons => AxiomaticLibraryType
    case node: MapEq => AxiomaticLibraryType
    case node: MapDisjoint => AxiomaticLibraryType
    case node: MapKeySet => AxiomaticLibraryType
    case node: MapValueSet => AxiomaticLibraryType
    case node: MapItemSet => AxiomaticLibraryType
    case node: MapSize => AxiomaticLibraryType
    case node: MapRemove => AxiomaticLibraryType
    case node: Forall => return Nil
    case node: Starall => return Nil
    case node: Exists => return Nil
    case node: Sum => NumericReductionOperator
    case node: Product => NumericReductionOperator
    case node: Let => return Nil
    case node: InlinePattern => InlineQuantifierPattern
    case node: Local => return Nil
    case node: Deref => Classes
    case node: ModelDeref => Models
    case node: DerefPointer => Pointers
    case node: AddrOf => Pointers
    case node: PredicateApply => return Nil
    case node: InstancePredicateApply => Classes
    case node: ADTFunctionInvocation => return Nil
    case node: ProcedureInvocation => return Nil
    case node: FunctionInvocation => return Nil
    case node: MethodInvocation => return Nil
    case node: InstanceFunctionInvocation => Classes
    case node: UMinus => return Nil
    case node: BitNot => BitOperators
    case node: Not => return Nil
    case node: AmbiguousMult =>
      return AmbiguousOperators +: (
        if(node.isProcessOp) Seq(Models)
        else Nil
      )
    case node: AmbiguousPlus =>
      return AmbiguousOperators +: (
        if(node.isProcessOp) Seq(Models)
        else if(node.isPointerOp) Seq(Pointers)
        else Nil
      )
    case node: AmbiguousOr =>
      return AmbiguousOperators +: (
        if(node.isProcessOp) Seq(Models)
        else Nil
      )
    case node: BitOp =>
      return AmbiguousOperators +: (
        if(node.isBoolOp) Nil
        else Seq(BitOperators)
      )
    case node: ComputationalOr => ComputationalLogicOperator
    case node: ComputationalXor => ComputationalLogicOperator
    case node: ComputationalAnd => ComputationalLogicOperator
    case node: Exp => Exponents
    case node: Plus => return Nil
    case node: Minus => return Nil
    case node: Mult => return Nil
    case node: Div => return Nil
    case node: FloorDiv => return Nil
    case node: Mod => return Nil
    case node: BitAnd => BitOperators
    case node: BitOr => BitOperators
    case node: BitXor => BitOperators
    case node: BitShl => BitOperators
    case node: BitShr => BitOperators
    case node: BitUShr => BitOperators
    case node: And => return Nil
    case node: Or => return Nil
    case node: Implies => return Nil
    case node: Star => return Nil
    case node: Wand => MagicWand
    case node: Scale => return Nil
    case node: Unfolding => return Nil
    case node: Perm => return Nil
    case node: HPerm => Models
    case node: APerm => Models
    case node: PointsTo => SugarPermissionOperator
    case node: CurPerm => return Nil
    case node: ValidArray => return Seq(SugarPermissionOperator, Arrays)
    case node: ValidMatrix => return Seq(SugarPermissionOperator, Arrays)
    case node: PermPointer => return Seq(SugarPermissionOperator, Pointers)
    case node: PermPointerIndex => return Seq(SugarPermissionOperator, Pointers)
    case node: Eq => return Nil
    case node: Neq => return Nil
    case node: Greater => return Nil
    case node: Less => return Nil
    case node: GreaterEq => return Nil
    case node: LessEq => return Nil
    case node: Select => return Nil
    case node: NewObject => Classes
    case node: NewArray => Arrays
    case node: Old => return Nil
    case node: AmbiguousSubscript =>
      return AmbiguousOperators +: (
        if(node.isPointerOp) Seq(Pointers)
        else if(node.isMapOp) Seq(AxiomaticLibraryType)
        else if(node.isArrayOp) Seq(Arrays)
        else Nil
      )
    case node: SeqSubscript => return Nil
    case node: ArraySubscript => Arrays
    case node: PointerAdd => Pointers
    case node: PointerSubscript => Pointers
    case node: Length => Arrays
    case node: Size => return Nil
    case node: Cons => SugarCollectionOperator
    case node: Head => SugarCollectionOperator
    case node: Tail => SugarCollectionOperator
    case node: Drop => return Nil
    case node: Take => return Nil
    case node: Slice => SugarCollectionOperator
    case node: SeqUpdate => return Nil
    case node: Concat => return Nil
    case node: RemoveAt => SugarCollectionOperator
    case node: Empty => SugarCollectionOperator
    case node: AmbiguousMember =>
      return AmbiguousOperators +: (
        if(node.isMapOp) Seq(AxiomaticLibraryType)
        else Nil
      )
    case node: SetMember => return Nil
    case node: SeqMember => return Nil
    case node: MapMember => AxiomaticLibraryType
    case node: BagMemberCount => return Nil
    case node: SubSet => return Nil
    case node: SubSetEq => SugarCollectionOperator
    case node: Permutation => PermutationOperator
    case node: OptGet => AxiomaticLibraryType
    case node: OptGetOrElse => AxiomaticLibraryType
    case node: GetLeft => AxiomaticLibraryType
    case node: GetRight => AxiomaticLibraryType
    case node: IsLeft => AxiomaticLibraryType
    case node: IsRight => AxiomaticLibraryType
    case node: MapGet => AxiomaticLibraryType
    case node: TupGet => AxiomaticLibraryType
    case node: VectorSum => MatrixVector
    case node: VectorCompare => MatrixVector
    case node: VectorRepeat => MatrixVector
    case node: MatrixSum => MatrixVector
    case node: MatrixCompare => MatrixVector
    case node: MatrixRepeat => MatrixVector
    case node: TypeValue => TypeValuesAndGenerics
    case node: TypeOf => TypeValuesAndGenerics
    case node: InstanceOf => TypeValuesAndGenerics
    case node: Cast => TypeValuesAndGenerics
    case node: SubType => TypeValuesAndGenerics
    case node: SuperType => TypeValuesAndGenerics
    case node: PreAssignExpression => ExpressionWithSideEffects
    case node: PostAssignExpression => ExpressionWithSideEffects
    case node: With => ExpressionWithSideEffects
    case node: Then => ExpressionWithSideEffects
    case node: Held => IntrinsicLocks
    case node: IdleToken => JavaThreads
    case node: JoinToken => JavaThreads
    case node: EmptyProcess => Models
    case node: ActionApply => Models
    case node: ProcessApply => Models
    case node: ProcessSeq => Models
    case node: ProcessChoice => Models
    case node: ProcessPar => Models
    case node: ProcessSelect => Models
    case node: ModelNew => Models
    case node: ModelState => Models
    case node: ModelAbstractState => Models
    case node: ModelCreate => Models
    case node: ModelDestroy => Models
    case node: ModelSplit => Models
    case node: ModelMerge => Models
    case node: ModelChoose => Models
    case node: ModelPerm => Models
    case node: ActionPerm => Models
    case node: Eval => node.expr match {
      case node: MethodInvocation =>
        topLevelInvocations += node
        return Nil
      case node: ProcedureInvocation =>
        topLevelInvocations += node
        return Nil
      // Function invocations do not count as a statement
      case _ => NonMethodInvocationEvaluation
    }
    case node: LocalDecl => UnscopedDeclaration
    case node: Return =>
      if(node.result == Void()(DiagnosticOrigin)) return Nil
      else return Seq(NonVoidReturn)
    case node: Assign => return Nil
    case node: Block => return Nil
    case node: Scope => return Nil
    case node: LoopInvariant => return Nil
    case node: IterationContract => LoopIterationContract
    case node: Branch =>
      node.branches match {
        case Seq((_, _), (`tt`, _)) => return Nil
        case _ => NonTrivialBranch
      }
    case node: Switch => SwitchStatement
    case node: Loop => node match {
      case Loop(Block(Nil), _, Block(Nil), _, _) => return Nil
      case _ => NonWhileLoop
    }
    case node: CatchClause => return Nil
    case node: TryCatchFinally => TryCatchStatement
    case node: Synchronized => IntrinsicLocks
    case node: ParInvariant => ParallelRegion
    case node: ParAtomic => ParallelRegion
    case node: ParBarrier => ParallelRegion
    case node: IterVariable => return Nil
    case node: ParBlock => ParallelRegion
    case node: ParParallel => ParallelRegion
    case node: ParRegion => ParallelRegion
    case node: VecBlock => ParallelRegion
    case node: ParStatement => ParallelRegion
    case node: Send => SendRecv
    case node: Recv => SendRecv
    case node: DefaultCase => SwitchStatement
    case node: Case => SwitchStatement
    case node: Label => node.stat match {
      case Block(Nil) => return Nil
      case _ => NonTrivialLabel
    }
    case node: Goto => return Nil
    case node: Exhale => return Nil
    case node: Assert => return Nil
    case node: Refute => return Nil
    case node: Inhale => return Nil
    case node: Assume => return Nil
    case node: SpecIgnoreStart => SpecIgnore
    case node: SpecIgnoreEnd => SpecIgnore
    case node: Throw => Exceptions
    case node: Wait => WaitNotify
    case node: Notify => WaitNotify
    case node: Fork => JavaThreads
    case node: Join => JavaThreads
    case node: Lock => IntrinsicLocks
    case node: Unlock => IntrinsicLocks
    case node: Fold => return Nil
    case node: Unfold => return Nil
    case node: WandCreate => MagicWand
    case node: WandQed => MagicWand
    case node: WandApply => MagicWand
    case node: WandUse => MagicWand
    case node: ModelDo => Models
    case node: Havoc => return Nil
    case node: Break => ExceptionalLoopControl
    case node: Continue => ExceptionalLoopControl
    case node: TNotAValue => return Nil
    case node: TAny => ExoticTypes
    case node: TNothing => ExoticTypes
    case node: TUnion => ExoticTypes
    case node: TVoid => return Nil
    case node: TNull => return Nil
    case node: TBool => return Nil
    case node: TResource => return Nil
    case node: TInt => return Nil
    case node: TBoundedInt => return Nil
    case node: TFloat => return Nil
    case node: TRational => return Nil
    case node: TFraction => AxiomaticLibraryType
    case node: TZFraction => AxiomaticLibraryType
    case node: TChar => TextTypes
    case node: TString => TextTypes
    case node: TRef => return Nil
    case node: TOption => AxiomaticLibraryType
    case node: TEither => AxiomaticLibraryType
    case node: TTuple => AxiomaticLibraryType
    case node: TSeq => return Nil
    case node: TSet => return Nil
    case node: TBag => return Nil
    case node: TMatrix => MatrixVector
    case node: TArray => Arrays
    case node: TPointer => Pointers
    case node: TMap => AxiomaticLibraryType
    case node: TProcess => Models
    case node: TModel => Models
    case node: TClass => Classes
    case node: TAxiomatic => return Nil
    case node: TType => TypeValuesAndGenerics
    case node: TVar => TypeValuesAndGenerics
    case node: Variable => return Nil
    case node: LabelDecl => return Nil
    case node: ParBlockDecl => ParallelRegion
    case node: ParInvariantDecl => ParallelRegion
    case node: SimplificationRule => TermRewriteRules
    case node: AxiomaticDataType => return Nil
    case node: ADTAxiom => return Nil
    case node: SignalsClause => Exceptions
    case node: ApplicableContract => return Nil
    case node: Function =>
      return (if(node.inline) Seq(ApplicableToBeInlined) else Nil) ++
        (if(node.typeArgs.nonEmpty) Seq(TypeValuesAndGenerics) else Nil)
    case node: Procedure =>
      return (if(node.inline) Seq(ApplicableToBeInlined) else Nil) ++
        (if(node.typeArgs.nonEmpty) Seq(TypeValuesAndGenerics) else Nil) ++
        (if(node.pure) Seq(MethodToBePurified) else Nil)
    case node: Predicate =>
      return (if(node.inline) Seq(ApplicableToBeInlined) else Nil)
    case node: InstanceFunction =>
      return Seq(Classes) ++
        (if(node.typeArgs.nonEmpty) Seq(TypeValuesAndGenerics) else Nil) ++
        (if(node.inline) Seq(ApplicableToBeInlined) else Nil)
    case node: InstanceMethod =>
      return Seq(Classes) ++
        (if(node.typeArgs.nonEmpty) Seq(TypeValuesAndGenerics) else Nil) ++
        (if(node.inline) Seq(ApplicableToBeInlined) else Nil) ++
        (if(node.pure) Seq(MethodToBePurified) else Nil)
    case node: InstancePredicate =>
      return Seq(Classes) ++
        (if(node.inline) Seq(ApplicableToBeInlined) else Nil)
    case node: ADTFunction => return Nil
    case node: Final => return Nil
    case node: InstanceField => FinalField
    case node: Class => Classes
    case node: ModelField => Models
    case node: ModelProcess => Models
    case node: ModelAction => Models
    case node: Model => Models
    case node: SilverPredicateAccess => return Nil
    case node: SilverDeref => return Nil
    case node: SilverPerm => return Nil
    case node: SilverPredPerm => return Nil
    case node: SilverUnfolding => return Nil
    case node: SilverCurFieldPerm => return Nil
    case node: SilverCurPredPerm => return Nil
    case node: SilverUnfold => return Nil
    case node: SilverFold => return Nil
    case node: SilverWhile => return Nil
    case node: SilverIf => return Nil
    case node: SilverNewRef => return Nil
    case node: SilverFieldAssign => return Nil
    case node: SilverLocalAssign => return Nil
    case node: SilverField => return Nil
    case node: CPure => return Nil
    case node: CInline => return Nil
    case node: CTypedef => return Nil
    case node: CExtern => return Nil
    case node: CStatic => return Nil
    case node: CVoid => return Nil
    case node: CChar => return Nil
    case node: CShort => return Nil
    case node: CInt => return Nil
    case node: CLong => return Nil
    case node: CFloat => return Nil
    case node: CDouble => return Nil
    case node: CSigned => return Nil
    case node: CUnsigned => return Nil
    case node: CBool => return Nil
    case node: CTypedefName => return Nil
    case node: CSpecificationType => return Nil
    case node: CTypeQualifierDeclarationSpecifier => return Nil
    case node: CConst => return Nil
    case node: CRestrict => return Nil
    case node: CVolatile => return Nil
    case node: CAtomic => return Nil
    case node: CKernel => return Nil
    case node: CPointer => return Nil
    case node: CParam => return Nil
    case node: CPointerDeclarator => return Nil
    case node: CArrayDeclarator => return Nil
    case node: CTypedFunctionDeclarator => return Nil
    case node: CAnonymousFunctionDeclarator => return Nil
    case node: CName => return Nil
    case node: CInit => return Nil
    case node: CDeclaration => return Nil
    case node: CFunctionDefinition => return Nil
    case node: CGlobalDeclaration => return Nil
    case node: CDeclarationStatement => return Nil
    case node: CGoto => return Nil
    case node: GpgpuLocalBarrier => return Nil
    case node: GpgpuGlobalBarrier => return Nil
    case node: GpgpuAtomic => return Nil
    case node: CLocal => return Nil
    case node: CInvocation => return Nil
    case node: CStructAccess => return Nil
    case node: CStructDeref => return Nil
    case node: GpgpuCudaKernelInvocation => return Nil
    case node: CPrimitiveType => return Nil
    case node: JavaName => return Nil
    case node: JavaImport => return Nil
    case node: JavaPublic => return Nil
    case node: JavaProtected => return Nil
    case node: JavaPrivate => return Nil
    case node: JavaStatic => return Nil
    case node: JavaAbstract => return Nil
    case node: JavaFinal => return Nil
    case node: JavaStrictFP => return Nil
    case node: JavaNative => return Nil
    case node: JavaSynchronized => return Nil
    case node: JavaTransient => return Nil
    case node: JavaVolatile => return Nil
    case node: JavaPure => return Nil
    case node: JavaInline => return Nil
    case node: JavaNamespace => return Nil
    case node: JavaClass => return Nil
    case node: JavaInterface => return Nil
    case node: JavaSharedInitialization => return Nil
    case node: JavaFields => return Nil
    case node: JavaConstructor => return Nil
    case node: JavaMethod => return Nil
    case node: JavaLocalDeclaration => return Nil
    case node: JavaLocalDeclarationStatement => return Nil
    case node: JavaNamedType => return Nil
    case node: JavaTClass => return Nil
    case node: JavaLocal => return Nil
    case node: JavaDeref => return Nil
    case node: JavaLiteralArray => return Nil
    case node: JavaInvocation => return Nil
    case node: JavaNewClass => return Nil
    case node: JavaNewLiteralArray => return Nil
    case node: JavaNewDefaultArray => return Nil
    case node: PVLNamedType => return Nil
    case node: PVLLocal => return Nil
    case node: PVLDeref => return Nil
    case node: PVLInvocation => return Nil
    case node: PVLNew => return Nil
    case node: PVLConstructor => return Nil
    case node: Commit => IntrinsicLocks
  })
}
