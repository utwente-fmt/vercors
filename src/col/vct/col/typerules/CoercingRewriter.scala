package vct.col.typerules

import com.typesafe.scalalogging.LazyLogging
import hre.util.FuncTools
import vct.col.ast._
import vct.col.ast.`type`.TFloats
import vct.col.origin._
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap
import vct.result.VerificationError.{SystemError, Unreachable}

case class NopCoercingRewriter[Pre <: Generation]() extends CoercingRewriter[Pre]() {
  override def applyCoercion(e: => Expr[Post], coercion: Coercion[Pre])(implicit o: Origin): Expr[Post] = e
}

case object CoercingRewriter {
  sealed trait CoercionError extends SystemError {
    override def text: String =
      messageContext(
        "Internal type error: CoercionErrors must not bubble. " + (this match {
          case IncoercibleDummy => "(No alternative matched, see stack trace)"
          case Incoercible(e, target) => s"Expression `$e` could not be coerced to `$target``"
          case IncoercibleText(e, target) => s"Expression `$e` could not be coerced to $target."
          case IncoercibleExplanation(e, message) => s"At `$e`: $message"
          case WrongType(n, expectedType, actualType) => s"$n was expected to have type $expectedType, but turned out to have type $actualType"
        })
      )
  }

  case object IncoercibleDummy extends CoercionError

  case class Incoercible(e: Expr[_], target: Type[_]) extends CoercionError

  case class IncoercibleText(e: Expr[_], targetText: String) extends CoercionError

  case class IncoercibleExplanation(blame: Node[_], message: String) extends CoercionError

  case class WrongType(n: Node[_], expectedType: Type[_], actualType: Type[_]) extends CoercionError

  private def coercionOrigin(of: Expr[_]): Origin = of.o.where(name = "unknown")
}

abstract class CoercingRewriter[Pre <: Generation]() extends AbstractRewriter[Pre, Rewritten[Pre]] with LazyLogging {
  import CoercingRewriter._

  type Post = Rewritten[Pre]

  val coercedDeclaration: SuccessionMap[Declaration[Pre], Declaration[Pre]] = SuccessionMap()

  class CoercedSuccessorsProvider extends SuccessorsProviderTrafo[Pre, Pre](null) {
    override def preTransform[I <: Declaration[Pre], O <: Declaration[Pre]](pre: I): Option[O] =
      Some(coercedDeclaration(pre).asInstanceOf[O])
  }

  override def succProvider: SuccessorsProvider[Pre, Post] =
    SuccessorsProviderChain(new CoercedSuccessorsProvider, allScopes.freeze)

  /**
    * Apply a particular coercion to an expression.
    * SAFETY: all promoting coercions must be injective; otherwise the default mapping coercion of sets is unsound.
    * @param e the expression to coerce
    * @param coercion the coercion
    * @return the coerced expression
    */
  def applyCoercion(e: => Expr[Post], coercion: Coercion[Pre])(implicit o: Origin): Expr[Post] = {
    coercion match {
      case CoerceIdentity(_) => e
      case CoercionSequence(cs) => cs.foldLeft(e) { case (e, c) => applyCoercion(e, c) }
      case CoerceNothingSomething(_) => e
      case CoerceSomethingAny(_) => e
      case CoerceSomethingAnyValue(_) => e
      case CoerceMapOption(inner, _, target) =>
        Select(OptEmpty(e), OptNoneTyped(dispatch(target)), OptSomeTyped(dispatch(target), applyCoercion(OptGet(e)(NeverNone), inner)))
      case CoerceMapEither((innerLeft, innerRight), _, _) =>
        Select(IsRight(e),
          EitherRight(applyCoercion(GetRight(e)(FramedGetRight), innerRight)),
          EitherLeft(applyCoercion(GetLeft(e)(FramedGetLeft), innerLeft)),
        )
      case CoerceMapSeq(inner, source, target) =>
        val f: Function[Post] = withResult((result: Result[Post]) => {
          val v = new Variable[Post](TSeq(dispatch(source)))
          val i = new Variable[Post](TInt())
          val result_i = SeqSubscript(result, i.get)(FramedSeqIndex)
          val v_i = SeqSubscript(v.get, i.get)(FramedSeqIndex)

          function(
            blame = AbstractApplicable,
            contractBlame = TrueSatisfiable,
            returnType = TSeq(dispatch(target)),
            args = Seq(v),
            ensures = UnitAccountedPredicate(
              Eq(Size(v.get), Size(result)) &&
              Forall(Seq(i), Seq(Seq(result_i)),
                (const[Post](0) <= i.get && i.get < Size(result)) ==>
                  (result_i === applyCoercion(v_i, inner)))
            ),
          )
        })

        globalDeclarations.declare(f)
        FunctionInvocation[Post](f.ref, Seq(e), Nil, Nil, Nil)(PanicBlame("default coercion for seq<_> requires nothing."))
      case CoerceMapSet(inner, source, target) =>
        val f: Function[Post] = withResult((result: Result[Post]) => {
          val v = new Variable(TSet(dispatch(source)))
          val elem = new Variable(dispatch(source))

          function(
            blame = AbstractApplicable,
            contractBlame = TrueSatisfiable,
            returnType = TSet(dispatch(target)),
            args = Seq(v),
            ensures = UnitAccountedPredicate(
              Eq(Size(result), Size(v.get)) &&
                Forall(Seq(elem), Seq(Seq(SetMember(elem.get, result))),
                  Eq(SetMember(applyCoercion(elem.get, inner), result), SetMember(elem.get, v.get)))
            ),
          )
        })

        globalDeclarations.declare(f)
        FunctionInvocation[Post](f.ref, Seq(e), Nil, Nil, Nil)(PanicBlame("Default coercion for set<_> requires nothing."))
      case CoerceMapBag(inner, source, target) =>
        val f: Function[Post] = withResult((result: Result[Post]) => {
          val v = new Variable(TBag(dispatch(source)))
          val elem = new Variable(dispatch(source))

          function(
            blame = AbstractApplicable,
            contractBlame = TrueSatisfiable,
            returnType = TBag(dispatch(target)),
            args = Seq(v),
            ensures = UnitAccountedPredicate(
              Eq(Size(result), Size(v.get)) &&
                Forall(Seq(elem), Seq(Seq(BagMemberCount(elem.get, result))),
                  Eq(BagMemberCount(applyCoercion(elem.get, inner), result), BagMemberCount(elem.get, v.get)))
            ),
          )
        })

        globalDeclarations.declare(f)
        FunctionInvocation[Post](f.ref, Seq(e), Nil, Nil, Nil)(PanicBlame("Default coercion for bag<_> requires nothing."))
      case CoerceMapMatrix(inner, source, target) =>
        ???
      case CoerceMapMap(inner, (sourceKey, sourceValue), (targetKey, targetValue)) =>
        val f: Function[Post] = withResult((result: Result[Post]) => {
          val v = new Variable(TMap(dispatch(sourceKey), dispatch(sourceValue)))
          val k = new Variable(dispatch(sourceKey))

          function(
            blame = AbstractApplicable,
            contractBlame = TrueSatisfiable,
            returnType = TMap(dispatch(targetKey), dispatch(targetValue)),
            args = Seq(v),
            ensures = UnitAccountedPredicate(
              Eq(MapKeySet(result), MapKeySet(v.get)) &&
                Forall(Seq(k), Seq(Seq(MapGet(result, k.get)(TriggerPatternBlame))),
                  SetMember(k.get, MapKeySet(result)) ==> Eq(MapGet(result, k.get)(FramedMapGet), MapGet(v.get, k.get)(FramedMapGet)))
            ),
          )
        })

        globalDeclarations.declare(f)
        FunctionInvocation[Post](f.ref, Seq(e), Nil, Nil, Nil)(PanicBlame("Default coercion for map<_, _> requires nothing."))
      case CoerceMapTuple(inner, sourceTypes, targetTypes) =>
        LiteralTuple(targetTypes.map(dispatch), inner.zipWithIndex.map { case (c, i) => applyCoercion(TupGet(e, i), c) })
      case CoerceMapType(inner, source, target) =>
        ???

      case CoerceBoolResource() => e
      case CoerceResourceResourceVal() => e
      case CoerceResourceValResource() => e
      case CoerceBoundIntFrac() => e
      case CoerceBoundIntZFrac(_) => e
      case CoerceBoundIntFloat(_, _) => e
      case CoerceJoinUnion(_, _, _) => e
      case CoerceSelectUnion(inner, _, _, _) => applyCoercion(e, inner)

      case CoerceSupports(_, _) => e
      case CoerceClassAnyClass(_) => e
      case CoerceJavaSupports(_, _) => e
      case CoerceJavaClassAnyClass(_) => e
      case CoerceCPrimitiveToCol(_, _) => e
      case CoerceColToCPrimitive(_, _) => e
      case CoerceCPPPrimitiveToCol(_, _) => e
      case CoerceColToCPPPrimitive(_, _) => e
      case CoerceNullRef() => e
      case CoerceNullArray(_) => e
      case CoerceNullClass(_) => e
      case CoerceNullJavaClass(_) => e
      case CoerceNullAnyClass() => e
      case CoerceNullPointer(_) => e
      case CoerceFracZFrac() => e
      case CoerceZFracRat() => e
      case CoerceFloatRat(_) => e
      case CoerceIncreasePrecision(_, _) => e
      case CoerceWidenBound(_, _) => e
      case CoerceUnboundInt(_) => e
      case CoerceCArrayPointer(_) => e
      case CoerceCPPArrayPointer(_) => e
      case CoerceNullEnum(_) => e

      case CoerceIntRat() => e
      case CoerceRatZFrac() => e
      case CoerceZFracFrac() => e
    }
  }

  def coerceAny(node: NodeFamily[Pre]): NodeFamily[Pre] = node match {
    case node: Verification[Pre] => node
    case node: VerificationContext[Pre] => node
    case node: Program[Pre] => node
    case node: Statement[Pre] => coerce(node)
    case node: Expr[Pre] => coerce(node)
    case node: Type[Pre] => node
    case node: DecreasesClause[Pre] => node
    case node: AccountedPredicate[Pre] => node
    case node: ApplicableContract[Pre] => node
    case node: LoopContract[Pre] => node
    case node: ParRegion[Pre] => coerce(node)
    case node: CatchClause[Pre] => node
    case node: SignalsClause[Pre] => node
    case node: FieldFlag[Pre] => node
    case node: IterVariable[Pre] => node
    case node: CDeclarator[Pre] => node
    case node: CDeclaration[Pre] => node
    case node: CDeclarationSpecifier[Pre] => node
    case node: CTypeQualifier[Pre] => node
    case node: CPointer[Pre] => node
    case node: CInit[Pre] => node
    case node: CPPDeclarator[Pre] => node
    case node: CPPDeclarationSpecifier[Pre] => node
    case node: CPPDeclaration[Pre] => node
    case node: CPPAddressing[Pre] => node
    case node: CPPInit[Pre] => node
    case node: CPPExprOrTypeSpecifier[Pre] => node
    case node: GpuMemoryFence[Pre] => node
    case node: JavaModifier[Pre] => node
    case node: JavaImport[Pre] => node
    case node: JavaName[Pre] => node
    case node: JavaVariableDeclaration[Pre] => node
    case node: Coercion[Pre] => node
    case node: Location[Pre] => node
    case node: Operator[Pre] => node
    case node: BipPortType[Pre] => node
    case node: JavaBipGlueName[Pre] => node
    case node: JavaBipGlueElement[Pre] => node
    case node: BipGlueRequires[Pre] => node
    case node: BipGlueAccepts[Pre] => node
    case node: BipGlueDataWire[Pre] => node
    case node: BipTransitionSignature[Pre] => node
    case node: LlvmFunctionContract[Pre] => node
    case node: LlvmLoopContract[Pre] => node
    case node: ProverLanguage[Pre] => node
    case node: SmtlibFunctionSymbol[Pre] => node
    case node: PVLCommunicateAccess[Pre] => node
    case node: PVLCommunicateSubject[Pre] => node
    case node: SeqRun[Pre] => node
    case node: Access[Pre] => node
    case node: Subject[Pre] => node
    case node: SeqGuard[Pre] => coerce(node)
  }

  def preCoerce(e: Expr[Pre]): Expr[Pre] = e
  def postCoerce(e: Expr[Pre]): Expr[Post] = rewriteDefault(e)
  override final def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case ApplyCoercion(e, coercion) => applyCoercion(dispatch(e), coercion)(e.o)
    case other => postCoerce(coerce(preCoerce(other)))
  }

  def preCoerce(stat: Statement[Pre]): Statement[Pre] = stat
  def postCoerce(stat: Statement[Pre]): Statement[Post] = rewriteDefault(stat)
  override final def dispatch(stat: Statement[Pre]): Statement[Post] =
    postCoerce(coerce(preCoerce(stat)))

  def preCoerce(decl: Declaration[Pre]): Declaration[Pre] = decl
  def postCoerce(decl: Declaration[Pre]): Unit = rewriteDefault(decl)
  override final def dispatch(decl: Declaration[Pre]): Unit = {
    val coercedDecl = coerce(preCoerce(decl))
    coercedDeclaration(decl) = coercedDecl
    postCoerce(coercedDecl)
  }

  def preCoerce(region: ParRegion[Pre]): ParRegion[Pre] = region
  def postCoerce(region: ParRegion[Pre]): ParRegion[Post] = rewriteDefault(region)
  override final def dispatch(region: ParRegion[Pre]): ParRegion[Post] =
    postCoerce(coerce(preCoerce(region)))



  def preCoerce(node: Verification[Pre]): Verification[Pre] = node
  def postCoerce(node: Verification[Pre]): Verification[Post] = rewriteDefault(node)
  override final def dispatch(node: Verification[Pre]): Verification[Post] = postCoerce(coerce(preCoerce(node)))

  def preCoerce(node: VerificationContext[Pre]): VerificationContext[Pre] = node
  def postCoerce(node: VerificationContext[Pre]): VerificationContext[Post] = rewriteDefault(node)
  override final def dispatch(node: VerificationContext[Pre]): VerificationContext[Post] = postCoerce(coerce(preCoerce(node)))

  def preCoerce(node: Program[Pre]): Program[Pre] = node
  def postCoerce(node: Program[Pre]): Program[Post] = rewriteDefault(node)
  override final def dispatch(node: Program[Pre]): Program[Post] = postCoerce(coerce(preCoerce(node)))

  def preCoerce(node: Type[Pre]): Type[Pre] = node
  def postCoerce(node: Type[Pre]): Type[Post] = rewriteDefault(node)
  override final def dispatch(node: Type[Pre]): Type[Post] = postCoerce(coerce(preCoerce(node)))

  def preCoerce(node: LoopContract[Pre]): LoopContract[Pre] = node
  def postCoerce(node: LoopContract[Pre]): LoopContract[Post] = rewriteDefault(node)
  override final def dispatch(node: LoopContract[Pre]): LoopContract[Post] = postCoerce(coerce(preCoerce(node)))

  def preCoerce(node: CatchClause[Pre]): CatchClause[Pre] = node
  def postCoerce(node: CatchClause[Pre]): CatchClause[Post] = rewriteDefault(node)
  override final def dispatch(node: CatchClause[Pre]): CatchClause[Post] = postCoerce(coerce(preCoerce(node)))

  def preCoerce(node: IterVariable[Pre]): IterVariable[Pre] = node
  def postCoerce(node: IterVariable[Pre]): IterVariable[Post] = rewriteDefault(node)
  override final def dispatch(node: IterVariable[Pre]): IterVariable[Post] = postCoerce(coerce(preCoerce(node)))

  def preCoerce(node: SignalsClause[Pre]): SignalsClause[Pre] = node
  def postCoerce(node: SignalsClause[Pre]): SignalsClause[Post] = rewriteDefault(node)
  override final def dispatch(node: SignalsClause[Pre]): SignalsClause[Post] = postCoerce(coerce(preCoerce(node)))

  def preCoerce(node: DecreasesClause[Pre]): DecreasesClause[Pre] = node
  def postCoerce(node: DecreasesClause[Pre]): DecreasesClause[Post] = rewriteDefault(node)
  override final def dispatch(node: DecreasesClause[Pre]): DecreasesClause[Post] = postCoerce(coerce(preCoerce(node)))

  def preCoerce(node: ApplicableContract[Pre]): ApplicableContract[Pre] = node
  def postCoerce(node: ApplicableContract[Pre]): ApplicableContract[Post] = rewriteDefault(node)
  override final def dispatch(node: ApplicableContract[Pre]): ApplicableContract[Post] = postCoerce(coerce(preCoerce(node)))

  def preCoerce(node: AccountedPredicate[Pre]): AccountedPredicate[Pre] = node
  def postCoerce(node: AccountedPredicate[Pre]): AccountedPredicate[Post] = rewriteDefault(node)
  override final def dispatch(node: AccountedPredicate[Pre]): AccountedPredicate[Post] = postCoerce(coerce(preCoerce(node)))

  def preCoerce(node: FieldFlag[Pre]): FieldFlag[Pre] = node
  def postCoerce(node: FieldFlag[Pre]): FieldFlag[Post] = rewriteDefault(node)
  override final def dispatch(node: FieldFlag[Pre]): FieldFlag[Post] = postCoerce(coerce(preCoerce(node)))

  override final def dispatch(node: Coercion[Pre]): Coercion[Post] = {
    throw Unreachable("Coercions are rewritten by the Expr dispatch")
  }


  def preCoerce(node: Location[Pre]): Location[Pre] = node
  def postCoerce(node: Location[Pre]): Location[Post] = rewriteDefault(node)
  override final def dispatch(node: Location[Pre]): Location[Post] = postCoerce(coerce(preCoerce(node)))

  def preCoerce(node: CDeclarationSpecifier[Pre]): CDeclarationSpecifier[Pre] = node
  def postCoerce(node: CDeclarationSpecifier[Pre]): CDeclarationSpecifier[Post] = rewriteDefault(node)
  override final def dispatch(node: CDeclarationSpecifier[Pre]): CDeclarationSpecifier[Post] = postCoerce(coerce(preCoerce(node)))

  def preCoerce(node: CTypeQualifier[Pre]): CTypeQualifier[Pre] = node
  def postCoerce(node: CTypeQualifier[Pre]): CTypeQualifier[Post] = rewriteDefault(node)
  override final def dispatch(node: CTypeQualifier[Pre]): CTypeQualifier[Post] = postCoerce(coerce(preCoerce(node)))

  def preCoerce(node: CPointer[Pre]): CPointer[Pre] = node
  def postCoerce(node: CPointer[Pre]): CPointer[Post] = rewriteDefault(node)
  override final def dispatch(node: CPointer[Pre]): CPointer[Post] = postCoerce(coerce(preCoerce(node)))

  def preCoerce(node: CDeclarator[Pre]): CDeclarator[Pre] = node
  def postCoerce(node: CDeclarator[Pre]): CDeclarator[Post] = rewriteDefault(node)
  override final def dispatch(node: CDeclarator[Pre]): CDeclarator[Post] = postCoerce(coerce(preCoerce(node)))

  def preCoerce(node: CInit[Pre]): CInit[Pre] = node
  def postCoerce(node: CInit[Pre]): CInit[Post] = rewriteDefault(node)
  override final def dispatch(node: CInit[Pre]): CInit[Post] = postCoerce(coerce(preCoerce(node)))

  def preCoerce(node: CDeclaration[Pre]): CDeclaration[Pre] = node
  def postCoerce(node: CDeclaration[Pre]): CDeclaration[Post] = rewriteDefault(node)
  override final def dispatch(node: CDeclaration[Pre]): CDeclaration[Post] = postCoerce(coerce(preCoerce(node)))

  def preCoerce(node: GpuMemoryFence[Pre]): GpuMemoryFence[Pre] = node
  def postCoerce(node: GpuMemoryFence[Pre]): GpuMemoryFence[Post] = rewriteDefault(node)
  override final def dispatch(node: GpuMemoryFence[Pre]): GpuMemoryFence[Post] = postCoerce(coerce(preCoerce(node)))

  def preCoerce(node: CPPDeclarator[Pre]): CPPDeclarator[Pre] = node
  def postCoerce(node: CPPDeclarator[Pre]): CPPDeclarator[Post] = rewriteDefault(node)
  override final def dispatch(node: CPPDeclarator[Pre]): CPPDeclarator[Post] = postCoerce(coerce(preCoerce(node)))

  def preCoerce(node: CPPDeclarationSpecifier[Pre]): CPPDeclarationSpecifier[Pre] = node
  def postCoerce(node: CPPDeclarationSpecifier[Pre]): CPPDeclarationSpecifier[Post] = rewriteDefault(node)
  override final def dispatch(node: CPPDeclarationSpecifier[Pre]): CPPDeclarationSpecifier[Post] = postCoerce(coerce(preCoerce(node)))

  def preCoerce(node: CPPDeclaration[Pre]): CPPDeclaration[Pre] = node
  def postCoerce(node: CPPDeclaration[Pre]): CPPDeclaration[Post] = rewriteDefault(node)
  override final def dispatch(node: CPPDeclaration[Pre]): CPPDeclaration[Post] = postCoerce(coerce(preCoerce(node)))

  def preCoerce(node: CPPAddressing[Pre]): CPPAddressing[Pre] = node
  def postCoerce(node: CPPAddressing[Pre]): CPPAddressing[Post] = rewriteDefault(node)
  override final def dispatch(node: CPPAddressing[Pre]): CPPAddressing[Post] = postCoerce(coerce(preCoerce(node)))

  def preCoerce(node: CPPInit[Pre]): CPPInit[Pre] = node
  def postCoerce(node: CPPInit[Pre]): CPPInit[Post] = rewriteDefault(node)
  override final def dispatch(node: CPPInit[Pre]): CPPInit[Post] = postCoerce(coerce(preCoerce(node)))

  def preCoerce(node: CPPExprOrTypeSpecifier[Pre]): CPPExprOrTypeSpecifier[Pre] = node
  def postCoerce(node: CPPExprOrTypeSpecifier[Pre]): CPPExprOrTypeSpecifier[Post] = rewriteDefault(node)
  override final def dispatch(node: CPPExprOrTypeSpecifier[Pre]): CPPExprOrTypeSpecifier[Post] = postCoerce(coerce(preCoerce(node)))

  def preCoerce(node: JavaName[Pre]): JavaName[Pre] = node
  def postCoerce(node: JavaName[Pre]): JavaName[Post] = rewriteDefault(node)
  override final def dispatch(node: JavaName[Pre]): JavaName[Post] = postCoerce(coerce(preCoerce(node)))

  def preCoerce(node: JavaImport[Pre]): JavaImport[Pre] = node
  def postCoerce(node: JavaImport[Pre]): JavaImport[Post] = rewriteDefault(node)
  override final def dispatch(node: JavaImport[Pre]): JavaImport[Post] = postCoerce(coerce(preCoerce(node)))

  def preCoerce(node: JavaModifier[Pre]): JavaModifier[Pre] = node
  def postCoerce(node: JavaModifier[Pre]): JavaModifier[Post] = rewriteDefault(node)
  override final def dispatch(node: JavaModifier[Pre]): JavaModifier[Post] = postCoerce(coerce(preCoerce(node)))

  def preCoerce(node: JavaVariableDeclaration[Pre]): JavaVariableDeclaration[Pre] = node
  def postCoerce(node: JavaVariableDeclaration[Pre]): JavaVariableDeclaration[Post] = rewriteDefault(node)
  override final def dispatch(node: JavaVariableDeclaration[Pre]): JavaVariableDeclaration[Post] = postCoerce(coerce(preCoerce(node)))

  def preCoerce(node: Operator[Pre]): Operator[Pre] = node
  def postCoerce(node: Operator[Pre]): Operator[Post] = rewriteDefault(node)
  override final def dispatch(node: Operator[Pre]): Operator[Post] = postCoerce(coerce(preCoerce(node)))

  def preCoerce(node: BipPortType[Pre]): BipPortType[Pre] = node
  def postCoerce(node: BipPortType[Pre]): BipPortType[Post] = rewriteDefault(node)
  override final def dispatch(node: BipPortType[Pre]): BipPortType[Post] = postCoerce(coerce(preCoerce(node)))

  def preCoerce(node: BipTransitionSignature[Pre]): BipTransitionSignature[Pre] = node
  def postCoerce(node: BipTransitionSignature[Pre]): BipTransitionSignature[Post] = rewriteDefault(node)
  override final def dispatch(node: BipTransitionSignature[Pre]): BipTransitionSignature[Post] = postCoerce(coerce(preCoerce(node)))

  def preCoerce(node: BipGlueDataWire[Pre]): BipGlueDataWire[Pre] = node
  def postCoerce(node: BipGlueDataWire[Pre]): BipGlueDataWire[Post] = rewriteDefault(node)
  override final def dispatch(node: BipGlueDataWire[Pre]): BipGlueDataWire[Post] = postCoerce(coerce(preCoerce(node)))

  def preCoerce(node: BipGlueRequires[Pre]): BipGlueRequires[Pre] = node
  def postCoerce(node: BipGlueRequires[Pre]): BipGlueRequires[Post] = rewriteDefault(node)
  override final def dispatch(node: BipGlueRequires[Pre]): BipGlueRequires[Post] = postCoerce(coerce(preCoerce(node)))

  def preCoerce(node: BipGlueAccepts[Pre]): BipGlueAccepts[Pre] = node
  def postCoerce(node: BipGlueAccepts[Pre]): BipGlueAccepts[Post] = rewriteDefault(node)
  override final def dispatch(node: BipGlueAccepts[Pre]): BipGlueAccepts[Post] = postCoerce(coerce(preCoerce(node)))

  def preCoerce(node: JavaBipGlueElement[Pre]): JavaBipGlueElement[Pre] = node
  def postCoerce(node: JavaBipGlueElement[Pre]): JavaBipGlueElement[Post] = rewriteDefault(node)
  override final def dispatch(node: JavaBipGlueElement[Pre]): JavaBipGlueElement[Post] = postCoerce(coerce(preCoerce(node)))

  def preCoerce(node: JavaBipGlueName[Pre]): JavaBipGlueName[Pre] = node
  def postCoerce(node: JavaBipGlueName[Pre]): JavaBipGlueName[Post] = rewriteDefault(node)
  override final def dispatch(node: JavaBipGlueName[Pre]): JavaBipGlueName[Post] = postCoerce(coerce(preCoerce(node)))

  def preCoerce(node: LlvmFunctionContract[Pre]): LlvmFunctionContract[Pre] = node
  def postCoerce(node: LlvmFunctionContract[Pre]): LlvmFunctionContract[Post] = rewriteDefault(node)
  override final def dispatch(node: LlvmFunctionContract[Pre]): LlvmFunctionContract[Post] = postCoerce(coerce(preCoerce(node)))

  def preCoerce(node: LlvmLoopContract[Pre]): LlvmLoopContract[Pre] = node
  def postCoerce(node: LlvmLoopContract[Pre]): LlvmLoopContract[Post] = rewriteDefault(node)
  override final def dispatch(node: LlvmLoopContract[Pre]): LlvmLoopContract[Post] = postCoerce(coerce(preCoerce(node)))

  def preCoerce(node: ProverLanguage[Pre]): ProverLanguage[Pre] = node
  def postCoerce(node: ProverLanguage[Pre]): ProverLanguage[Post] = rewriteDefault(node)
  override final def dispatch(node: ProverLanguage[Pre]): ProverLanguage[Post] = postCoerce(coerce(preCoerce(node)))

  def preCoerce(node: SmtlibFunctionSymbol[Pre]): SmtlibFunctionSymbol[Pre] = node
  def postCoerce(node: SmtlibFunctionSymbol[Pre]): SmtlibFunctionSymbol[Post] = rewriteDefault(node)
  override final def dispatch(node: SmtlibFunctionSymbol[Pre]): SmtlibFunctionSymbol[Post] = postCoerce(coerce(preCoerce(node)))

  def preCoerce(node: PVLCommunicateAccess[Pre]): PVLCommunicateAccess[Pre] = node
  def postCoerce(node: PVLCommunicateAccess[Pre]): PVLCommunicateAccess[Post] = rewriteDefault(node)
  override final def dispatch(node: PVLCommunicateAccess[Pre]): PVLCommunicateAccess[Post] = postCoerce(coerce(preCoerce(node)))

  def preCoerce(node: PVLCommunicateSubject[Pre]): PVLCommunicateSubject[Pre] = node
  def postCoerce(node: PVLCommunicateSubject[Pre]): PVLCommunicateSubject[Post] = rewriteDefault(node)
  override final def dispatch(node: PVLCommunicateSubject[Pre]): PVLCommunicateSubject[Post] = postCoerce(coerce(preCoerce(node)))

  def preCoerce(node: Access[Pre]): Access[Pre] = node
  def postCoerce(node: Access[Pre]): Access[Post] = rewriteDefault(node)
  override final def dispatch(node: Access[Pre]): Access[Post] = postCoerce(coerce(preCoerce(node)))

  def preCoerce(node: Subject[Pre]): Subject[Pre] = node
  def postCoerce(node: Subject[Pre]): Subject[Post] = rewriteDefault(node)
  override final def dispatch(node: Subject[Pre]): Subject[Post] = postCoerce(coerce(preCoerce(node)))

  def preCoerce(node: SeqRun[Pre]): SeqRun[Pre] = node
  def postCoerce(node: SeqRun[Pre]): SeqRun[Post] = rewriteDefault(node)
  override final def dispatch(node: SeqRun[Pre]): SeqRun[Post] = postCoerce(coerce(preCoerce(node)))

  def preCoerce(node: SeqGuard[Pre]): SeqGuard[Pre] = node
  def postCoerce(node: SeqGuard[Pre]): SeqGuard[Post] = rewriteDefault(node)
  override final def dispatch(node: SeqGuard[Pre]): SeqGuard[Post] = postCoerce(coerce(preCoerce(node)))

  def coerce(value: Expr[Pre], target: Type[Pre]): Expr[Pre] =
    ApplyCoercion(value, CoercionUtils.getCoercion(value.t, target) match {
      case Some(coercion) => coercion
      case None => throw Incoercible(value, target)
    })(coercionOrigin(value))

  def coerceArgs(args: Seq[Expr[Pre]], app: Applicable[Pre]): Seq[Expr[Pre]] =
    args.zip(app.args).map {
      case (value, arg) => coerce(value, arg.t)
    }

  def coerceArgs(args: Seq[Expr[Pre]], app: ContractApplicable[Pre], tArgs: Seq[Type[Pre]]): Seq[Expr[Pre]] =
    args.zip(app.args).map {
      case (value, arg) => coerce(value, arg.t.particularize(app.typeArgs.zip(tArgs).toMap))
    }

  def coerceGiven(givenMap: Seq[(Ref[Pre, Variable[Pre]], Expr[Pre])]): Seq[(Ref[Pre, Variable[Pre]], Expr[Pre])] =
    givenMap.map {
      case (Ref(v), e) => (v.ref, coerce(e, v.t))
    }

  def coerceYields(yields: Seq[(Expr[Pre], Ref[Pre, Variable[Pre]])], blame: => Expr[_]): Seq[(Expr[Pre], Ref[Pre, Variable[Pre]])] =
    yields.map {
      case (target, Ref(yieldArg)) => CoercionUtils.getCoercion[Pre](yieldArg.t, target.t) match {
        case None => throw IncoercibleExplanation(blame, "The target for a yielded argument does not exactly match the yields type.")
        case Some(CoerceIdentity(_)) => (target, yieldArg.ref)
        case Some(_) => throw IncoercibleExplanation(blame, "The target for a yielded argument does not exactly match the yields type.")
      }
    }

  def rat(e: Expr[Pre]): Expr[Pre] = coerce(e, TRational[Pre]())
  def bool(e: Expr[Pre]): Expr[Pre] = coerce(e, TBool[Pre]())
  def res(e: Expr[Pre]): Expr[Pre] = coerce(e, TResource[Pre]())
  def int(e: Expr[Pre]): Expr[Pre] = coerce(e, TInt[Pre]())
  def string(e: Expr[Pre]): Expr[Pre] = coerce(e, TString[Pre]())
  def float(e: Expr[Pre]): Expr[Pre] = coerce(e, TFloats.max[Pre])
  def process(e: Expr[Pre]): Expr[Pre] = coerce(e, TProcess[Pre]())
  def ref(e: Expr[Pre]): Expr[Pre] = coerce(e, TRef[Pre]())
  def cls(e: Expr[Pre]): Expr[Pre] = coerce(e, TAnyClass[Pre]())
  def option(e: Expr[Pre]): (Expr[Pre], TOption[Pre]) =
    CoercionUtils.getAnyOptionCoercion(e.t) match {
      case Some((coercion, t)) => (ApplyCoercion(e, coercion)(coercionOrigin(e)), t)
      case None => throw IncoercibleText(e, s"option")
    }
  def tuple(e: Expr[Pre]): (Expr[Pre], TTuple[Pre]) =
    CoercionUtils.getAnyTupleCoercion(e.t) match {
      case Some((coercion, t)) => (ApplyCoercion(e, coercion)(coercionOrigin(e)), t)
      case None => throw IncoercibleText(e, s"tuple")
    }
  def seq(e: Expr[Pre]): (Expr[Pre], TSeq[Pre]) =
    CoercionUtils.getAnySeqCoercion(e.t) match {
      case Some((coercion, t)) => (ApplyCoercion(e, coercion)(coercionOrigin(e)), t)
      case None => throw IncoercibleText(e, s"sequence")
    }
  def set(e: Expr[Pre]): (Expr[Pre], TSet[Pre]) =
    CoercionUtils.getAnySetCoercion(e.t) match {
      case Some((coercion, t)) => (ApplyCoercion(e, coercion)(coercionOrigin(e)), t)
      case None => throw IncoercibleText(e, s"set")
    }
  def bag(e: Expr[Pre]): (Expr[Pre], TBag[Pre]) =
    CoercionUtils.getAnyBagCoercion(e.t) match {
      case Some((coercion, t)) => (ApplyCoercion(e, coercion)(coercionOrigin(e)), t)
      case None => throw IncoercibleText(e, s"bag")
    }
  def map(e: Expr[Pre]): (Expr[Pre], TMap[Pre]) =
    CoercionUtils.getAnyMapCoercion(e.t) match {
      case Some((coercion, t)) => (ApplyCoercion(e, coercion)(coercionOrigin(e)), t)
      case None => throw IncoercibleText(e, s"map")
    }
  def sized(e: Expr[Pre]): (Expr[Pre], SizedType[Pre]) =
    CoercionUtils.getAnySizedCoercion(e.t) match {
      case Some((coercion, t)) => (ApplyCoercion(e, coercion)(coercionOrigin(e)), t)
      case None => throw IncoercibleText(e, s"collection type")
    }
  def array(e: Expr[Pre]): (Expr[Pre], TArray[Pre]) =
    CoercionUtils.getAnyArrayCoercion(e.t) match {
      case Some((coercion, t)) => (ApplyCoercion(e, coercion)(coercionOrigin(e)), t)
      case None => throw IncoercibleText(e, s"array")
    }
  def arrayMatrix(e: Expr[Pre]): (Expr[Pre], TArray[Pre]) =
    CoercionUtils.getAnyMatrixArrayCoercion(e.t) match {
      case Some((coercion, t)) => (ApplyCoercion(e, coercion)(coercionOrigin(e)), t)
      case None => throw IncoercibleText(e, s"two-dimensional array")
    }
  def pointer(e: Expr[Pre]): (Expr[Pre], TPointer[Pre]) =
    CoercionUtils.getAnyPointerCoercion(e.t) match {
      case Some((coercion, t)) => (ApplyCoercion(e, coercion)(coercionOrigin(e)), t)
      case None => throw IncoercibleText(e, s"pointer")
    }
  def matrix(e: Expr[Pre]): (Expr[Pre], TMatrix[Pre]) =
    CoercionUtils.getAnyMatrixCoercion(e.t) match {
      case Some((coercion, t)) => (ApplyCoercion(e, coercion)(coercionOrigin(e)), t)
      case None => throw IncoercibleText(e, s"matrix")
    }
  def model(e: Expr[Pre]): (Expr[Pre], TModel[Pre]) =
    CoercionUtils.getAnyModelCoercion(e.t) match {
      case Some((coercion, t)) => (ApplyCoercion(e, coercion)(coercionOrigin(e)), t)
      case None => throw IncoercibleText(e, s"model")
    }
  def either(e: Expr[Pre]): (Expr[Pre], TEither[Pre]) =
    CoercionUtils.getAnyEitherCoercion(e.t) match {
      case Some((coercion, t)) => (ApplyCoercion(e, coercion)(coercionOrigin(e)), t)
      case None => throw IncoercibleText(e, s"either")
    }
  def bitvec(e: Expr[Pre]): (Expr[Pre], TSmtlibBitVector[Pre]) =
    CoercionUtils.getAnyBitvecCoercion(e.t) match {
      case Some((coercion, t)) => (ApplyCoercion(e, coercion)(coercionOrigin(e)), t)
      case None => throw IncoercibleText(e, s"(_ BitVec ?)")
    }
  def bitvec2[T](e1: Expr[Pre], e2: Expr[Pre], f: (Expr[Pre], Expr[Pre]) => T): T = {
    val (e1c, t) = bitvec(e1)
    val e2c = coerce(e2, t)
    f(e1c, e2c)
  }
  def fp(e: Expr[Pre]): (Expr[Pre], TSmtlibFloatingPoint[Pre]) =
    CoercionUtils.getAnySmtlibFloatCoercion(e.t) match {
      case Some((coercion, t)) => (ApplyCoercion(e, coercion)(coercionOrigin(e)), t)
      case None => throw IncoercibleText(e, s"(_ FloatingPoint ? ?)")
    }
  def fp2[T](e1: Expr[Pre], e2: Expr[Pre], f: (Expr[Pre], Expr[Pre]) => T): T = {
    val (e1c, t) = fp(e1)
    val e2c = coerce(e2, t)
    f(e1c, e2c)
  }
  def reglan(e: Expr[Pre]): Expr[Pre] = coerce(e, TSmtlibRegLan())
  def smtstr(e: Expr[Pre]): Expr[Pre] = coerce(e, TSmtlibString())
  def smtarr(e: Expr[Pre]): (Expr[Pre], TSmtlibArray[Pre]) =
    CoercionUtils.getAnySmtlibArrayCoercion(e.t) match {
      case Some((coercion, t)) => (ApplyCoercion(e, coercion)(coercionOrigin(e)), t)
      case None => throw IncoercibleText(e, s"(Array ? ?)")
    }
  def z3seq(e: Expr[Pre]): (Expr[Pre], TSmtlibSeq[Pre]) =
    CoercionUtils.getAnySmtlibSeqCoercion(e.t) match {
      case Some((coercion, t)) => (ApplyCoercion(e, coercion)(coercionOrigin(e)), t)
      case None => throw IncoercibleText(e, s"(Seq ?)")
    }

  def firstOkHelper[T](thing: Either[Seq[CoercionError], T], onError: => T): Either[Seq[CoercionError], T] =
    thing match {
      case Left(errs) => try {
        Right(onError)
      } catch {
        case err: CoercionError => Left(errs :+ err)
      }
      case Right(value) => Right(value)
    }

  implicit class FirstOkHelper[T](res: Either[Seq[CoercionError], T]) {
    def onCoercionError(f: => T): Either[Seq[CoercionError], T] =
      res match {
        case Left(errs) => try {
          Right(f)
        } catch {
          case err: CoercionError => Left(errs :+ err)
        }
        case Right(value) => Right(value)
      }
  }

  def firstOk[T](expr: Expr[Pre], message: => String,
                  alt1: => T = throw IncoercibleDummy,
                  alt2: => T = throw IncoercibleDummy,
                  alt3: => T = throw IncoercibleDummy,
                  alt4: => T = throw IncoercibleDummy,
                  alt5: => T = throw IncoercibleDummy,
                  alt6: => T = throw IncoercibleDummy,
                  alt7: => T = throw IncoercibleDummy,
                  alt8: => T = throw IncoercibleDummy,
                  alt9: => T = throw IncoercibleDummy,
                  alt10: => T = throw IncoercibleDummy,
                  alt11: => T = throw IncoercibleDummy) : T = {
    Left(Nil)
      .onCoercionError(alt1)
      .onCoercionError(alt2)
      .onCoercionError(alt3)
      .onCoercionError(alt4)
      .onCoercionError(alt5)
      .onCoercionError(alt6)
      .onCoercionError(alt7)
      .onCoercionError(alt8)
      .onCoercionError(alt9)
      .onCoercionError(alt10)
      .onCoercionError(alt11)
    match {
      case Left(errs) =>
        for(err <- errs) {
          logger.debug(err.text)
        }
        throw IncoercibleExplanation(expr, message)
      case Right(value) => value
    }
  }

  def coerce(e: Expr[Pre]): Expr[Pre] = {
    implicit val o: Origin = e.o

    e match {
      case ApplyCoercion(_, _) =>
        throw Unreachable("All instances of ApplyCoercion should be immediately rewritten by CoercingRewriter.dispatch.")

      case ActionApply(action, args) =>
        ActionApply(action, coerceArgs(args, action.decl))
      case ActionPerm(loc, perm) =>
        ActionPerm(loc, rat(perm))
      case AddrOf(e) =>
        AddrOf(e)
      case ADTFunctionInvocation(typeArgs, ref, args) => typeArgs match {
        case Some((adt, typeArgs)) =>
          ADTFunctionInvocation(Some((adt, typeArgs)), ref, args.zip(ref.decl.args).map {
            case (value, arg) =>
              try {
                coerce(value, arg.t.particularize(adt.decl.typeArgs.zip(typeArgs).toMap))
              } catch {
                case x: CoercionError =>
                  println(x.text)
                  ???
              }
          })
        case None =>
          ADTFunctionInvocation(None, ref, coerceArgs(args, ref.decl))
      }
      case AmbiguousComputationalAnd(left, right) =>
        firstOk(e, s"Expected both operands to be of type integer or boolean, but got ${left.t} and ${right.t}.",
          AmbiguousComputationalAnd(int(left), int(right)),
          AmbiguousComputationalAnd(bool(left), bool(right)),
        )
      case AmbiguousComputationalOr(left, right) =>
        firstOk(e, s"Expected both operands to be of type integer or boolean, but got ${left.t} and ${right.t}.",
          AmbiguousComputationalOr(int(left), int(right)),
          AmbiguousComputationalOr(bool(left), bool(right)),
        )
      case AmbiguousComputationalXor(left, right) =>
        firstOk(e, s"Expected both operands to be of type integer or boolean, but got ${left.t} and ${right.t}.",
          AmbiguousComputationalXor(int(left), int(right)),
          AmbiguousComputationalXor(bool(left), bool(right)),
        )
      case AmbiguousGreater(left, right) =>
        firstOk(e, s"Expected both operands to be numeric, a set, or a bag, but got ${left.t} and ${right.t}.",
          AmbiguousGreater(int(left), int(right)),
          AmbiguousGreater(rat(left), rat(right)), {
            val (coercedLeft, leftSet) = set(left)
            val (coercedRight, rightSet) = set(right)
            val sharedType = Types.leastCommonSuperType(leftSet.element, rightSet.element)
            AmbiguousGreater(coerce(coercedLeft, TSet(sharedType)), coerce(coercedRight, TSet(sharedType)))
          }, {
            val (coercedLeft, leftBag) = bag(left)
            val (coercedRight, rightBag) = bag(right)
            val sharedType = Types.leastCommonSuperType(leftBag.element, rightBag.element)
            AmbiguousGreater(coerce(coercedLeft, TBag(sharedType)), coerce(coercedRight, TBag(sharedType)))
          },
        )
      case AmbiguousGreaterEq(left, right) =>
        firstOk(e, s"Expected both operands to be numeric, a set, or a bag, but got ${left.t} and ${right.t}.",
          AmbiguousGreaterEq(int(left), int(right)),
          AmbiguousGreaterEq(rat(left), rat(right)), {
            val (coercedLeft, leftSet) = set(left)
            val (coercedRight, rightSet) = set(right)
            val sharedType = Types.leastCommonSuperType(leftSet.element, rightSet.element)
            AmbiguousGreaterEq(coerce(coercedLeft, TSet(sharedType)), coerce(coercedRight, TSet(sharedType)))
          }, {
            val (coercedLeft, leftBag) = bag(left)
            val (coercedRight, rightBag) = bag(right)
            val sharedType = Types.leastCommonSuperType(leftBag.element, rightBag.element)
            AmbiguousGreaterEq(coerce(coercedLeft, TBag(sharedType)), coerce(coercedRight, TBag(sharedType)))
          },
        )
      case AmbiguousLess(left, right) =>
        firstOk(e, s"Expected both operands to be numeric, a set, or a bag, but got ${left.t} and ${right.t}.",
          AmbiguousLess(int(left), int(right)),
          AmbiguousLess(rat(left), rat(right)), {
            val (coercedLeft, leftSet) = set(left)
            val (coercedRight, rightSet) = set(right)
            val sharedType = Types.leastCommonSuperType(leftSet.element, rightSet.element)
            AmbiguousLess(coerce(coercedLeft, TSet(sharedType)), coerce(coercedRight, TSet(sharedType)))
          }, {
            val (coercedLeft, leftBag) = bag(left)
            val (coercedRight, rightBag) = bag(right)
            val sharedType = Types.leastCommonSuperType(leftBag.element, rightBag.element)
            AmbiguousLess(coerce(coercedLeft, TBag(sharedType)), coerce(coercedRight, TBag(sharedType)))
          },
        )
      case AmbiguousLessEq(left, right) =>
        firstOk(e, s"Expected both operands to be numeric, a set, or a bag, but got ${left.t} and ${right.t}.",
          AmbiguousLessEq(int(left), int(right)),
          AmbiguousLessEq(rat(left), rat(right)), {
            val (coercedLeft, leftSet) = set(left)
            val (coercedRight, rightSet) = set(right)
            val sharedType = Types.leastCommonSuperType(leftSet.element, rightSet.element)
            AmbiguousLessEq(coerce(coercedLeft, TSet(sharedType)), coerce(coercedRight, TSet(sharedType)))
          }, {
            val (coercedLeft, leftBag) = bag(left)
            val (coercedRight, rightBag) = bag(right)
            val sharedType = Types.leastCommonSuperType(leftBag.element, rightBag.element)
            AmbiguousLessEq(coerce(coercedLeft, TBag(sharedType)), coerce(coercedRight, TBag(sharedType)))
          },
        )
      case minus @ AmbiguousMinus(left, right) =>
        firstOk(e, s"Expected both operands to be numeric, a set or a bag; or a pointer and integer, but got ${left.t} and ${right.t}.",
          Minus(int(left), int(right)),
          Minus(rat(left), rat(right)),
          AmbiguousMinus(pointer(left)._1, int(right))(minus.blame), {
            val (coercedLeft, TSet(elementLeft)) = set(left)
            val (coercedRight, TSet(elementRight)) = set(right)
            val sharedType = Types.leastCommonSuperType(elementLeft, elementRight)
            AmbiguousMinus(coerce(coercedLeft, TSet(sharedType)), coerce(coercedRight, TSet(sharedType)))(minus.blame)
          }, {
            val (coercedLeft, TBag(elementLeft)) = bag(left)
            val (coercedRight, TBag(elementRight)) = bag(right)
            val sharedType = Types.leastCommonSuperType(elementLeft, elementRight)
            AmbiguousMinus(coerce(coercedLeft, TBag(sharedType)), coerce(coercedRight, TBag(sharedType)))(minus.blame)
          }
        )
      case AmbiguousMember(x, xs) =>
        firstOk(xs, s"Expected collection to be a sequence, set, bag or map, but got ${xs.t}.", {
          val (coercedXs, TSeq(element)) = seq(xs)
          val sharedType = Types.leastCommonSuperType(x.t, element)
          AmbiguousMember(coerce(x, sharedType), coerce(coercedXs, TSeq(sharedType)))
        }, {
          val (coercedXs, TSet(element)) = set(xs)
          val sharedType = Types.leastCommonSuperType(x.t, element)
          AmbiguousMember(coerce(x, sharedType), coerce(coercedXs, TSet(sharedType)))
        }, {
          val (coercedXs, TBag(element)) = bag(xs)
          val sharedType = Types.leastCommonSuperType(x.t, element)
          AmbiguousMember(coerce(x, sharedType), coerce(coercedXs, TBag(sharedType)))
        }, {
          val (coercedXs, TMap(element, _)) = map(xs)
          AmbiguousMember(coerce(x, element), coercedXs)
        })
      case AmbiguousMult(left, right) =>
        firstOk(e, s"Expected both operands to be numeric, a process, a set or a bag but got ${left.t} and ${right.t}.",
          AmbiguousMult(int(left), int(right)),
          AmbiguousMult(rat(left), rat(right)),
          AmbiguousMult(process(left), process(right)), {
            val (coercedLeft, TSet(elementLeft)) = set(left)
            val (coercedRight, TSet(elementRight)) = set(right)
            val sharedType = Types.leastCommonSuperType(elementLeft, elementRight)
            AmbiguousMult(coerce(coercedLeft, TSet(sharedType)), coerce(coercedRight, TSet(sharedType)))
          }, {
            val (coercedLeft, TBag(elementLeft)) = bag(left)
            val (coercedRight, TBag(elementRight)) = bag(right)
            val sharedType = Types.leastCommonSuperType(elementLeft, elementRight)
            AmbiguousMult(coerce(coercedLeft, TBag(sharedType)), coerce(coercedRight, TBag(sharedType)))
          }
        )
      case AmbiguousOr(left, right) =>
        firstOk(e, s"Expected both operands to be boolean or a process, but got ${left.t} and ${right.t}.",
          AmbiguousOr(bool(left), bool(right)),
          AmbiguousOr(process(left), process(right)),
        )
      case plus @ AmbiguousPlus(left, right) =>
        firstOk(e, s"Expected both operands to be numeric, a process, a sequence, set, bag, or string; or a pointer and integer, but got ${left.t} and ${right.t}.",
          AmbiguousPlus(int(left), int(right))(plus.blame),
          AmbiguousPlus(float(left), float(right))(plus.blame),
          AmbiguousPlus(rat(left), rat(right))(plus.blame),
          AmbiguousPlus(process(left), process(right))(plus.blame),
          AmbiguousPlus(string(left), string(right))(plus.blame),
          AmbiguousPlus(pointer(left)._1, int(right))(plus.blame), {
            val (coercedLeft, TSeq(elementLeft)) = seq(left)
            val (coercedRight, TSeq(elementRight)) = seq(right)
            val sharedType = Types.leastCommonSuperType(elementLeft, elementRight)
            AmbiguousPlus(coerce(coercedLeft, TSeq(sharedType)), coerce(coercedRight, TSeq(sharedType)))(plus.blame)
          }, {
            val (coercedLeft, TSet(elementLeft)) = set(left)
            val (coercedRight, TSet(elementRight)) = set(right)
            val sharedType = Types.leastCommonSuperType(elementLeft, elementRight)
            AmbiguousPlus(coerce(coercedLeft, TSet(sharedType)), coerce(coercedRight, TSet(sharedType)))(plus.blame)
          }, {
            val (coercedLeft, TBag(elementLeft)) = bag(left)
            val (coercedRight, TBag(elementRight)) = bag(right)
            val sharedType = Types.leastCommonSuperType(elementLeft, elementRight)
            AmbiguousPlus(coerce(coercedLeft, TBag(sharedType)), coerce(coercedRight, TBag(sharedType)))(plus.blame)
          }, {
            val validOperators = plus.getValidOperatorsOf(OperatorLeftPlus())
            validOperators match {
              case Some(Seq(op)) => AmbiguousPlus(cls(left), coerce(right, op.args.head.t))(plus.blame)
              case _ => throw IncoercibleText(left, "This expression does not have a matching custom plus operator")
            }
          }, {
            // TODO: Definition of operators should be type checked for arity
            val validOperators = plus.getValidOperatorsOf(OperatorRightPlus())
            validOperators match {
              case Some(Seq(op)) => AmbiguousPlus(cls(right), coerce(left, op.args.head.t))(plus.blame)
              case _ => throw IncoercibleText(right, "This expression does not have a matching custom right plus operator")
            }
          }
        )
      case AmbiguousResult() => e
      case sub @ AmbiguousSubscript(collection, index) =>
        firstOk(e, s"Expected collection to be a sequence, array, pointer or map, but got ${collection.t}.",
          AmbiguousSubscript(seq(collection)._1, int(index))(sub.blame),
          AmbiguousSubscript(array(collection)._1, int(index))(sub.blame),
          AmbiguousSubscript(pointer(collection)._1, int(index))(sub.blame),
          AmbiguousSubscript(map(collection)._1, coerce(index, map(collection)._2.key))(sub.blame),
        )
      case AmbiguousThis() => e
      case And(left, right) =>
        And(bool(left), bool(right))
      case any @ Any() =>
        Any()(any.blame)
      case a @ ArraySubscript(arr, index) =>
        ArraySubscript(array(arr)._1, int(index))(a.blame)
      case BagAdd(xs, ys) =>
        val (left, TBag(leftT)) = bag(xs)
        val (right, TBag(rightT)) = bag(ys)
        val sharedElement = Types.leastCommonSuperType(leftT, rightT)
        BagAdd(coerce(left, TBag(sharedElement)), coerce(right, TBag(sharedElement)))
      case BagLargestCommon(xs, ys) =>
        val (left, TBag(leftT)) = bag(xs)
        val (right, TBag(rightT)) = bag(ys)
        val sharedElement = Types.leastCommonSuperType(leftT, rightT)
        BagLargestCommon(coerce(left, TBag(sharedElement)), coerce(right, TBag(sharedElement)))
      case BagMemberCount(x, xs) =>
        val (coercedBag, TBag(element)) = bag(xs)
        val sharedType = Types.leastCommonSuperType(x.t, element)
        BagMemberCount(coerce(x, sharedType), coerce(coercedBag, TBag(sharedType)))
      case BagMinus(xs, ys) =>
        val (left, TBag(leftT)) = bag(xs)
        val (right, TBag(rightT)) = bag(ys)
        val sharedElement = Types.leastCommonSuperType(leftT, rightT)
        BagMinus(coerce(left, TBag(sharedElement)), coerce(right, TBag(sharedElement)))
      case bgi @ BipGuardInvocation(obj, ref) =>
        BipGuardInvocation(cls(obj), ref)
      case BitAnd(left, right) =>
        BitAnd(int(left), int(right))
      case BitNot(arg) =>
        BitNot(int(arg))
      case BitOr(left, right) =>
        BitOr(int(left), int(right))
      case BitShl(left, right) =>
        BitShl(int(left), int(right))
      case BitShr(left, right) =>
        BitShr(int(left), int(right))
      case BitUShr(left, right) =>
        BitUShr(int(left), int(right))
      case BitXor(left, right) =>
        BitXor(int(left), int(right))
      case Cast(value, typeValue) =>
        Cast(value, typeValue)
      case CastFloat(e, t) =>
        CastFloat(float(e), t)
      case CCast(e, t) => CCast(e, t)
      case c @ CharValue(_) => c
      case inv @ CInvocation(applicable, args, givenArgs, yields) =>
        CInvocation(applicable, args, givenArgs, yields)(inv.blame)
      case CLocal(name) => e
      case c @ Committed(obj) =>
        Committed(cls(obj))(c.blame)
      case ComputationalAnd(left, right) =>
        ComputationalAnd(bool(left), bool(right))
      case ComputationalOr(left, right) =>
        ComputationalOr(bool(left), bool(right))
      case ComputationalXor(left, right) =>
        ComputationalXor(bool(left), bool(right))
      case Concat(xs, ys) =>
        val (coercedXs, TSeq(xElement)) = seq(xs)
        val (coercedYs, TSeq(yElement)) = seq(ys)
        val sharedType = Types.leastCommonSuperType(xElement, yElement)
        Concat(coerce(xs, TSeq(sharedType)), coerce(ys, TSeq(sharedType)))
      case Cons(x, xs) =>
        val (coercedXs, TSeq(element)) = seq(xs)
        val sharedType = Types.leastCommonSuperType(x.t, element)
        Cons(coerce(x, sharedType), coerce(xs, TSeq(sharedType)))
      case cfa@CPPClassMethodOrFieldAccess(classInstance, methodOrFieldName) => CPPClassMethodOrFieldAccess(classInstance, methodOrFieldName)(cfa.blame)
      case defn@CPPLambdaDefinition(contract, declarator, body) =>
        CPPLambdaDefinition(contract, declarator, body)(defn.blame)
      case CPPLambdaRef() => e
      case inv@CPPInvocation(applicable, args, givenArgs, yields) =>
        CPPInvocation(applicable, args, givenArgs, yields)(inv.blame)
      case CPPLocal(_, _) => e
      case SYCLReadWriteAccess() => e
      case SYCLReadOnlyAccess() => e
      case SYCLRange(dims) => SYCLRange(dims)
      case SYCLNDRange(globalRange, localRange) => SYCLNDRange(globalRange, localRange)
      case StringConcat(left, right) =>
        StringConcat(string(left), string(right))
      case acc @ CStructAccess(struct, field) =>
        CStructAccess(struct, field)(acc.blame)
      case CStructDeref(struct, field) =>
        CStructDeref(struct, field)
      case CurPerm(loc) =>
        CurPerm(loc)
      case CurrentThreadId() =>
        CurrentThreadId()
      //case deref @ DerefVeyMontThread(ref) =>
        //DerefVeyMontThread( TVeyMontThread[Pre](ref))
      case deref @ Deref(obj, ref) =>
        Deref(cls(obj), ref)(deref.blame)
      case deref @ DerefHeapVariable(ref) =>
        DerefHeapVariable(ref)(deref.blame)
      case deref @ DerefPointer(p) =>
        DerefPointer(pointer(p)._1)(deref.blame)
      case deref @ EndpointUse(_) => deref
      case div @ Div(left, right) =>
        firstOk(e, s"Expected both operands to be rational.",
          // PB: horrible hack: Div ends up being silver.PermDiv, which expects an integer divisor. In other cases,
          // we just hope the silver type-check doesn't complain, since in z3 it is uniformly `/` for mixed integers
          // and rationals.
          Div(rat(left), int(right))(div.blame),
          Div(rat(left), rat(right))(div.blame),
        )
      case Drop(xs, count) =>
        Drop(seq(xs)._1, int(count))
      case Empty(obj) =>
        Empty(sized(obj)._1)
      case EmptyProcess() => EmptyProcess()
      case use @ EnumUse(enum, const) => use
      case Eq(left, right) =>
        val sharedType = Types.leastCommonSuperType(left.t, right.t)
        Eq(coerce(left, sharedType), coerce(right, sharedType))
      case EitherLeft(e) =>
        EitherLeft(e)
      case EitherRight(e) =>
        EitherRight(e)
      case Exists(bindings, triggers, body) =>
        Exists(bindings, triggers, bool(body))
      case Exp(left, right) =>
        firstOk(e, s"Expected both operands to be numeric, but got ${left.t} and ${right.t}.",
          Exp(int(left), int(right)),
          Exp(rat(left), rat(right)),
        )
      case div @ FloorDiv(left, right) =>
        FloorDiv(int(left), int(right))(div.blame)
      case Forall(bindings, triggers, body) =>
        Forall(bindings, triggers, bool(body))
      case ForPerm(bindings, loc, body) =>
        ForPerm(bindings, loc, bool(body))
      case ForPermWithValue(binding, body) =>
        ForPermWithValue(binding, bool(body))
      case inv @ FunctionInvocation(ref, args, typeArgs, givenMap, yields) =>
        FunctionInvocation(ref, coerceArgs(args, ref.decl, typeArgs), typeArgs, coerceGiven(givenMap), coerceYields(yields, inv))(inv.blame)
      case get @ GetLeft(e) =>
        GetLeft(either(e)._1)(get.blame)
      case get @ GetRight(e) =>
        GetRight(either(e)._1)(get.blame)
      case GlobalThreadId() =>
        GlobalThreadId()
      case e @ GpgpuCudaKernelInvocation(kernel, blocks, threads, args, givenArgs, yields) =>
        GpgpuCudaKernelInvocation(kernel, int(blocks), int(threads), args, givenArgs, yields)(e.blame)
      case Greater(left, right) =>
        firstOk(e, s"Expected both operands to be numeric, but got ${left.t} and ${right.t}.",
          Greater(int(left), int(right)),
          Greater(rat(left), rat(right)),
        )
      case GreaterEq(left, right) =>
        firstOk(e, s"Expected both operands to be numeric, but got ${left.t} and ${right.t}.",
          GreaterEq(int(left), int(right)),
          GreaterEq(rat(left), rat(right)),
        )
      case head @ Head(xs) =>
        Head(seq(xs)._1)(head.blame)
      case Held(obj) =>
        Held(cls(obj))
      case IdleToken(thread) =>
        IdleToken(cls(thread))
      case Implies(left, right) =>
        Implies(bool(left), res(right))
      case FunctionOf(e, ref) =>
        FunctionOf(e, ref)
      case IndeterminateInteger(min, max) =>
        IndeterminateInteger(int(min), int(max))
      case InlinePattern(inner, parent, group) =>
        InlinePattern(inner, parent, group)
      case inv @ InstanceFunctionInvocation(obj, ref, args, typeArgs, givenMap, yields) =>
        InstanceFunctionInvocation(cls(obj), ref, coerceArgs(args, ref.decl, typeArgs), typeArgs, coerceGiven(givenMap), coerceYields(yields, inv))(inv.blame)
      case InstanceOf(value, typeValue) =>
        InstanceOf(value, typeValue)
      case InstancePredicateApply(obj, ref, args, perm) =>
        InstancePredicateApply(cls(obj), ref, coerceArgs(args, ref.decl), rat(perm))
      case CoalesceInstancePredicateApply(obj, ref, args, perm) =>
        CoalesceInstancePredicateApply(cls(obj), ref, coerceArgs(args, ref.decl), rat(perm))
      case IsLeft(e) =>
        IsLeft(either(e)._1)
      case IsRight(e) =>
        IsRight(either(e)._1)
      case deref @ JavaDeref(obj, field) => e
      case inv @ JavaInvocation(obj, typeParams, method, arguments, givenArgs, yields) => e
      case JavaLiteralArray(exprs) =>
        JavaLiteralArray(exprs)
      case JavaLocal(name) => e
      case JavaNewClass(args, typeArgs, name, givenMap, yields) => e
      case JavaNewDefaultArray(baseType, specifiedDims, moreDims) => e
      case JavaNewLiteralArray(baseType, dims, initializer) => e
      case str @ JavaStringValue(_, _) => str
      case str @ StringValue(_) => str
      case JoinToken(thread) =>
        JoinToken(cls(thread))
      case length @ Length(arr) =>
        Length(array(arr)._1)(length.blame)
      case Less(left, right) =>
        firstOk(e, s"Expected both operands to be numeric, but got ${left.t} and ${right.t}.",
          Less(int(left), int(right)),
          Less(rat(left), rat(right)),
        )
      case LessEq(left, right) =>
        firstOk(e, s"Expected both operands to be numeric, but got ${left.t} and ${right.t}.",
          LessEq(int(left), int(right)),
          LessEq(rat(left), rat(right)),
        )
      case Let(binding, value, main) =>
        Let(binding, coerce(value, binding.t), main)
      case LiteralBag(element, values) =>
        LiteralBag(element, values.map(coerce(_, element)))
      case LiteralMap(k, v, values) =>
        LiteralMap(k, v, values.map {
          case (valueK, valueV) => (coerce(valueK, k), coerce(valueV, v))
        })
      case LiteralSeq(element, values) =>
        LiteralSeq(element, values.map(coerce(_, element)))
      case LiteralSet(element, values) =>
        LiteralSet(element, values.map(coerce(_, element)))
      case LiteralTuple(ts, values) =>
        LiteralTuple(ts, values.zip(ts).map {
          case (v, t) => coerce(v, t)
        })
      case Local(ref) =>
        Local(ref)
      case LocalThreadId() =>
        LocalThreadId()
      case MapCons(m, k, v) =>
        val (coercedMap, mapType) = map(m)
        val sharedType = Types.leastCommonSuperType(mapType.value, v.t)
        MapCons(coerce(coercedMap, TMap(mapType.key, sharedType)), coerce(k, mapType.key), coerce(v, sharedType))
      case MapDisjoint(left, right) =>
        val (coercedLeft, leftType) = map(left)
        val (coercedRight, rightType) = map(right)

        if(leftType.key != rightType.key)
          throw IncoercibleExplanation(e, s"Expected both operands to have a map type of which the key type is equal, " +
            s"but got ${leftType.key} and ${rightType.key}")

        val sharedType = Types.leastCommonSuperType(leftType.value, rightType.value)
        val mapType = TMap(leftType.key, sharedType)
        MapDisjoint(coerce(coercedLeft, mapType), coerce(coercedRight, mapType))
      case MapEq(left, right) =>
        val (coercedLeft, leftType) = map(left)
        val (coercedRight, rightType) = map(right)

        if(leftType.key != rightType.key)
          throw IncoercibleExplanation(e, s"Expected both operands to have a map type of which the key type is equal, " +
            s"but got ${leftType.key} and ${rightType.key}")

        val sharedType = Types.leastCommonSuperType(leftType.value, rightType.value)
        val mapType = TMap(leftType.key, sharedType)
        MapEq(coerce(coercedLeft, mapType), coerce(coercedRight, mapType))
      case get @ MapGet(m, k) =>
        val (coercedMap, mapType) = map(m)
        MapGet(coercedMap, coerce(k, mapType.key))(get.blame)
      case MapItemSet(m) =>
        MapItemSet(map(m)._1)
      case MapKeySet(m) =>
        MapKeySet(map(m)._1)
      case MapMember(x, xs) =>
        val (coercedMap, mapType) = map(xs)
        MapMember(coerce(x, mapType.key), coercedMap)
      case MapRemove(m, k) =>
        val (coercedMap, mapType) = map(m)
        MapRemove(coercedMap, coerce(k, mapType.key))
      case MapValueSet(m) =>
        MapValueSet(map(m)._1)
      case MatrixCompare(left, right) =>
        val (coercedLeft, leftType) = matrix(left)
        val (coercedRight, rightType) = matrix(right)
        val sharedType = Types.leastCommonSuperType(leftType.element, rightType.element)
        MatrixCompare(coerce(coercedLeft, TMatrix(sharedType)), coerce(coercedRight, TMatrix(sharedType)))
      case MatrixRepeat(e) =>
        MatrixRepeat(e)
      case MatrixSum(indices, mat) =>
        MatrixSum(coerce(indices, TSeq[Pre](TInt())), coerce(mat, TSeq[Pre](TRational())))
      case inv @ MethodInvocation(obj, ref, args, outArgs, typeArgs, givenMap, yields) =>
        MethodInvocation(obj, ref, coerceArgs(args, ref.decl, typeArgs), outArgs, typeArgs, coerceGiven(givenMap), coerceYields(yields, inv))(inv.blame)
      case Minus(left, right) =>
        firstOk(e, s"Expected both operands to be numeric, but got ${left.t} and ${right.t}.",
          Minus(int(left), int(right)),
          Minus(rat(left), rat(right)),
        )
      case div @ Mod(left, right) =>
        firstOk(e, s"Expected both operands to be numeric, but got ${left.t} and ${right.t}.",
          Mod(int(left), int(right))(div.blame),
          Mod(rat(left), rat(right))(div.blame),
        )
      case ModelAbstractState(m, state) =>
        ModelAbstractState(model(m)._1, bool(state))
      case ModelChoose(m, perm, totalProcess, choice) =>
        ModelChoose(model(m)._1, rat(perm), process(totalProcess), process(choice))
      case ModelCreate(m, init) =>
        ModelCreate(model(m)._1, process(init))
      case deref @ ModelDeref(obj, ref) =>
        ModelDeref(model(obj)._1, ref)(deref.blame)
      case ModelDestroy(m) =>
        ModelDestroy(model(m)._1)
      case ModelMerge(m, leftPerm, leftProcess, rightPerm, rightProcess) =>
        ModelMerge(model(m)._1, rat(leftPerm), process(leftProcess), rat(rightPerm), process(rightProcess))
      case ModelNew(ref) =>
        ModelNew(ref)
      case ModelPerm(loc, perm) =>
        ModelPerm(loc, rat(perm))
      case ModelSplit(m, leftPerm, leftProcess, rightPerm, rightProcess) =>
        ModelSplit(model(m)._1, rat(leftPerm), process(leftProcess), rat(rightPerm), process(rightProcess))
      case ModelState(m, perm, state) =>
        ModelState(model(m)._1, rat(perm), process(state))
      case Mult(left, right) =>
        firstOk(e, s"Expected both operands to be numeric, but got ${left.t} and ${right.t}.",
          Mult(int(left), int(right)),
          Mult(rat(left), rat(right)),
        )
      case NdIndex(indices, dimensions) =>
        NdIndex(indices.map(int), dimensions.map(int))
      case NdLength(dimensions) =>
        NdLength(dimensions.map(int))
      case NdPartialIndex(indices, linearIndex, dimensions) =>
        NdPartialIndex(indices.map(int), int(linearIndex), dimensions.map(int))
      case Neq(left, right) =>
        val sharedType = Types.leastCommonSuperType(left.t, right.t)
        Neq(coerce(left, sharedType), coerce(right, sharedType))
      case na @ NewArray(element, dims, moreDims) =>
        NewArray(element, dims.map(int), moreDims)(na.blame)
      case NewObject(cls) =>
        NewObject(cls)
      case NoPerm() =>
        NoPerm()
      case Not(arg) =>
        Not(bool(arg))
      case Null() =>
        Null()
      case old @ Old(expr, at) =>
        Old(expr, at)(old.blame)
      case OptEmpty(opt) =>
        OptEmpty(option(opt)._1)
      case get @ OptGet(opt) =>
        OptGet(option(opt)._1)(get.blame)
      case OptGetOrElse(opt, alt) =>
        val (coercedOpt, optType) = option(opt)
        val sharedType = Types.leastCommonSuperType(alt.t, optType.element)
        OptGetOrElse(coerce(coercedOpt, TOption(sharedType)), coerce(alt, sharedType))
      case OptNone() =>
        OptNone()
      case OptNoneTyped(t) =>
        OptNoneTyped(t)
      case OptSome(e) =>
        OptSome(e)
      case OptSomeTyped(t, e) =>
        OptSomeTyped(t, coerce(e, t))
      case Or(left, right) =>
        Or(bool(left), bool(right))
      case Perm(loc, perm) =>
        Perm(loc, rat(perm))
      case PermPointer(p, len, perm) =>
        PermPointer(pointer(p)._1, int(len), rat(perm))
      case PermPointerIndex(p, idx, perm) =>
        PermPointerIndex(pointer(p)._1, int(idx), rat(perm))
      case Permutation(left, right) =>
        val (coercedLeft, leftType) = seq(left)
        val (coercedRight, rightType) = seq(right)
        val sharedType = Types.leastCommonSuperType(leftType.element, rightType.element)
        Permutation(coerce(left, TSeq(sharedType)), coerce(right, TSeq(sharedType)))
      case Plus(left, right) =>
        firstOk(e, s"Expected both operands to be numeric, but got ${left.t} and ${right.t}.",
          Plus(int(left), int(right)),
          Plus(rat(left), rat(right)),
        )
      case add @ PointerAdd(p, offset) =>
        PointerAdd(pointer(p)._1, int(offset))(add.blame)
      case len @ PointerBlockLength(p) =>
        PointerBlockLength(pointer(p)._1)(len.blame)
      case off @ PointerBlockOffset(p) =>
        PointerBlockOffset(pointer(p)._1)(off.blame)
      case len @ PointerLength(p) =>
        PointerLength(pointer(p)._1)(len.blame)
      case get @ PointerSubscript(p, index) =>
        PointerSubscript(pointer(p)._1, int(index))(get.blame)
      case PointsTo(loc, perm, value) =>
        PointsTo(loc, rat(perm), coerce(value, loc.t))
      case PolarityDependent(onInhale, onExhale) =>
        PolarityDependent(res(onInhale), res(onExhale))
      case ass @ PostAssignExpression(target, value) =>
        PostAssignExpression(target, coerce(value, target.t))(ass.blame)
      case ass @ PreAssignExpression(target, value) =>
        PreAssignExpression(target, coerce(value, target.t))(ass.blame)
      case PredicateApply(ref, args, perm) =>
        PredicateApply(ref, coerceArgs(args, ref.decl), rat(perm))
      case inv @ ProcedureInvocation(ref, args, outArgs, typeArgs, givenMap, yields) =>
        ProcedureInvocation(ref, coerceArgs(args, ref.decl, typeArgs), outArgs, typeArgs, coerceGiven(givenMap), coerceYields(yields, inv))(inv.blame)
      case inv @ LlvmFunctionInvocation(ref, args, givenMap, yields) =>
        LlvmFunctionInvocation(ref, args, givenMap, yields)(inv.blame)
      case inv @ LlvmAmbiguousFunctionInvocation(name, args, givenMap, yields) =>
        LlvmAmbiguousFunctionInvocation(name, args, givenMap, yields)(inv.blame)
      case ProcessApply(process, args) =>
        ProcessApply(process, coerceArgs(args, process.decl))
      case ProcessChoice(left, right) =>
        ProcessChoice(process(left), process(right))
      case ProcessPar(left, right) =>
        ProcessPar(process(left), process(right))
      case ProcessSelect(cond, whenTrue, whenFalse) =>
        ProcessSelect(bool(cond), process(whenTrue), process(whenFalse))
      case ProcessSeq(left, right) =>
        ProcessSeq(process(left), process(right))
      case Product(bindings, condition, main) =>
        Product(bindings, bool(condition), int(main))
      case ProverFunctionInvocation(ref, args) =>
        ProverFunctionInvocation(ref, coerceArgs(args, ref.decl))
      case PVLDeref(obj, field) => e
      case PVLInvocation(obj, method, args, typeArgs, givenArgs, yields) => e
      case PVLLocal(name) => e
      case PVLNew(t, args, givenMap, yields) => e
      case Range(from, to) =>
        Range(int(from), int(to))
      case ReadPerm() =>
        ReadPerm()
      case RemoveAt(xs, i) =>
        RemoveAt(seq(xs)._1, int(i))
      case ResourceOfResourceValue(r) =>
        ResourceOfResourceValue(coerce(r, TResourceVal()))
      case ResourceValue(r) =>
        ResourceValue(res(r))
      case Result(ref) =>
        Result(ref)
      case s @ Scale(scale, r) =>
        Scale(rat(scale), res(r))(s.blame)
      case ScaleByParBlock(ref, r) =>
        ScaleByParBlock(ref, res(r))
      case ScopedExpr(locals, body) =>
        ScopedExpr(locals, body)
      case Select(condition, whenTrue, whenFalse) =>
        val sharedType = Types.leastCommonSuperType(whenTrue.t, whenFalse.t)
        Select(bool(condition), coerce(whenTrue, sharedType), coerce(whenFalse, sharedType))
      case SeqMember(x, xs) =>
        val (coercedSeq, seqType) = seq(xs)
        val sharedType = Types.leastCommonSuperType(x.t, seqType.element)
        SeqMember(coerce(x, sharedType), coerce(coercedSeq, TSeq(sharedType)))
      case get @ SeqSubscript(xs, index) =>
        SeqSubscript(seq(xs)._1, int(index))(get.blame)
      case update @ SeqUpdate(xs, i, x) =>
        val (coercedSeq, seqType) = seq(xs)
        val sharedType = Types.leastCommonSuperType(x.t, seqType.element)
        SeqUpdate(coerce(coercedSeq, TSeq(sharedType)), int(i), coerce(x, sharedType))
      case SetIntersection(xs, ys) =>
        val (left, TSet(leftT)) = set(xs)
        val (right, TSet(rightT)) = set(ys)
        val sharedElement = Types.leastCommonSuperType(leftT, rightT)
        SetIntersection(coerce(left, TSet(sharedElement)), coerce(right, TSet(sharedElement)))
      case SetMember(x, xs) =>
        val (coercedSet, setType) = set(xs)
        val sharedType = Types.leastCommonSuperType(x.t, setType.element)
        SetMember(coerce(x, sharedType), coerce(coercedSet, TSet(sharedType)))
      case SetMinus(xs, ys) =>
        val (left, TSet(leftT)) = set(xs)
        val (right, TSet(rightT)) = set(ys)
        val sharedElement = Types.leastCommonSuperType(leftT, rightT)
        SetMinus(coerce(left, TSet(sharedElement)), coerce(right, TSet(sharedElement)))
      case SetUnion(xs, ys) =>
        val (left, TSet(leftT)) = set(xs)
        val (right, TSet(rightT)) = set(ys)
        val sharedElement = Types.leastCommonSuperType(leftT, rightT)
        SetUnion(coerce(left, TSet(sharedElement)), coerce(right, TSet(sharedElement)))
      case SharedMemSize(xs) =>
        firstOk(e, s"Expected operand to be a pointer or array, but got ${xs.t}.",
          SharedMemSize(array(xs)._1),
          SharedMemSize(pointer(xs)._1),
        )
      case SilverBagSize(xs) =>
        SilverBagSize(bag(xs)._1)
      case SilverCurFieldPerm(obj, field) =>
        SilverCurFieldPerm(ref(obj), field)
      case SilverCurPredPerm(ref, args) =>
        SilverCurPredPerm(ref, coerceArgs(args, ref.decl))
      case deref @ SilverDeref(obj, field) =>
        SilverDeref(ref(obj), field)(deref.blame)
      case SilverIntToRat(perm) =>
        SilverIntToRat(int(perm))
      case SilverMapSize(xs) =>
        SilverMapSize(map(xs)._1)
      case SilverNull() =>
        SilverNull()
      case SilverPartialADTFunctionInvocation(name, args, partialTypeArgs) => e
      case SilverSetSize(xs) =>
        SilverSetSize(set(xs)._1)
      case SilverSeqSize(xs) =>
        SilverSeqSize(seq(xs)._1)
      case SilverUntypedNonemptyLiteralMap(values) =>
        SilverUntypedNonemptyLiteralMap(values)
      case Size(obj) =>
        Size(sized(obj)._1)
      case Slice(xs, from, to) =>
        Slice(seq(xs)._1, int(from), int(to))
      case SmtlibBitvecLiteral(data) => SmtlibBitvecLiteral(data)
      case SmtlibBvAdd(left, right) => bitvec2(left, right, SmtlibBvAdd(_, _))
      case SmtlibBvAnd(left, right) => bitvec2(left, right, SmtlibBvAnd(_, _))
      case SmtlibBvMul(left, right) => bitvec2(left, right, SmtlibBvMul(_, _))
      case SmtlibBvNeg(bv) => SmtlibBvNeg(bitvec(bv)._1)
      case SmtlibBvNot(bv) => SmtlibBvNot(bitvec(bv)._1)
      case SmtlibBvOr(left, right) => bitvec2(left, right, SmtlibBvOr(_, _))
      case SmtlibBvShl(left, right) => bitvec2(left, right, SmtlibBvShl(_, _))
      case SmtlibBvShr(left, right) => bitvec2(left, right, SmtlibBvShr(_, _))
      case SmtlibBvUDiv(left, right) => bitvec2(left, right, SmtlibBvUDiv(_, _))
      case SmtlibBvULt(left, right) => bitvec2(left, right, SmtlibBvULt(_, _))
      case SmtlibBvURem(left, right) => bitvec2(left, right, SmtlibBvURem(_, _))
      case SmtlibConcat(left, right) => bitvec2(left, right, SmtlibConcat(_, _))
      case SmtlibExtract(inclusiveEndIndexFromRight, startIndexFromRight, bv) =>
        SmtlibExtract(inclusiveEndIndexFromRight, startIndexFromRight, bitvec(bv)._1)
      case SmtlibFp(sign, exponent, mantissa) =>
        SmtlibFp(coerce(sign, TSmtlibBitVector(1)), bitvec(exponent)._1, bitvec(mantissa)._1)
      case SmtlibFpAbs(arg) => SmtlibFpAbs(fp(arg)._1)
      case SmtlibFpAdd(left, right) => fp2(left, right, SmtlibFpAdd(_, _))
      case SmtlibFpCast(arg, exponentBits, mantissaAndSignBits) =>
        SmtlibFpCast(fp(arg)._1, exponentBits, mantissaAndSignBits)
      case SmtlibFpDiv(left, right) => fp2(left, right, SmtlibFpDiv(_, _))
      case SmtlibFpEq(left, right) => fp2(left, right, SmtlibFpEq(_, _))
      case SmtlibFpFma(left, right, addend) =>
        val (leftc, t) = fp(left)
        SmtlibFpFma(left, coerce(right, t), coerce(addend, t))
      case SmtlibFpFromReal(arg, e, m) => SmtlibFpFromReal(rat(arg), e, m)
      case SmtlibFpFromSInt(bv, e, m) => SmtlibFpFromSInt(bitvec(bv)._1, e, m)
      case SmtlibFpFromUInt(bv, e, m) => SmtlibFpFromUInt(bitvec(bv)._1, e, m)
      case SmtlibFpGeq(left, right) => fp2(left, right, SmtlibFpGeq(_, _))
      case SmtlibFpGt(left, right) => fp2(left, right, SmtlibFpGt(_, _))
      case SmtlibFpIsInfinite(arg) => SmtlibFpIsInfinite(fp(arg)._1)
      case SmtlibFpIsNaN(arg) => SmtlibFpIsNaN(fp(arg)._1)
      case SmtlibFpIsNegative(arg) => SmtlibFpIsNegative(fp(arg)._1)
      case SmtlibFpIsNormal(arg) => SmtlibFpIsNormal(fp(arg)._1)
      case SmtlibFpIsPositive(arg) => SmtlibFpIsPositive(fp(arg)._1)
      case SmtlibFpIsSubnormal(arg) => SmtlibFpIsSubnormal(fp(arg)._1)
      case SmtlibFpIsZero(arg) => SmtlibFpIsZero(fp(arg)._1)
      case SmtlibFpLeq(left, right) => fp2(left, right, SmtlibFpLeq(_, _))
      case SmtlibFpLt(left, right) => fp2(left, right, SmtlibFpLt(_, _))
      case SmtlibFpMax(left, right) => fp2(left, right, SmtlibFpMax(_, _))
      case SmtlibFpMin(left, right) => fp2(left, right, SmtlibFpMin(_, _))
      case SmtlibFpMul(left, right) => fp2(left, right, SmtlibFpMul(_, _))
      case SmtlibFpNeg(arg) => SmtlibFpNeg(fp(arg)._1)
      case SmtlibFpRem(left, right) => fp2(left, right, SmtlibFpRem(_, _))
      case SmtlibFpRoundToIntegral(arg) => SmtlibFpRoundToIntegral(fp(arg)._1)
      case SmtlibFpSqrt(arg) => SmtlibFpSqrt(fp(arg)._1)
      case SmtlibFpSub(left, right) => fp2(left, right, SmtlibFpSub(_, _))
      case SmtlibFpToReal(arg) => SmtlibFpToReal(fp(arg)._1)
      case SmtlibFpToSInt(arg, bits) => SmtlibFpToSInt(fp(arg)._1, bits)
      case SmtlibFpToUInt(arg, bits) => SmtlibFpToUInt(fp(arg)._1, bits)
      case SmtlibLiteralString(data) => SmtlibLiteralString(data)
      case SmtlibReAll() => SmtlibReAll()
      case SmtlibReAllChar() => SmtlibReAllChar()
      case SmtlibReComp(arg) => SmtlibReComp(reglan(arg))
      case SmtlibReConcat(left, right) => SmtlibReConcat(reglan(left), reglan(right))
      case SmtlibReContains(re, str) => SmtlibReContains(reglan(re), smtstr(str))
      case SmtlibReDiff(left, right) => SmtlibReDiff(reglan(left), reglan(right))
      case SmtlibReFromStr(arg) => SmtlibReFromStr(smtstr(arg))
      case SmtlibReInter(left, right) => SmtlibReInter(reglan(left), reglan(right))
      case SmtlibReNone() => SmtlibReNone()
      case SmtlibReOpt(arg) => SmtlibReOpt(reglan(arg))
      case SmtlibRePlus(arg) => SmtlibRePlus(reglan(arg))
      case SmtlibReRange(left, right) => SmtlibReRange(smtstr(left), smtstr(right))
      case SmtlibReRepeat(count, arg) => SmtlibReRepeat(count, reglan(arg))
      case SmtlibReRepeatRange(from, to, arg) => SmtlibReRepeatRange(from, to, reglan(arg))
      case SmtlibReStar(arg) => SmtlibReStar(reglan(arg))
      case SmtlibReUnion(left, right) => SmtlibReUnion(reglan(left), reglan(right))
      case SmtlibRNA() => SmtlibRNA()
      case SmtlibRNE() => SmtlibRNE()
      case SmtlibRTN() => SmtlibRTN()
      case SmtlibRTP() => SmtlibRTP()
      case SmtlibRTZ() => SmtlibRTZ()
      case SmtlibSelect(arr, i) =>
        val (e, t) = smtarr(arr)
        SmtlibSelect(e, i.zip(t.index).map { case (e, t) => coerce(e, t) })
      case SmtlibStore(arr, i, x) =>
        val (e, t) = smtarr(arr)
        SmtlibStore(e, i.zip(t.index).map { case (e, t) => coerce(e, t) }, coerce(x, t.value))
      case SmtlibStrAt(str, i) => SmtlibStrAt(smtstr(str), int(i))
      case SmtlibStrConcat(left, right) => SmtlibStrConcat(smtstr(left), smtstr(right))
      case SmtlibStrContains(left, right) => SmtlibStrContains(smtstr(left), smtstr(right))
      case SmtlibStrFromCode(arg) => SmtlibStrFromCode(int(arg))
      case SmtlibStrFromInt(arg) => SmtlibStrFromInt(int(arg))
      case SmtlibStrIndexOf(haystack, needle, fromIndex) => SmtlibStrIndexOf(smtstr(haystack), smtstr(needle), int(fromIndex))
      case SmtlibStrIsDigit(arg) => SmtlibStrIsDigit(smtstr(arg))
      case SmtlibStrLen(arg) => SmtlibStrLen(smtstr(arg))
      case SmtlibStrLeq(left, right) => SmtlibStrLeq(smtstr(left), smtstr(right))
      case SmtlibStrLt(left, right) => SmtlibStrLt(smtstr(left), smtstr(right))
      case SmtlibStrPrefixOf(left, right) => SmtlibStrPrefixOf(smtstr(left), smtstr(right))
      case SmtlibStrReplace(haystack, needle, replacement) => SmtlibStrReplace(smtstr(haystack), smtstr(needle), smtstr(replacement))
      case SmtlibStrReplaceAll(haystack, needle, replacement) => SmtlibStrReplaceAll(smtstr(haystack), smtstr(needle), smtstr(replacement))
      case SmtlibStrReplaceRe(haystack, re, replacement) => SmtlibStrReplaceRe(smtstr(haystack), reglan(re), smtstr(replacement))
      case SmtlibStrReplaceReAll(haystack, re, replacement) => SmtlibStrReplaceReAll(smtstr(haystack), reglan(re), smtstr(replacement))
      case SmtlibStrSuffixOf(left, right) => SmtlibStrSuffixOf(smtstr(left), smtstr(right))
      case SmtlibStrToCode(arg) => SmtlibStrToCode(smtstr(arg))
      case SmtlibStrToInt(arg) => SmtlibStrToInt(smtstr(arg))
      case SmtlibSubstr(str, i, n) => SmtlibSubstr(smtstr(str), int(i), int(n))
      case SmtlibToFp(bv, e, m) => SmtlibToFp(coerce(bv, TSmtlibBitVector(e + m)), e, m)
      case Star(left, right) =>
        Star(res(left), res(right))
      case starall @ Starall(bindings, triggers, body) =>
        Starall(bindings, triggers, res(body))(starall.blame)
      case SubBag(left, right) =>
        val (coercedLeft, leftBag) = bag(left)
        val (coercedRight, rightBag) = bag(right)
        val sharedType = Types.leastCommonSuperType(leftBag.element, rightBag.element)
        SubBag(coerce(coercedLeft, TBag(sharedType)), coerce(coercedRight, TBag(sharedType)))
      case SubBagEq(left, right) =>
        val (coercedLeft, leftBag) = bag(left)
        val (coercedRight, rightBag) = bag(right)
        val sharedType = Types.leastCommonSuperType(leftBag.element, rightBag.element)
        SubBagEq(coerce(coercedLeft, TBag(sharedType)), coerce(coercedRight, TBag(sharedType)))
      case SubSet(left, right) =>
        val (coercedLeft, leftSet) = set(left)
        val (coercedRight, rightSet) = set(right)
        val sharedType = Types.leastCommonSuperType(leftSet.element, rightSet.element)
        SubSet(coerce(coercedLeft, TSet(sharedType)), coerce(coercedRight, TSet(sharedType)))
      case SubSetEq(left, right) =>
        val (coercedLeft, leftSet) = set(left)
        val (coercedRight, rightSet) = set(right)
        val sharedType = Types.leastCommonSuperType(leftSet.element, rightSet.element)
        SubSetEq(coerce(coercedLeft, TSet(sharedType)), coerce(coercedRight, TSet(sharedType)))
      case SubType(left, right) =>
        SubType(left, right)
      case Sum(bindings, condition, main) =>
        Sum(bindings, bool(condition), int(main))
      case SuperType(left, right) =>
        SuperType(left, right)
      case Tail(xs) =>
        Tail(seq(xs)._1)
      case Take(xs, count) =>
        Take(seq(xs)._1, int(count))
      case Then(value, post) =>
        Then(value, post)
      case ThisModel(ref) =>
        ThisModel(ref)
      case ThisSeqProg(ref) =>
        ThisSeqProg(ref)
      case ThisObject(ref) =>
        ThisObject(ref)
      case TupGet(tup, index) =>
        TupGet(tuple(tup)._1, index)
      case TypeOf(expr) =>
        TypeOf(expr)
      case TypeValue(value) =>
        TypeValue(value)
      case UMinus(arg) =>
        firstOk(e, s"Expected operand to be numeric, but got ${arg.t}.",
          UMinus(int(arg)),
          UMinus(rat(arg)),
        )
      case u @ Unfolding(pred, body) =>
        Unfolding(res(pred), body)(u.blame)
      case UntypedLiteralBag(values) =>
        val sharedType = Types.leastCommonSuperType(values.map(_.t))
        UntypedLiteralBag(values.map(coerce(_, sharedType)))
      case UntypedLiteralSeq(values) =>
        val sharedType = Types.leastCommonSuperType(values.map(_.t))
        UntypedLiteralSeq(values.map(coerce(_, sharedType)))
      case UntypedLiteralSet(values) =>
        val sharedType = Types.leastCommonSuperType(values.map(_.t))
        UntypedLiteralSet(values.map(coerce(_, sharedType)))
      case ValidArray(arr, len) =>
        ValidArray(array(arr)._1, int(len))
      case ValidMatrix(mat, w, h) =>
        ValidMatrix(arrayMatrix(mat)._1, int(w), int(h))
      case value: BooleanValue[Pre] => e
      case value: IntegerValue[Pre] => e
      case value: FloatValue[Pre] => e
      case value @ Value(loc) =>
        Value(loc)
      case values @ Values(arr, from, to) =>
        Values(array(arr)._1, int(from), int(to))(values.blame)
      case VectorCompare(left, right) =>
        val (coercedLeft, leftType) = seq(left)
        val (coercedRight, rightType) = seq(right)
        val sharedType = Types.leastCommonSuperType(leftType.element, rightType.element)
        val seqType = TSeq(sharedType)
        VectorCompare(coerce(coercedLeft, seqType), coerce(coercedRight, seqType))
      case VectorRepeat(e) =>
        VectorRepeat(e)
      case VectorSum(indices, vec) =>
        VectorSum(coerce(indices, TSeq[Pre](TInt())), coerce(vec, TSeq[Pre](TRational())))
      case Void() =>
        Void()
      case Wand(left, right) =>
        Wand(res(left), res(right))
      case With(pre, value) =>
        With(pre, value)
      case WritePerm() =>
        WritePerm()
      case Z3ArrayConst(domain, codomain, value) => Z3ArrayConst(domain, codomain, coerce(value, codomain))
      case Z3ArrayMap(ref, arg +: args) =>
        val (_, TSmtlibArray(keyType, _)) = smtarr(arg)
        val ts = ref.ref.decl.args.map(_.t).map(TSmtlibArray(keyType, _))
        Z3ArrayMap(ref, (arg +: args).zip(ts).map { case (e, t) => coerce(e, t) })
      case Z3ArrayMap(ref, _) => Z3ArrayMap(ref, Nil)
      case Z3ArrayOfFunction(ref) => Z3ArrayOfFunction(ref)
      case Z3BvNand(left, right) => bitvec2(left, right, Z3BvNand(_, _))
      case Z3BvNor(left, right) => bitvec2(left, right, Z3BvNor(_, _))
      case Z3BvSMod(left, right) => bitvec2(left, right, Z3BvSMod(_, _))
      case Z3BvSRem(left, right) => bitvec2(left, right, Z3BvSRem(_, _))
      case Z3BvSShr(left, right) => bitvec2(left, right, Z3BvSShr(_, _))
      case Z3BvSub(left, right) => bitvec2(left, right, Z3BvSub(_, _))
      case Z3BvXnor(left, right) => bitvec2(left, right, Z3BvXnor(_, _))
      case Z3SeqAt(seq, offset) => Z3SeqAt(z3seq(seq)._1, int(offset))
      case Z3SeqConcat(left, right) => Z3SeqConcat(z3seq(left)._1, z3seq(right)._1)
      case Z3SeqContains(seq, subseq) => Z3SeqContains(z3seq(seq)._1, z3seq(subseq)._1)
      case Z3SeqEmpty(elementType) => Z3SeqEmpty(elementType)
      case Z3SeqExtract(seq, offset, len) => Z3SeqExtract(z3seq(seq)._1, int(offset), int(len))
      case Z3SeqFoldl(f, base, seq) =>
        val (cseq, seqt) = z3seq(seq)
        Z3SeqFoldl(coerce(f, TSmtlibArray(Seq(base.t, seqt.element), base.t)), base, cseq)
      case Z3SeqFoldlI(f, offset, base, seq) =>
        val (cseq, seqt) = z3seq(seq)
        Z3SeqFoldlI(coerce(f, TSmtlibArray(Seq(TInt(), base.t, seqt.element), base.t)), int(offset), base, cseq)
      case Z3SeqLen(arg) => Z3SeqLen(z3seq(arg)._1)
      case Z3SeqMap(f, seq) =>
        val (cf, arrt) = smtarr(f)
        if(arrt.index.size != 1) coerce(f, TSmtlibArray(Seq(TAnyValue()), arrt.value))
        Z3SeqMap(cf, coerce(seq, TSmtlibSeq(arrt.index.head)))
      case Z3SeqMapI(f, offset, seq) =>
        val (cf, arrt) = smtarr(f)
        if(arrt.index.size != 2) coerce(f, TSmtlibArray(Seq(TInt(), TAnyValue()), arrt.value))
        Z3SeqMapI(cf, int(offset), coerce(seq, TSmtlibSeq(arrt.index(1))))
      case Z3SeqNth(seq, offset) => Z3SeqNth(z3seq(seq)._1, int(offset))
      case Z3SeqPrefixOf(pre, subseq) => Z3SeqPrefixOf(z3seq(pre)._1, z3seq(subseq)._1)
      case Z3SeqReplace(haystack, needle, replacement) => Z3SeqReplace(z3seq(haystack)._1, z3seq(needle)._1, z3seq(replacement)._1)
      case Z3SeqSuffixOf(post, seq) => Z3SeqSuffixOf(z3seq(post)._1, z3seq(seq)._1)
      case Z3SeqUnit(arg) => Z3SeqUnit(arg)
      case Z3TransitiveClosure(ref, args) => Z3TransitiveClosure(ref, coerceArgs(args, ref.ref.decl))
      case localIncoming: BipLocalIncomingData[Pre] => localIncoming
      case glue: JavaBipGlue[Pre] => glue
      case LlvmLocal(name) => e
    }
  }

  def coerce(stat: Statement[Pre]): Statement[Pre] = {
    implicit val o: Origin = stat.o
    stat match {
      case a @ Assert(assn) => Assert(res(assn))(a.blame)
      case a @ Assign(target, value) =>
        try { Assign(target, coerce(value, target.t))(a.blame) } catch {
          case err: Incoercible =>
            println(err.text)
            throw err
        }
      case Assume(assn) => Assume(bool(assn))
      case Block(statements) => Block(statements)
      case Branch(branches) => Branch(branches.map { case (cond, effect) => (bool(cond), effect) })
      case Break(label) => Break(label)
      case Case(pattern) => Case(pattern)
      case CDeclarationStatement(decl) => CDeclarationStatement(decl)
      case CGoto(label) => CGoto(label)
      case c @ Commit(obj) => Commit(cls(obj))(c.blame)
      case Continue(label) => Continue(label)
      case CPPDeclarationStatement(decl) => CPPDeclarationStatement(decl)
      case CPPLifetimeScope(body) => CPPLifetimeScope(body)
      case DefaultCase() => DefaultCase()
      case Eval(expr) => Eval(expr)
      case e @ Exhale(assn) => Exhale(res(assn))(e.blame)
      case Extract(body) => Extract(body)
      case f @ Fold(assn) => Fold(res(assn))(f.blame)
      case f @ Fork(obj) => Fork(cls(obj))(f.blame)
      case proof @ FramedProof(pre, body, post) => FramedProof(res(pre), body, res(post))(proof.blame)
      case Goto(lbl) => Goto(lbl)
      case GpgpuAtomic(impl, before, after) => GpgpuAtomic(impl, before, after)
      case b @ GpgpuBarrier(requires, ensures, specifier) => GpgpuBarrier(res(requires), res(ensures), specifier)(b.blame)
      case Havoc(loc) => Havoc(loc)
      case IndetBranch(branches) => IndetBranch(branches)
      case Inhale(assn) => Inhale(res(assn))
      case inv @ InvokeProcedure(ref, args, outArgs, typeArgs, givenMap, yields) =>
        InvokeProcedure(ref, coerceArgs(args, ref.decl, typeArgs), outArgs, typeArgs, coerceGiven(givenMap), coerceYields(yields, args.head))(inv.blame)
      case inv @ InvokeMethod(obj, ref, args, outArgs, typeArgs, givenMap, yields) =>
        InvokeMethod(cls(obj), ref, coerceArgs(args, ref.decl, typeArgs), outArgs, typeArgs, coerceGiven(givenMap), coerceYields(yields, args.head))(inv.blame)
      case JavaLocalDeclarationStatement(decl) => JavaLocalDeclarationStatement(decl)
      case j @ Join(obj) => Join(cls(obj))(j.blame)
      case Label(decl, stat) => Label(decl, stat)
      case LocalDecl(local) => LocalDecl(local)
      case l @ Lock(obj) => Lock(cls(obj))(l.blame)
      case Loop(init, cond, update, contract, body) => Loop(init, bool(cond), update, contract, body)
      case LlvmLoop(cond, contract, body) => LlvmLoop(bool(cond), contract, body)
      case ModelDo(model, perm, after, action, impl) => ModelDo(model, rat(perm), after, action, impl)
      case n @ Notify(obj) => Notify(cls(obj))(n.blame)
      case at @ ParAtomic(inv, content) => ParAtomic(inv, content)(at.blame)
      case bar @ ParBarrier(block, invs, requires, ensures, content) => ParBarrier(block, invs, res(requires), res(ensures), content)(bar.blame)
      case p @ ParInvariant(decl, inv, content) => ParInvariant(decl, res(inv), content)(p.blame)
      case ParStatement(impl) => ParStatement(impl)
      case RangedFor(iter, contract, body) => RangedFor(iter, contract, body)
      case Recv(ref) => Recv(ref)
      case r @ Refute(assn) => Refute(res(assn))(r.blame)
      case Return(result) => Return(result) // TODO coerce return, make AmbiguousReturn?
      case Scope(locals, body) => Scope(locals, body)
      case send @ Send(decl, offset, resource) => Send(decl, offset, res(resource))(send.blame)
      case ass @ SilverFieldAssign(obj, field, value) => SilverFieldAssign(ref(obj), field, coerce(value, field.decl.t))(ass.blame)
      case SilverLocalAssign(v, value) => SilverLocalAssign(v, coerce(value, v.decl.t))
      case SilverNewRef(v, fields) => SilverNewRef(v, fields)
      case SpecIgnoreEnd() => SpecIgnoreEnd()
      case SpecIgnoreStart() => SpecIgnoreStart()
      case Switch(expr, body) => Switch(expr, body)
      case s @ Synchronized(obj, body) => Synchronized(cls(obj), body)(s.blame)
      case t @ Throw(obj) => Throw(cls(obj))(t.blame)
      case TryCatchFinally(body, after, catches) => TryCatchFinally(body, after, catches)
      case u @ Unfold(assn) => Unfold(res(assn))(u.blame)
      case u @ Unlock(obj) => Unlock(cls(obj))(u.blame)
      case VecBlock(iters, requires, ensures, content) => VecBlock(iters, res(requires), res(ensures), content)
      case w @ Wait(obj) => Wait(cls(obj))(w.blame)
      case w @ WandApply(assn) => WandApply(res(assn))(w.blame)
      case w @ WandPackage(expr, stat) => WandPackage(res(expr), stat)(w.blame)
      case VeyMontAssignExpression(t,a) => VeyMontAssignExpression(t,a)
      case CommunicateX(r,s,t,a) => CommunicateX(r,s,t,a)
      case c @ PVLCommunicate(s, r) if r.fieldType == s.fieldType => PVLCommunicate(s, r)(c.blame)
      case comm@PVLCommunicate(s, r) => throw IncoercibleExplanation(comm, s"The receiver should have type ${s.fieldType}, but actually has type ${r.fieldType}.")
      case c @ Communicate(r, s) if r.field.decl.t == s.field.decl.t => Communicate(r, s)(c.blame)
      case comm@Communicate(r, s) => throw IncoercibleExplanation(comm, s"The receiver should have type ${s.field.decl.t}, but actually has type ${r.field.decl.t}.")
      case a @ PVLSeqAssign(r, f, v) =>
        try { PVLSeqAssign(r, f, coerce(v, f.decl.t))(a.blame) } catch {
          case err: Incoercible =>
            println(err.text)
            throw err
        }
      case a @ SeqAssign(r, f, v) =>
        try { SeqAssign(r, f, coerce(v, f.decl.t))(a.blame) } catch {
          case err: Incoercible =>
            println(err.text)
            throw err
        }
      case s: SeqBranch[Pre] => s
      case s: SeqLoop[Pre] => s
      case branch@UnresolvedSeqBranch(branches) => UnresolvedSeqBranch(branches.map { case (cond, effect) => (bool(cond), effect) })(branch.blame)
      case branch@PVLBranch(branches) => PVLBranch(branches.map { case (cond, effect) => (bool(cond), effect) })(branch.blame)
      case loop@UnresolvedSeqLoop(cond, contract, body) => UnresolvedSeqLoop(bool(cond), contract, body)(loop.blame)
      case loop@PVLLoop(init, cond, update, contract, body) => PVLLoop(init, bool(cond), update, contract, body)(loop.blame)
    }
  }

  def coerce(decl: Declaration[Pre]): Declaration[Pre] = {
    implicit val o: Origin = decl.o
    decl match {
      case unit: CTranslationUnit[Pre] =>
        new CTranslationUnit(unit.declarations)
      case unit: CPPTranslationUnit[Pre] =>
        new CPPTranslationUnit(unit.declarations)
      case variable: HeapVariable[Pre] =>
        new HeapVariable(variable.t)
      case rule: SimplificationRule[Pre] =>
        new SimplificationRule[Pre](bool(rule.axiom))
      case dataType: AxiomaticDataType[Pre] =>
        dataType
      case clazz: Class[Pre] =>
        new Class[Pre](clazz.declarations, clazz.supports, res(clazz.intrinsicLockInvariant))
      case enum: Enum[Pre] =>
        enum
      case enumConstant: EnumConstant[Pre] =>
        enumConstant
      case model: Model[Pre] =>
        model
      case function: Function[Pre] =>
        new Function[Pre](function.returnType, function.args, function.typeArgs, function.body.map(coerce(_, function.returnType)), function.contract, function.inline, function.threadLocal)(function.blame)
      case procedure: Procedure[Pre] =>
        procedure
      case predicate: Predicate[Pre] =>
        new Predicate[Pre](predicate.args, predicate.body.map(res), predicate.threadLocal, predicate.inline)
      case definition: CFunctionDefinition[Pre] =>
        definition
      case declaration: CGlobalDeclaration[Pre] =>
        declaration
      case definition: CPPFunctionDefinition[Pre] =>
        definition
      case declaration: CPPGlobalDeclaration[Pre] =>
        declaration
      case namespace: JavaNamespace[Pre] =>
        namespace
      case clazz: JavaClass[Pre] =>
        new JavaClass[Pre](clazz.name, clazz.modifiers, clazz.typeParams, res(clazz.intrinsicLockInvariant), clazz.ext, clazz.imp, clazz.decls)(clazz.blame)
      case interface: JavaInterface[Pre] =>
        interface
      case interface: JavaAnnotationInterface[Pre] =>
        interface
      case field: SilverField[Pre] =>
        field
      case function: InstanceFunction[Pre] =>
        new InstanceFunction[Pre](function.returnType, function.args, function.typeArgs, function.body.map(coerce(_, function.returnType)), function.contract, function.inline, function.threadLocal)(function.blame)
      case method: InstanceMethod[Pre] =>
        method
      case predicate: InstancePredicate[Pre] =>
        new InstancePredicate[Pre](predicate.args, predicate.body.map(res), predicate.threadLocal, predicate.inline)
      case field: InstanceField[Pre] =>
        field
      case method: RunMethod[Pre] =>
        method
      case method: InstanceOperatorMethod[Pre] =>
        method
      case function: InstanceOperatorFunction[Pre] =>
        new InstanceOperatorFunction[Pre](function.returnType, function.operator, function.args, function.body.map(coerce(_, function.returnType)), function.contract, function.inline, function.threadLocal)(function.o)
      case initialization: JavaSharedInitialization[Pre] =>
        initialization
      case fields: JavaFields[Pre] =>
        new JavaFields[Pre](fields.modifiers, fields.t, fields.decls.map {
          case JavaVariableDeclaration(name, dims, None) => JavaVariableDeclaration(name, dims, None)
          case JavaVariableDeclaration(name, dims, Some(v)) =>
            JavaVariableDeclaration(name, dims, Some(coerce(v, FuncTools.repeat[Type[Pre]](TArray(_), dims, fields.t))))
        })
      case constructor: JavaConstructor[Pre] =>
        constructor
      case method: JavaMethod[Pre] =>
        method
      case param: JavaParam[Pre] =>
        param
      case method: JavaAnnotationMethod[Pre] =>
        method
      case constructor: PVLConstructor[Pre] =>
        constructor
      case field: ModelField[Pre] =>
        field
      case proc: ModelProcess[Pre] =>
        new ModelProcess[Pre](proc.args, process(proc.impl), bool(proc.requires), bool(proc.ensures), proc.modifies, proc.accessible)(proc.blame)
      case action: ModelAction[Pre] =>
        new ModelAction[Pre](action.args, bool(action.requires), bool(action.ensures), action.modifies, action.accessible)
      case axiom: ADTAxiom[Pre] =>
        new ADTAxiom[Pre](bool(axiom.axiom))
      case function: ADTFunction[Pre] =>
        function
      case variable: Variable[Pre] =>
        variable
      case decl: LabelDecl[Pre] =>
        decl
      case decl: SendDecl[Pre] =>
        decl
      case decl: ParBlockDecl[Pre] =>
        decl
      case decl: ParInvariantDecl[Pre] =>
        decl
      case param: CParam[Pre] =>
        param
      case decl: CLocalDeclaration[Pre] =>
        decl
      case param: CPPParam[Pre] =>
        param
      case decl: CPPLocalDeclaration[Pre] =>
        decl
      case declaration: JavaLocalDeclaration[Pre] =>
        new JavaLocalDeclaration[Pre](declaration.modifiers, declaration.t, declaration.decls.map {
          case JavaVariableDeclaration(name, dims, None) => JavaVariableDeclaration(name, dims, None)
          case JavaVariableDeclaration(name, dims, Some(v)) =>
            JavaVariableDeclaration(name, dims, Some(coerce(v, FuncTools.repeat[Type[Pre]](TArray(_), dims, declaration.t))))
        })
      case seqProg: SeqProg[Pre] => seqProg
      case thread: Endpoint[Pre] => new Endpoint(thread.cls, thread.constructor, thread.args)
      case bc: BipConstructor[Pre] => new BipConstructor(bc.args, bc.body, bc.requires)(bc.blame)
      case bc: BipComponent[Pre] =>
        new BipComponent(bc.fqn, res(bc.invariant), bc.initial)
      case bsp: BipStatePredicate[Pre] =>
        new BipStatePredicate(bool(bsp.expr))
      case trans: BipTransition[Pre] =>
        new BipTransition(trans.signature, trans.port, trans.source, trans.target, trans.data, trans.guard,
          bool(trans.requires),
          bool(trans.ensures),
          trans.body
        )(trans.blame)
      case data: BipIncomingData[Pre] => data
      case data: BipOutgoingData[Pre] => data
      case data: BipData[Pre] => data
      case guard: BipGuard[Pre] => guard
      case port: BipPort[Pre] => port
      case glue: JavaBipGlueContainer[Pre] => glue
      case glue: BipGlue[Pre] => glue
      case synchronization: BipPortSynchronization[Pre] => synchronization
      case synchronization: BipTransitionSynchronization[Pre] => synchronization
      case definition: LlvmFunctionDefinition[Pre] => definition
      case typ: ProverType[Pre] => typ
      case func: ProverFunction[Pre] => func
      case function: LlvmSpecFunction[Pre] =>
        new LlvmSpecFunction[Pre](function.name, function.returnType, function.args, function.typeArgs, function.body.map(coerce(_, function.returnType)), function.contract, function.inline, function.threadLocal)(function.blame)
      case glob: LlvmGlobal[Pre] => glob
      case endpoint: PVLEndpoint[Pre] => endpoint
      case seqProg: PVLSeqProg[Pre] => seqProg
      case seqRun: PVLSeqRun[Pre] => seqRun
      }
  }

  def coerce(region: ParRegion[Pre]): ParRegion[Pre] = {
    implicit val o: Origin = region.o
    region match {
      case region @ ParParallel(regions) => ParParallel(regions)(region.blame)
      case region @ ParSequential(regions) => ParSequential(regions)(region.blame)
      case region @ ParBlock(decl, iters, context_everywhere, requires, ensures, content) =>
        ParBlock(decl, iters, res(context_everywhere), res(requires), res(ensures), content)(region.blame)
    }
  }

  def coerce(node: Verification[Pre]): Verification[Pre] = {
    implicit val o: Origin = node.o
    val Verification(tasks, expectedErrors) = node
    Verification(tasks, expectedErrors)
  }

  def coerce(node: VerificationContext[Pre]): VerificationContext[Pre] = {
    implicit val o: Origin = node.o
    val VerificationContext(program) = node
    VerificationContext(program)
  }

  def coerce(node: Program[Pre]): Program[Pre] = {
    implicit val o: Origin = node.o
    val Program(decl) = node
    Program(decl)(node.blame)
  }

  // PB: types may very well contain expressions eventually, but for now they don't.
  def coerce(node: Type[Pre]): Type[Pre] =
    node

  def coerce(node: LoopContract[Pre]): LoopContract[Pre] = {
    implicit val o: Origin = node.o
    node match {
      case li @ LoopInvariant(invariant, decreases) =>
        LoopInvariant(res(invariant), decreases)(li.blame)
      case ic @ IterationContract(requires, ensures, context_everywhere) =>
        IterationContract(res(requires), res(ensures), res(context_everywhere))(ic.blame)
    }
  }

  def coerce(node: CatchClause[Pre]): CatchClause[Pre] = {
    implicit val o: Origin = node.o
    val CatchClause(decl, body) = node
    CatchClause(decl, body)
  }

  def coerce(node: IterVariable[Pre]): IterVariable[Pre] = {
    implicit val o: Origin = node.o
    val IterVariable(variable, from, to) = node
    IterVariable(variable, int(from), int(to))
  }

  def coerce(node: SignalsClause[Pre]): SignalsClause[Pre] = {
    implicit val o: Origin = node.o
    val SignalsClause(binding, assn) = node
    SignalsClause(binding, res(assn))
  }

  def coerce(node: DecreasesClause[Pre]): DecreasesClause[Pre] = {
    implicit val o: Origin = node.o
    node match {
      case DecreasesClauseAssume() =>
        DecreasesClauseAssume()
      case DecreasesClauseNoRecursion() =>
        DecreasesClauseNoRecursion()
      case DecreasesClauseTuple(exprs) =>
        DecreasesClauseTuple(exprs.map(int))  // Since we currently only support integers
    }
  }

  def coerce(node: ApplicableContract[Pre]): ApplicableContract[Pre] = {
    implicit val o: Origin = node.o
    val ApplicableContract(requires, ensures, context_everywhere, signals, givenArgs, yieldsArgs, decreases) = node
    ApplicableContract(requires, ensures, res(context_everywhere), signals, givenArgs, yieldsArgs, decreases)(node.blame)
  }

  def coerce(node: AccountedPredicate[Pre]): AccountedPredicate[Pre] = {
    implicit val o: Origin = node.o
    node match {
      case UnitAccountedPredicate(pred) =>
        UnitAccountedPredicate(res(pred))
      case SplitAccountedPredicate(left, right) =>
        SplitAccountedPredicate(left, right)
    }
  }

  def coerce(node: FieldFlag[Pre]): FieldFlag[Pre] = {
    implicit val o: Origin = node.o
    node match {
      case value: Final[_] => value
    }
  }


  def coerce(node: Location[Pre]): Location[Pre] = {
    implicit val o: Origin = node.o
    node match {
      case HeapVariableLocation(ref) =>
        HeapVariableLocation(ref)
      case FieldLocation(obj, field) =>
        FieldLocation(cls(obj), field)
      case ModelLocation(obj, field) =>
        ModelLocation(model(obj)._1, field)
      case SilverFieldLocation(obj, field) =>
        SilverFieldLocation(ref(obj), field)
      case a @ ArrayLocation(arrayObj, subscript) =>
        ArrayLocation(array(arrayObj)._1, int(subscript))(a.blame)
      case p @ PointerLocation(pointerExp) =>
        PointerLocation(pointer(pointerExp)._1)(p.blame)
      case PredicateLocation(predicate, args) =>
        PredicateLocation(predicate, coerceArgs(args, predicate.decl))
      case InstancePredicateLocation(predicate, obj, args) =>
        InstancePredicateLocation(predicate, cls(obj), coerceArgs(args, predicate.decl))
      case al @ AmbiguousLocation(expr) =>
        AmbiguousLocation(expr)(al.blame)
    }
  }

  def coerce(node: CDeclarationSpecifier[Pre]): CDeclarationSpecifier[Pre] = {
    implicit val o: Origin = node.o
    node match {
      case CPure() => CPure()
      case CInline() => CInline()
      case CTypedef() => CTypedef()
      case CExtern() => CExtern()
      case CStatic() => CStatic()
      case GPULocal() => GPULocal()
      case GPUGlobal() => GPUGlobal()
      case CVoid() => CVoid()
      case CChar() => CChar()
      case CShort() => CShort()
      case CInt() => CInt()
      case CLong() => CLong()
      case CSigned() => CSigned()
      case CUnsigned() => CUnsigned()
      case CBool() => CBool()
      case CTypedefName(name) => CTypedefName(name)
      case CSpecificationType(t) => CSpecificationType(t)
      case CTypeQualifierDeclarationSpecifier(typeQual) =>
        CTypeQualifierDeclarationSpecifier(typeQual)
      case specifier: CFunctionSpecifier[Pre] => specifier
      case specifier: CAlignmentSpecifier[Pre] => specifier
      case ck @ CUDAKernel() => CUDAKernel()(ck.blame)
      case ok @ OpenCLKernel() => OpenCLKernel()(ok.blame)
    }
  }

  def coerce(node: CTypeQualifier[Pre]): CTypeQualifier[Pre] = {
    implicit val o: Origin = node.o
    node match {
      case CConst() => CConst()
      case CRestrict() => CRestrict()
      case CVolatile() => CVolatile()
      case CAtomic() => CAtomic()
    }
  }

  def coerce(node: CPointer[Pre]): CPointer[Pre] = {
    implicit val o: Origin = node.o
    val CPointer(qualifiers) = node
    CPointer(qualifiers)
  }

  def coerce(node: CDeclarator[Pre]): CDeclarator[Pre] = {
    implicit val o: Origin = node.o
    node match {
      case CPointerDeclarator(pointers, inner) =>
        CPointerDeclarator(pointers, inner)
      case C @ CArrayDeclarator(qualifiers, size, inner) =>
        CArrayDeclarator(qualifiers, size.map(int), inner)(C.blame)
      case CTypedFunctionDeclarator(params, varargs, inner) =>
        CTypedFunctionDeclarator(params, varargs, inner)
      case CAnonymousFunctionDeclarator(params, inner) =>
        CAnonymousFunctionDeclarator(params, inner)
      case CName(name) =>
        CName(name)
    }
  }

  def coerce(node: CInit[Pre]): CInit[Pre] = {
    implicit val o: Origin = node.o
    val CInit(decl, init) = node
    CInit(decl, init)
  }

  def coerce(node: CDeclaration[Pre]): CDeclaration[Pre] = {
    implicit val o: Origin = node.o
    val CDeclaration(contract, kernelInvariant, specs, init) = node
    CDeclaration(contract, res(kernelInvariant), specs, init)
  }

  def coerce(node: GpuMemoryFence[Pre]): GpuMemoryFence[Pre] = {
    implicit val o: Origin = node.o
    node match {
      case GpuLocalMemoryFence() => GpuLocalMemoryFence()
      case GpuGlobalMemoryFence() => GpuGlobalMemoryFence()
      case GpuZeroMemoryFence(value) => GpuZeroMemoryFence(value)
    }
  }

  def coerce(node: CPPDeclarator[Pre]): CPPDeclarator[Pre] = {
    implicit val o: Origin = node.o
    node match {
      case CPPAddressingDeclarator(pointers, inner) =>
        CPPAddressingDeclarator(pointers, inner)
      case array @ CPPArrayDeclarator(inner, size) =>
        CPPArrayDeclarator(inner, size.map(int))(array.blame)
      case CPPTypedFunctionDeclarator(params, varargs, inner) =>
        CPPTypedFunctionDeclarator(params, varargs, inner)
      case CPPLambdaDeclarator(params) =>
        CPPLambdaDeclarator(params)
      case CPPName(name) =>
        CPPName(name)
    }
  }

  def coerce(node: CPPDeclarationSpecifier[Pre]): CPPDeclarationSpecifier[Pre] = {
    implicit val o: Origin = node.o
    node match {
      case CPPPure() => CPPPure()
      case CPPInline() => CPPInline()
      case CPPVoid() => CPPVoid()
      case CPPChar() => CPPChar()
      case CPPShort() => CPPShort()
      case CPPInt() => CPPInt()
      case CPPLong() => CPPLong()
      case CPPSigned() => CPPSigned()
      case CPPUnsigned() => CPPUnsigned()
      case CPPBool() => CPPBool()
      case CPPTypedefName(name, arg) => CPPTypedefName(name, arg)
      case CPPSpecificationType(t) => CPPSpecificationType(t)
      case SYCLClassDefName(name, arg) => SYCLClassDefName(name, arg)
    }
  }

  def coerce(node: CPPDeclaration[Pre]): CPPDeclaration[Pre] = {
    implicit val o: Origin = node.o
    val CPPDeclaration(contract, specs, init) = node
    CPPDeclaration(contract, specs, init)
  }

  def coerce(node: CPPAddressing[Pre]): CPPAddressing[Pre] = {
    implicit val o: Origin = node.o
    node match {
      case CPPPointer() => CPPPointer()
      case CPPReference() => CPPReference()
    }
  }

  def coerce(node: CPPInit[Pre]): CPPInit[Pre] = {
    implicit val o: Origin = node.o
    val CPPInit(decl, init) = node
    CPPInit(decl, init)
  }

  def coerce(node: CPPExprOrTypeSpecifier[Pre]): CPPExprOrTypeSpecifier[Pre] = {
    implicit val o: Origin = node.o
    val CPPExprOrTypeSpecifier(expr, typeSpec) = node
    CPPExprOrTypeSpecifier(expr, typeSpec)
  }

  def coerce(node: JavaName[Pre]): JavaName[Pre] = {
    implicit val o: Origin = node.o
    val JavaName(names) = node
    JavaName(names)
  }

  def coerce(node: JavaImport[Pre]): JavaImport[Pre] = {
    implicit val o: Origin = node.o
    val JavaImport(isStatic, name, star) = node
    JavaImport(isStatic, name, star)
  }

  def coerce(node: JavaModifier[Pre]): JavaModifier[Pre] = {
    implicit val o: Origin = node.o
    node match {
      case JavaPublic() => JavaPublic()
      case JavaProtected() => JavaProtected()
      case JavaPrivate() => JavaPrivate()
      case JavaStatic() => JavaStatic()
      case JavaAbstract() => JavaAbstract()
      case JavaFinal() => JavaFinal()
      case JavaStrictFP() => JavaStrictFP()
      case JavaNative() => JavaNative()
      case js @ JavaSynchronized() => JavaSynchronized()(js.blame)
      case JavaTransient() => JavaTransient()
      case JavaVolatile() => JavaVolatile()
      case annotation @ JavaAnnotation(name, args) => JavaAnnotation(name, args)(annotation.blame)
      case JavaPure() => JavaPure()
      case JavaInline() => JavaInline()
      case JavaBipAnnotation() => JavaBipAnnotation()
    }
  }

  def coerce(node: JavaVariableDeclaration[Pre]): JavaVariableDeclaration[Pre] = {
    implicit val o: Origin = node.o
    val JavaVariableDeclaration(name, dim, init) = node
    JavaVariableDeclaration(name, dim, init)
  }

  def coerce(node: Operator[Pre]): Operator[Pre] = {
    implicit val o: Origin = node.o
    node match {
      case OperatorLeftPlus() => OperatorLeftPlus()
      case OperatorRightPlus() => OperatorRightPlus()
    }
  }

  def coerce(node: BipPortType[Pre]): BipPortType[Pre] = {
    implicit val o: Origin = node.o
    node match {
      case BipEnforceable() =>  BipEnforceable()
      case BipSpontaneous() => BipSpontaneous()
      case BipInternal() => BipInternal()
    }
  }

  def coerce(node: BipTransitionSignature[Pre]): BipTransitionSignature[Pre] = node
  def coerce(node: BipGlueDataWire[Pre]): BipGlueDataWire[Pre] = node
  def coerce(node: BipGlueRequires[Pre]): BipGlueRequires[Pre] = node
  def coerce(node: BipGlueAccepts[Pre]): BipGlueAccepts[Pre] = node

  def coerce(node: JavaBipGlueElement[Pre]): JavaBipGlueElement[Pre] = node
  def coerce(node: JavaBipGlueName[Pre]): JavaBipGlueName[Pre] = node

  def coerce(node: LlvmFunctionContract[Pre]): LlvmFunctionContract[Pre] = node
  def coerce(node: LlvmLoopContract[Pre]): LlvmLoopContract[Pre] = node

  def coerce(node: ProverLanguage[Pre]): ProverLanguage[Pre] = node
  def coerce(node: SmtlibFunctionSymbol[Pre]): SmtlibFunctionSymbol[Pre] = node

  def coerce(node: PVLCommunicateAccess[Pre]): PVLCommunicateAccess[Pre] = node
  def coerce(node: PVLCommunicateSubject[Pre]): PVLCommunicateSubject[Pre] = node
  def coerce(node: SeqRun[Pre]): SeqRun[Pre] = node
  def coerce(node: Access[Pre]): Access[Pre] = node
  def coerce(node: Subject[Pre]): Subject[Pre] = node
  def coerce(node: SeqGuard[Pre]): SeqGuard[Pre] = node match {
    case EndpointGuard(endpoint, cond) => EndpointGuard(endpoint, bool(cond))(node.o)
    case UnpointedGuard(cond) => UnpointedGuard(bool(cond))(node.o)
  }

}
