package vct.col.feature

import vct.col.ast.Node

case object Feature {
  def scan[G](node: Node[G]): Set[Feature] = {
    val scanner = new FeatureRainbow[G]()
    scanner.scan(node)
    scanner.features.toSet
  }

  def examples[G](node: Node[G]): Map[Feature, Seq[Node[G]]] = {
    val scanner = new FeatureRainbow[G]()
    scanner.scan(node)
    scanner.examples.toMap.map { case (k, v) => k -> v.toSeq }
  }
}

sealed trait Feature

case object ApplicableToBeInlined extends Feature // InlineApplicables
case object MethodToBePurified extends Feature // PureMethodsToFunctions
case object FinalField extends Feature // ConstantifyFinalFields
case object DynamicallyTypedCollection extends Feature // PinCollectionTypes
case object CurrentThread extends Feature // EncodeCurrentThread
case object StarSubscript extends Feature // QuantifySubscriptAny
case object AxiomaticLibraryType extends Feature // ImportADT
case object SilverAxiomaticLibraryType extends Feature
case object WildcardReadPermission extends Feature // supported
case object SequenceRange extends Feature // supported
case object BuiltinArrayOperators extends Feature // EncodeArrayValues
case object Classes extends Feature // ClassToRef
case object Pointers extends Feature // ~ImportADT?
case object Arrays extends Feature // ~ImportADT?
case object AmbiguousOperators extends Feature // Disambiguate
case object SugarPermissionOperator extends Feature // DesugarPermissionOperators
case object SugarCollectionOperator extends Feature // DesugarCollectionOperators
case object ExpressionWithSideEffects extends Feature // ResolveExpressionSideEffects
case object UnscopedDeclaration extends Feature // CollectLocalDeclarations
case object LoopIterationContract extends Feature // IterationContractToParBlock
case object ParallelRegion extends Feature // ParBlockEncoder
case object SpecIgnore extends Feature // FilterSpecIgnore
case object NonTrivialBranch extends Feature // BranchToIfElse
case object NonWhileLoop extends Feature // ForLoopToWhileLoop
case object IntrinsicLocks extends Feature // EncodeIntrinsicLock
case object WaitNotify extends Feature // EncodeIntrinsicLock
case object Evaluation extends Feature // EvaluationTargetDummy
case object ExceptionalLoopControl extends Feature // ContinueToBreak + EncodeBreakReturn
case object NonTrivialLabel extends Feature // EncodeBreakReturn
case object SwitchStatement extends Feature // SwitchToGoto
case object Exceptions extends Feature // EncodeTryThrowSignals
case object TryCatchStatement extends Feature // EncodeTryThrowSignals

case object MatrixVector extends Feature // TODO
case object NumericReductionOperator extends Feature // TODO

case object ContextSensitiveNode extends Feature // TODO
case object InlineQuantifierPattern extends Feature // TODO
case object TypeValuesAndGenerics extends Feature // TODO
case object NonVoidReturn extends Feature // TODO
case object ExoticTypes extends Feature // TODO

case object Models extends Feature // TODO

case object ComputationalLogicOperator extends Feature // delay?
case object MagicWand extends Feature // delay?
case object JavaThreads extends Feature // delay?
case object SendRecv extends Feature // delay?

case object TextTypes extends Feature // delay?
case object TermRewriteRules extends Feature // delay?
case object Exponents extends Feature // delay?
case object BitOperators extends Feature // delay?
case object PermutationOperator extends Feature // delay?

case object Coercions extends Feature
case object AxiomaticDataTypes extends Feature
case object SmtOperators extends Feature
case object JavaSpecific extends Feature
case object CSpecific extends Feature
case object PvlSpecific extends Feature
case object SilverSpecific extends Feature
case object Contracts extends Feature
case object BasicStatement extends Feature
case object Assignment extends Feature
case object Floats extends Feature
case object Gotos extends Feature
case object TerminationMeasure extends Feature
case object Resources extends Feature
case object ProofHelpers extends Feature
case object SmtDeclarations extends Feature
case object SmtTypes extends Feature
case object Methods extends Feature
case object WhileLoops extends Feature
case object RootStructure extends Feature