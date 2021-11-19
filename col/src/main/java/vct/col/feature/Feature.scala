package vct.col.feature

sealed trait Feature

case object ApplicableToBeInlined extends Feature // InlineApplicables
case object MethodToBePurified extends Feature // PureMethodsToFunctions
case object FinalField extends Feature // ConstantifyFinalFields
case object DynamicallyTypedCollection extends Feature // PinCollectionTypes
case object CurrentThread extends Feature // EncodeCurrentThread
case object StarSubscript extends Feature // QuantifySubscriptAny
case object AxiomaticLibraryType extends Feature // ImportADT
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

case object MatrixVector extends Feature // TODO
case object NumericReductionOperator extends Feature // TODO

case object ContextSensitiveNode extends Feature // TODO
case object InlineQuantifierPattern extends Feature // TODO
case object MagicWand extends Feature // TODO
case object TypeValuesAndGenerics extends Feature // TODO
case object NonMethodInvocationEvaluation extends Feature // TODO
case object IntrinsicLocks extends Feature // TODO
case object JavaThreads extends Feature // TODO
case object NonVoidReturn extends Feature // TODO
case object NonTrivialBranch extends Feature // TODO
case object NonWhileLoop extends Feature // TODO
case object SendRecv extends Feature // TODO
case object WaitNotify extends Feature // TODO
case object ComputationalLogicOperator extends Feature // TODO
case object ExoticTypes extends Feature // TODO

case object Exceptions extends Feature // TODO
case object ExceptionalLoopControl extends Feature // TODO + ContinueToBreak
case object SwitchStatement extends Feature // TODO
case object TryCatchStatement extends Feature // TODO

case object Models extends Feature // TODO

case object TextTypes extends Feature // delay?
case object TermRewriteRules extends Feature // delay?
case object Exponents extends Feature // delay?
case object BitOperators extends Feature // delay?
case object PermutationOperator extends Feature // delay?
