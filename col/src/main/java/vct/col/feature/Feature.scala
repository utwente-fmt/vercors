package vct.col.feature

sealed trait Feature

case object DynamicallyTypedCollection extends Feature // PinCollectionTypes
case object ContextSensitiveNode extends Feature // TODO
case object CurrentThread extends Feature // EncodeCurrentThread
case object StarSubscript extends Feature // QuantifySubscriptAny
case object AxiomaticLibraryType extends Feature // ImportADT
case object WildcardReadPermission extends Feature // supported
case object SequenceRange extends Feature // supported
case object BuiltinArrayOperators extends Feature // EncodeArrayValues
case object NumericReductionOperator extends Feature // TODO
case object InlineQuantifierPattern extends Feature // TODO
case object Classes extends Feature // ClassToRef
case object Models extends Feature // TODO
case object Pointers extends Feature // ~ImportADT?
case object Arrays extends Feature // ~ImportADT?
case object BitOperators extends Feature // TODO
case object AmbiguousOperators extends Feature // Disambiguate
case object Exponents extends Feature // TODO
case object MagicWand extends Feature // TODO
case object SugarPermissionOperator extends Feature // DesugarPermissionOperators
case object SugarCollectionOperator extends Feature // DesugarCollectionOperators
case object PermutationOperator extends Feature // TODO
case object MatrixVector extends Feature // TODO
case object TypeValuesAndGenerics extends Feature // TODO
case object ExpressionWithSideEffects extends Feature // ResolveExpressionSideEffects
case object NonMethodInvocationEvaluation extends Feature // TODO
case object IntrinsicLocks extends Feature // TODO
case object JavaThreads extends Feature // TODO
case object UnscopedDeclaration extends Feature // TODO
case object NonVoidReturn extends Feature // TODO
case object LoopIterationContract extends Feature // TODO
case object NonTrivialBranch extends Feature // TODO
case object SwitchStatement extends Feature // TODO
case object TryCatchStatement extends Feature // TODO
case object NonWhileLoop extends Feature // TODO
case object ParallelRegion extends Feature // TODO
case object SendRecv extends Feature // TODO
case object SpecIgnore extends Feature // TODO
case object Exceptions extends Feature // TODO
case object WaitNotify extends Feature // TODO
case object ExceptionalLoopControl extends Feature // TODO + ContinueToBreak
case object ExoticTypes extends Feature // TODO
case object TextTypes extends Feature // TODO
case object TermRewriteRules extends Feature // TODO
case object ApplicableToBeInlined extends Feature // TODO
case object MethodToBePurified extends Feature // TODO
case object FinalField extends Feature // TODO
case object ComputationalLogicOperator extends Feature // TODO