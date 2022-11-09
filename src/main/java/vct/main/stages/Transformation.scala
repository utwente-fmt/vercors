package vct.main.stages

import com.typesafe.scalalogging.LazyLogging
import hre.progress.Progress
import hre.stages.Stage
import vct.col.ast.{IterationContract, Program, RunMethod, SimplificationRule, Verification, VerificationContext}
import vct.col.check.CheckError
import vct.col.feature
import vct.col.feature.Feature
import vct.col.rewrite._
import vct.col.rewrite.exc._
import vct.col.rewrite.adt._
import vct.col.rewrite.lang.NoSupportSelfLoop
import vct.col.origin.{ExpectedError, FileSpanningOrigin}
import vct.col.print.Printer
import vct.col.rewrite.{Generation, InitialGeneration, RewriterBuilder}
import vct.importer.{PathAdtImporter, Util}
import vct.main.Main.TemporarilyUnsupported
import vct.main.stages.Transformation.TransformationCheckError
import vct.options.types.{Backend, PathOrStd}
import vct.options.Options
import vct.parsers.transform.BlameProvider
import vct.resources.Resources
import vct.result.VerificationError.SystemError

object Transformation {
  case class TransformationCheckError(errors: Seq[CheckError]) extends SystemError {
    override def text: String =
      "A rewrite caused the AST to no longer typecheck:\n" + errors.map(_.toString).mkString("\n")
  }

  private def writeOutFunctions(m: Map[String, PathOrStd]): Seq[(String, Verification[_ <: Generation] => Unit)] =
    m.toSeq.map {
      case (key, out) => (key, (program: Verification[_ <: Generation]) => out.write { writer =>
        Printer(writer).print(program)
      })
    }

  def simplifierFor(path: PathOrStd, options: Options): RewriterBuilder =
    ApplyTermRewriter.BuilderFor(
      ruleNodes = Util.loadPVLLibraryFile[InitialGeneration](path).declarations.collect {
        case rule: SimplificationRule[InitialGeneration] => rule
      },
      debugIn = options.devSimplifyDebugIn,
      debugMatch = options.devSimplifyDebugMatch,
      debugNoMatch = options.devSimplifyDebugNoMatch,
      debugMatchShort = options.devSimplifyDebugMatchShort,
      debugFilterInputKind = options.devSimplifyDebugFilterInputKind,
      debugFilterRule = options.devSimplifyDebugFilterRule,
    )

  def ofOptions(options: Options): Transformation =
    options.backend match {
      case Backend.Silicon | Backend.Carbon =>
        SilverTransformation(
          adtImporter = PathAdtImporter(options.adtPath),
          onBeforePassKey = writeOutFunctions(options.outputBeforePass),
          onAfterPassKey = writeOutFunctions(options.outputAfterPass),
          simplifyBeforeRelations = options.simplifyPaths.map(simplifierFor(_, options)),
          simplifyAfterRelations = options.simplifyPathsAfterRelations.map(simplifierFor(_, options)),
          checkSat = options.devCheckSat,
        )
    }
}

/**
 * Executes a sequence of rewriters. Currently the only concrete implementation is [[SilverTransformation]].
 *
 * Refer to [[RewriterBuilder]] and [[RewriterBuilderArg]] for information on how to use a [[Rewriter]] in the
 * pass chain.
 *
 * @param onBeforePassKey Execute a side effect just before a rewrite pass is executed.
 * @param onAfterPassKey Execute a side effect just after a rewrite pass is executed. The consistency check is done
 *                       before the side effect is performed.
 * @param passes The list of rewrite passes to execute.
 */
class Transformation
(
  val onBeforePassKey: Seq[(String, Verification[_ <: Generation] => Unit)],
  val onAfterPassKey: Seq[(String, Verification[_ <: Generation] => Unit)],
  val passes: Seq[RewriterBuilder]
) extends Stage[VerificationContext[_ <: Generation], Verification[_ <: Generation]] with LazyLogging {
  override def friendlyName: String = "Transformation"
  override def progressWeight: Int = 10

  override def run(input: VerificationContext[_ <: Generation]): Verification[_ <: Generation] = {
    val tempUnsupported = Set[feature.Feature](
      feature.MatrixVector,
      feature.NumericReductionOperator,
      feature.Models,
    )

    feature.Feature.examples(input).foreach {
      case (feature, examples) if tempUnsupported.contains(feature) =>
        throw TemporarilyUnsupported(feature.getClass.getSimpleName.stripSuffix("$"), examples.toSeq)
      case (_, _) =>
    }

    var result: Verification[_ <: Generation] = Verification(Seq(input))(FileSpanningOrigin)

    Progress.foreach(passes, (pass: RewriterBuilder) => pass.key) { pass =>
      onBeforePassKey.foreach {
        case (key, action) => if(pass.key == key) action(result)
      }

      result = pass().dispatch(result)

      result.check match {
        case Nil => // ok
        case errors => throw TransformationCheckError(errors)
      }

      onAfterPassKey.foreach {
        case (key, action) => if(pass.key == key) action(result)
      }

      result = PrettifyBlocks().dispatch(result)
    }

    for((feature, examples) <- Feature.examples(result)) {
      logger.debug(f"$feature:")
//      for(example <- examples.take(3)) {
//        logger.debug(f"$example")
//      }
    }

    result
  }
}

/**
 * Defines the rewrite chain appropriate for the Viper backends: Silicon and Carbon.
 *
 * @param adtImporter Decides how to import the definition of the built-in axiomatically-defined datatypes.
 * @param onBeforePassKey Execute a side effect just before a rewrite pass is executed.
 * @param onAfterPassKey Execute a side effect just after a rewrite pass is executed. The consistency check is done
 *                       before the side effect is performed.
 * @param simplifyBeforeRelations The list of passes to execute at the appropriate point for simplification, just before
 *                                quantified integer relations are simplified.
 * @param simplifyAfterRelations The list of passes to execute at the appropriate point for simplification, just after
 *                               quantified integer relations are simplified.
 * @param checkSat Check that non-trivial contracts are satisfiable.
 */
case class SilverTransformation
(
  adtImporter: ImportADTImporter = PathAdtImporter(Resources.getAdtPath),
  override val onBeforePassKey: Seq[(String, Verification[_ <: Generation] => Unit)] = Nil,
  override val onAfterPassKey: Seq[(String, Verification[_ <: Generation] => Unit)] = Nil,
  simplifyBeforeRelations: Seq[RewriterBuilder] = Options().simplifyPaths.map(Transformation.simplifierFor(_, Options())),
  simplifyAfterRelations: Seq[RewriterBuilder] = Options().simplifyPathsAfterRelations.map(Transformation.simplifierFor(_, Options())),
  checkSat: Boolean = true,
) extends Transformation(onBeforePassKey, onAfterPassKey, Seq(
    // Remove the java.lang.Object -> java.lang.Object inheritance loop
    NoSupportSelfLoop,

    // Delete stuff that may be declared unsupported at a later stage
    FilterSpecIgnore,

    // Normalize AST
    Disambiguate, // Resolve overloaded operators (+, subscript, etc.)
    DisambiguateLocation, // Resolve location type

    // Q (RR): Should the next two passes be more in the center of the pass list? Like the exception passes?
    EncodeJavaLangString, // Encode java strings as string objects and interning functions
    EncodeString, // Encode spec string as seq<int>

    CollectLocalDeclarations, // all decls in Scope
    DesugarPermissionOperators, // no PointsTo, \pointer, etc.
    ReadToValue, // resolve wildcard into fractional permission
    DesugarCoalescingOperators, // no .!
    PinCollectionTypes, // no anonymous sequences, sets, etc.
    QuantifySubscriptAny, // no arr[*]
    IterationContractToParBlock,
    PropagateContextEverywhere, // inline context_everywhere into loop invariants
    EncodeArrayValues, // maybe don't target shift lemmas on generated function for \values
    GivenYieldsToArgs,

    CheckProcessAlgebra,
    EncodeCurrentThread,
    EncodeIntrinsicLock,
    EncodeForkJoin,
    InlineApplicables,
    PureMethodsToFunctions,
    RefuteToInvertedAssert,

    // Encode parallel blocks
    EncodeSendRecv,
    ParBlockEncoder,

    // Encode exceptional behaviour (no more continue/break/return/try/throw)
    SpecifyImplicitLabels,
    SwitchToGoto,
    ContinueToBreak,
    EncodeBreakReturn,

    ) ++ simplifyBeforeRelations ++ Seq(
    SimplifyQuantifiedRelations,
    SimplifyNestedQuantifiers,
    ) ++ simplifyAfterRelations ++ Seq(

    // Encode proof helpers
    EncodeProofHelpers,

    // Make final fields constant functions. Explicitly before ResolveExpressionSideEffects, because that pass will
    // flatten out functions in the rhs of assignments, making it harder to detect final field assignments where the
    // value is pure and therefore be put in the contract of the constant function.
    ConstantifyFinalFields,

    // Resolve side effects including method invocations, for encodetrythrowsignals.
    ResolveExpressionSideEffects,
    EncodeTryThrowSignals,

    // No more classes
    ClassToRef,

    CheckContractSatisfiability.withArg(checkSat),

    ResolveExpressionSideChecks,

    DesugarCollectionOperators,

    // Translate internal types to domains
    FloatToRat,
    EnumToDomain,
    ImportArray.withArg(adtImporter),
    ImportPointer.withArg(adtImporter),
    ImportMapCompat.withArg(adtImporter),
    ImportEither.withArg(adtImporter),
    ImportTuple.withArg(adtImporter),
    ImportOption.withArg(adtImporter),
    ImportFrac.withArg(adtImporter),
    ImportNothing.withArg(adtImporter),
    ImportVoid.withArg(adtImporter),
    ImportNull.withArg(adtImporter),
    ImportAny.withArg(adtImporter),
    ImportViperOrder.withArg(adtImporter),

    ExtractInlineQuantifierPatterns,
    MonomorphizeContractApplicables,

    // Silver compat (basically no new nodes)
    FinalizeArguments,
    ResolveScale,
    ExplicitADTTypeArgs,
    ForLoopToWhileLoop,
    BranchToIfElse,
    EvaluationTargetDummy,
    SingletonStarall,

    // Final translation to rigid silver nodes
    SilverIntRatCoercion,
    // PB TODO: PinSilverNodes has now become a collection of Silver oddities, it should be more structured / split out.
    PinSilverNodes,
  ))