package vct.main.stages

import com.typesafe.scalalogging.LazyLogging
import hre.debug.TimeTravel
import hre.debug.TimeTravel.CauseWithBadEffect
import hre.progress.Progress
import hre.stages.Stage
import vct.col.ast.{Program, SimplificationRule, Verification}
import vct.col.check.CheckError
import vct.col.feature
import vct.col.feature.Feature
import vct.col.print.Ctx
import vct.col.rewrite._
import vct.col.rewrite.adt._
import vct.col.rewrite.bip._
import vct.col.rewrite.exc._
import vct.rewrite.lang.NoSupportSelfLoop
import vct.col.rewrite.veymont.StructureCheck
import vct.importer.{PathAdtImporter, Util}
import vct.main.Main.TemporarilyUnsupported
import vct.main.stages.Transformation.{
  PassEventHandler,
  TransformationCheckError,
}
import vct.options.Options
import vct.options.types.{Backend, PathOrStd}
import vct.resources.Resources
import vct.result.VerificationError.SystemError
import vct.rewrite.adt.ImportSetCompat
import vct.rewrite.{
  EncodeAutoValue,
  EncodeRange,
  EncodeResourceValues,
  ExplicitResourceValues,
  HeapVariableToRef,
  MonomorphizeClass,
  SmtlibToProverTypes,
}
import vct.rewrite.lang.ReplaceSYCLTypes
import vct.rewrite.veymont.{
  DeduplicateChorGuards,
  DropChorExpr,
  EncodeChannels,
  EncodeChorBranchUnanimity,
  EncodeChoreography,
  EncodeEndpointInequalities,
  StratifyUnpointedExpressions,
  GenerateChoreographyPermissions,
  GenerateImplementation,
  InferEndpointContexts,
  SpecializeEndpointClasses,
  StratifyExpressions,
}
import vct.rewrite.csimplifier.MakeRuntimeChecks

import java.nio.file.Path
import java.nio.file.Files
import java.nio.file.Paths

object Transformation extends LazyLogging {
  case class TransformationCheckError(
      pass: RewriterBuilder,
      errors: Seq[(Program[_], CheckError)],
  ) extends SystemError {
    override def text: String =
      s"The ${pass.key} rewrite caused the AST to no longer typecheck:\n" +
        errors.map { case (program, err) => err.message(program.highlight) }
          .mkString("\n")
  }

  private def writeOutFunctions(
      event: TransformationEvent,
      m: Map[String, PathOrStd],
  ): Seq[PassEventHandler] =
    m.toSeq.map { case (key, out) =>
      (
          passes,
          currentEvent,
          currentKey,
          program: Verification[_ <: Generation],
      ) => {
        if (key == currentKey && event == currentEvent) {
          out.write { writer => program.write(writer)(Ctx().namesIn(program)) }
        }
      }
    }

  private def reportIntermediateProgram(
      out: Path,
      stageKey: String,
  ): PassEventHandler = {
    Files.createDirectories(out)
    Files.list(out).filter(_.endsWith(".col")).forEach(Files.delete(_))
    (passes, event, pass, program) => {
      val i =
        passes.map(_.key).indexOf(pass) * 2 +
          (if (event == before)
             0
           else
             1)
      val target = PathOrStd
        .Path(out.resolve(f"$stageKey-$i%03d-$event-$pass.col"))
      target.write { writer => program.write(writer)(Ctx().namesIn(program)) }
    }
  }

  def simplifierFor(path: PathOrStd, options: Options): RewriterBuilder =
    ApplyTermRewriter.BuilderFor(
      ruleNodes = Util.loadPVLLibraryFile[InitialGeneration](
        path,
        options.getParserDebugOptions,
      ).declarations.collect {
        case rule: SimplificationRule[InitialGeneration] => rule
      },
      debugIn = options.devSimplifyDebugIn,
      debugMatch = options.devSimplifyDebugMatch,
      debugNoMatch = options.devSimplifyDebugNoMatch,
      debugMatchShort = options.devSimplifyDebugMatchShort,
      debugFilterInputKind = options.devSimplifyDebugFilterInputKind,
      debugFilterRule = options.devSimplifyDebugFilterRule,
    )

  def ofOptions(
      options: Options,
      bipResults: BIP.VerificationResults = BIP.VerificationResults(),
  ): Transformation =
    options.backend match {
      case Backend.Silicon | Backend.Carbon =>
        SilverTransformation(
          adtImporter = PathAdtImporter(
            options.adtPath,
            options.getParserDebugOptions,
          ),
          onPassEvent =
            options.outputIntermediatePrograms
              .map(p => reportIntermediateProgram(p, "verify")).toSeq ++
              writeOutFunctions(
                Transformation.before,
                options.outputBeforePass,
              ) ++
              writeOutFunctions(Transformation.after, options.outputAfterPass),
          simplifyBeforeRelations = options.simplifyPaths
            .map(simplifierFor(_, options)),
          simplifyAfterRelations = options.simplifyPathsAfterRelations
            .map(simplifierFor(_, options)),
          checkSat = options.devCheckSat,
          inferHeapContextIntoFrame = options.inferHeapContextIntoFrame,
          bipResults = bipResults,
          splitVerificationByProcedure =
            options.devSplitVerificationByProcedure,
          veymontGeneratePermissions = options.veymontGeneratePermissions,
        )
    }

  def veymontImplementationGenerationOfOptions(
      options: Options
  ): Transformation =
    VeyMontImplementationGeneration(
      importer = PathAdtImporter(
        options.veymontResourcePath,
        options.getParserDebugOptions,
      ),
      onPassEvent =
        options.outputIntermediatePrograms
          .map(p => reportIntermediateProgram(p, "generate")).toSeq ++
          writeOutFunctions(before, options.outputBeforePass) ++
          writeOutFunctions(after, options.outputAfterPass),
    )

  def cSimplifierOfOptions(options: Options): Transformation =
    CSimplifier(onPassEvent =
      writeOutFunctions(before, options.outputBeforePass) ++
        writeOutFunctions(after, options.outputAfterPass)
    )

  sealed trait TransformationEvent
  case object before extends TransformationEvent
  case object after extends TransformationEvent
  type PassEventHandler =
    (
        Seq[RewriterBuilder],
        TransformationEvent,
        String,
        Verification[_ <: Generation],
    ) => Unit
}

/** Executes a sequence of rewriters. Currently the only concrete implementation
  * is [[SilverTransformation]].
  *
  * Refer to [[RewriterBuilder]] and [[RewriterBuilderArg]] for information on
  * how to use a [[Rewriter]] in the pass chain.
  *
  * @param onPassEvent
  *   Execute a handler before/after a pass is executed.
  * @param passes
  *   The list of rewrite passes to execute.
  */
class Transformation(
    val onPassEvent: Seq[PassEventHandler],
    val passes: Seq[RewriterBuilder],
) extends Stage[Verification[_ <: Generation], Verification[_ <: Generation]]
    with LazyLogging {
  override def friendlyName: String = "Transformation"
  override def progressWeight: Int = 10

  override def run(
      input: Verification[_ <: Generation]
  ): Verification[_ <: Generation] = {
    val tempUnsupported = Set[feature.Feature](
      feature.MatrixVector,
      feature.NumericReductionOperator,
      feature.Models,
    )

    feature.Feature.examples(input).foreach {
      case (feature, examples) if tempUnsupported.contains(feature) =>
        throw TemporarilyUnsupported(
          feature.getClass.getSimpleName.stripSuffix("$"),
          examples.toSeq,
        )
      case (_, _) =>
    }

    TimeTravel.safelyRepeatable {
      var result: Verification[_ <: Generation] = input

      Progress.foreach(passes, (pass: RewriterBuilder) => pass.key) { pass =>
        onPassEvent.foreach { action =>
          action(passes, Transformation.before, pass.key, result)
        }

        result =
          try { pass().dispatch(result) }
          catch {
            case c @ CauseWithBadEffect(effect) =>
              logger.error(s"An error occurred in pass ${pass.key}")
              throw c
          }

        onPassEvent.foreach { action =>
          action(passes, Transformation.after, pass.key, result)
        }

        result.tasks.map(_.program)
          .flatMap(program => program.check.map(program -> _)) match {
          case Nil => // ok
          case errors => throw TransformationCheckError(pass, errors)
        }

        result = PrettifyBlocks().dispatch(result)
      }

      for ((feature, examples) <- Feature.examples(result)) {
        logger.debug(f"$feature:")
        for (example <- examples.take(3)) {
          logger.debug(f"${example.toString.takeWhile(_ != '\n')}")
          logger.debug(f"  ${example.getClass.getSimpleName}")
        }
      }

      result
    }
  }
}

/** Defines the rewrite chain appropriate for the Viper backends: Silicon and
  * Carbon.
  *
  * @param adtImporter
  *   Decides how to import the definition of the built-in axiomatically-defined
  *   datatypes.
  * @param onBeforePassKey
  *   Execute a side effect just before a rewrite pass is executed.
  * @param onAfterPassKey
  *   Execute a side effect just after a rewrite pass is executed. The
  *   consistency check is done before the side effect is performed.
  * @param simplifyBeforeRelations
  *   The list of passes to execute at the appropriate point for simplification,
  *   just before quantified integer relations are simplified.
  * @param simplifyAfterRelations
  *   The list of passes to execute at the appropriate point for simplification,
  *   just after quantified integer relations are simplified.
  * @param checkSat
  *   Check that non-trivial contracts are satisfiable.
  */
case class SilverTransformation(
    adtImporter: ImportADTImporter = PathAdtImporter(
      Resources.getAdtPath,
      vct.parsers.debug.DebugOptions.NONE,
    ),
    override val onPassEvent: Seq[PassEventHandler] = Nil,
    simplifyBeforeRelations: Seq[RewriterBuilder] = Options().simplifyPaths
      .map(Transformation.simplifierFor(_, Options())),
    simplifyAfterRelations: Seq[RewriterBuilder] = Options()
      .simplifyPathsAfterRelations
      .map(Transformation.simplifierFor(_, Options())),
    inferHeapContextIntoFrame: Boolean = true,
    bipResults: BIP.VerificationResults,
    checkSat: Boolean = true,
    splitVerificationByProcedure: Boolean = false,
    veymontGeneratePermissions: Boolean = false,
) extends Transformation(
      onPassEvent,
      Seq(
        // Replace leftover SYCL types
        ReplaceSYCLTypes,
        CFloatIntCoercion,
        ComputeBipGlue,
        InstantiateBipSynchronizations,
        EncodeBipPermissions,
        EncodeBip.withArg(bipResults),

        // Remove the java.lang.Object -> java.lang.Object inheritance loop
        NoSupportSelfLoop,

        // Delete stuff that may be declared unsupported at a later stage
        FilterSpecIgnore,

        // Normalize AST
        // Make sure Disambiguate comes after CFloatIntCoercion, so CInts are gone
        Disambiguate, // Resolve overloaded operators (+, subscript, etc.)
        DisambiguateLocation, // Resolve location type
        EncodeRangedFor,

        // VeyMont sequential program encoding
        StratifyExpressions,
        StratifyUnpointedExpressions,
        DeduplicateChorGuards,
        InferEndpointContexts,
        GenerateChoreographyPermissions.withArg(veymontGeneratePermissions),
        EncodeChorBranchUnanimity,
        EncodeEndpointInequalities,
        EncodeChoreography,
        EncodeString, // Encode spec string as seq<int>
        EncodeChar,
        CollectLocalDeclarations, // all decls in Scope
        DesugarPermissionOperators, // no PointsTo, \pointer, etc.
        ReadToValue, // resolve wildcard into fractional permission
        TrivialAddrOf,
        DesugarCoalescingOperators, // no ?.
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
        ExplicitResourceValues,
        EncodeResourceValues,

        // Encode parallel blocks
        EncodeSendRecv,
        ParBlockEncoder,

        // Extract explicitly extracted code sections, which ban continue/break/return/goto outside them.
        SpecifyImplicitLabels,
        EncodeExtract,

        // Encode exceptional behaviour (no more continue/break/return/try/throw)
        SwitchToGoto,
        ContinueToBreak,
        EncodeBreakReturn,
      ) ++ simplifyBeforeRelations ++ Seq(
        SimplifyQuantifiedRelations,
        SimplifyNestedQuantifiers,
        TupledQuantifiers,
      ) ++ simplifyAfterRelations ++ Seq(
        UntupledQuantifiers,

        // Encode proof helpers
        EncodeProofHelpers.withArg(inferHeapContextIntoFrame),
        ImportSetCompat.withArg(adtImporter),

        // Make final fields constant functions. Explicitly before ResolveExpressionSideEffects, because that pass will
        // flatten out functions in the rhs of assignments, making it harder to detect final field assignments where the
        // value is pure and therefore be put in the contract of the constant function.
        ConstantifyFinalFields,

        // Resolve side effects including method invocations, for encodetrythrowsignals.
        ResolveExpressionSideChecks,
        ResolveExpressionSideEffects,
        EncodeTryThrowSignals,
        ResolveScale,
        MonomorphizeClass,
        // No more classes
        ClassToRef,
        HeapVariableToRef,
        CheckContractSatisfiability.withArg(checkSat),
        DesugarCollectionOperators,
        EncodeNdIndex,
        ExtractInlineQuantifierPatterns,
        // Translate internal types to domains
        FloatToRat,
        SmtlibToProverTypes,
        EnumToDomain,
        ImportArray.withArg(adtImporter),
        ImportPointer.withArg(adtImporter),
        ImportVector.withArg(adtImporter),
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
        EncodeRange.withArg(adtImporter),

        // After Disambiguate and  ImportVector
        TruncDivMod,

        // All locations with a value should now be SilverField
        EncodeForPermWithValue,
        EncodeAutoValue,
        ExtractInlineQuantifierPatterns,
        RewriteTriggerADTFunctions,
        MonomorphizeContractApplicables,

        // Silver compat (basically no new nodes)
        FinalizeArguments,
        ExplicitADTTypeArgs,
        ForLoopToWhileLoop,
        BranchToIfElse,
        EvaluationTargetDummy,

        // Final translation to rigid silver nodes
        SilverIntRatCoercion,
        // PB TODO: PinSilverNodes has now become a collection of Silver oddities, it should be more structured / split out.
        PinSilverNodes,
        Explode.withArg(splitVerificationByProcedure),
      ),
    )

case class VeyMontImplementationGeneration(
    importer: ImportADTImporter = PathAdtImporter(
      Resources.getVeymontPath,
      vct.parsers.debug.DebugOptions.NONE,
    ),
    override val onPassEvent: Seq[PassEventHandler] = Nil,
) extends Transformation(
      onPassEvent,
      Seq(
        DropChorExpr,
        StratifyExpressions,
        StratifyUnpointedExpressions,
        DeduplicateChorGuards,
        SpecializeEndpointClasses,
        EncodeChannels.withArg(importer),
        InferEndpointContexts,
        GenerateImplementation,
        PrettifyBlocks,
      ),
    )

case class CSimplifier(override val onPassEvent: Seq[PassEventHandler] = Nil)
    extends Transformation(onPassEvent, Seq(MakeRuntimeChecks))
