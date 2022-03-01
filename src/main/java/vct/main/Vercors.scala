package vct.main

import vct.col.ast.{GlobalDeclaration, Program}
import vct.col.check.CheckError
import vct.col.newrewrite._
import vct.col.newrewrite.exc._
import vct.col.newrewrite.lang._
import vct.col.origin.Origin
import vct.col.resolve.{Java, ResolutionError, ResolveReferences, ResolveTypes}
import vct.col.rewrite.{Generation, InitialGeneration, RewriterBuilder}
import vct.java.JavaLibraryLoader
import vct.main.Vercors.FileSpanningOrigin
import vct.options.Options
import vct.parsers.{ParseResult, Parsers}
import viper.api.Silicon

import java.nio.file.{Path, Paths}
import scala.collection.immutable.{AbstractSeq, LinearSeq}
import scala.xml.NodeSeq

case object Vercors {
  case object FileSpanningOrigin extends Origin {
    override def preferredName: String = "unknown"
    override def context: String = "[At node that spans multiple files]"
  }

  val passes: Seq[RewriterBuilder] = Seq(
    // Language-specific nodes -> COL (because of fragile references)
    LangSpecificToCol,
    // Remove the java.lang.Object -> java.lang.Object inheritance loop
    NoSupportSelfLoop,

    // Delete stuff that may be declared unsupported at a later stage
    FilterSpecIgnore,

    // Normalize AST
    Disambiguate, // Resolve overloaded operators (+, subscript, etc.)
    CollectLocalDeclarations, // all decls in Scope
    DesugarPermissionOperators, // no PointsTo, \pointer, etc.
    PinCollectionTypes, // no anonymous sequences, sets, etc.
    QuantifySubscriptAny, // no arr[*]
    IterationContractToParBlock,
    PropagateContextEverywhere, // inline context_everywhere into loop invariants
    EncodeArrayValues, // maybe don't target shift lemmas on generated function for \values
    GivenYieldsToArgs,

    CheckProcessAlgebra,

    EncodeCurrentThread,
    EncodeIntrinsicLock,
    InlineApplicables,
    PureMethodsToFunctions,

    // Encode parallel blocks
    EncodeSendRecv,
    EncodeParAtomic,
    ParBlockEncoder,

    // Encode exceptional behaviour (no more continue/break/return/try/throw)
    SpecifyImplicitLabels,
    SwitchToGoto,
    ContinueToBreak,
    EncodeBreakReturn,
    // Resolve side effects including method invocations, for encodetrythrowsignals.
    ResolveExpressionSideEffects,
    EncodeTryThrowSignals,

    // No more classes
    ConstantifyFinalFields,
    ClassToRef,

    // Simplify pure expressions (no more new complex expressions)
    ApplyTermRewriter.BuilderForFile(Paths.get("src/main/universal/res/config/pushin.pvl")),
    ApplyTermRewriter.BuilderForFile(Paths.get("src/main/universal/res/config/simplify.pvl")),
    SimplifyQuantifiedRelations,
    ApplyTermRewriter.BuilderForFile(Paths.get("src/main/universal/res/config/simplify.pvl")),

    // Translate internal types to domains
    ImportADT,

    ExtractInlineQuantifierPatterns,
    MonomorphizeContractApplicables,

    // Silver compat (basically no new nodes)
    ResolveScale,
    ExplicitADTTypeArgs,
    ForLoopToWhileLoop,
    BranchToIfElse,
    DesugarCollectionOperators,
    EvaluationTargetDummy,

    // Final translation to rigid silver nodes
    SilverIntRatCoercion,
    // PB TODO: PinSilverNodes has now become a collection of Silver oddities, it should be more structured / split out.
    PinSilverNodes,
  )
}

case class Vercors(options: Options) {
  def parse[G](paths: Seq[Path]): ParseResult[G] =
    ParseResult.reduce(paths.map(Parsers.parse))

  def resolve[G <: Generation](parse: Seq[GlobalDeclaration[G]]): Either[Seq[CheckError], Program[_ <: Generation]] = {
    implicit val o: Origin = FileSpanningOrigin

    val parsedProgram = Program(parse, Some(Java.JAVA_LANG_OBJECT[G]))(FileSpanningOrigin)
    val extraDecls = ResolveTypes.resolve(parsedProgram, Some(JavaLibraryLoader))
    val joinedProgram = Program(parsedProgram.declarations ++ extraDecls, parsedProgram.rootClass)(FileSpanningOrigin)
    val typedProgram = LangTypesToCol().dispatch(joinedProgram)
    ResolveReferences.resolve(typedProgram) match {
      case Nil =>
        val resolvedProgram = LangSpecificToCol().dispatch(typedProgram)
        resolvedProgram.check match {
          case Nil => Right(resolvedProgram)
          case some => Left(some)
        }
      case some => Left(some)
    }
  }

  def go(): Unit = {
    val ParseResult(decls, expectedErrors) = parse(options.inputs)

    var program = resolve(decls).getOrElse(???)

    for(pass <- Vercors.passes) {
      program = pass().dispatch(program)
      program = PrettifyBlocks().dispatch(program)
    }

    Silicon(Map.empty, options.z3Path).submit(program)

    expectedErrors.foreach(_.signalDone())
  }
}
