package vct.main

import com.typesafe.scalalogging.LazyLogging
import hre.progress.Progress
import vct.col.ast.{CGlobalDeclaration, GlobalDeclaration, Node, Procedure, Program}
import vct.col.check.CheckError
import vct.col.feature.{Feature, FeatureRainbow}
import vct.col.newrewrite._
import vct.col.newrewrite.exc._
import vct.col.newrewrite.lang._
import vct.col.origin.Origin
import vct.col.resolve.{C, Java, RefCGlobalDeclaration, ResolutionError, ResolveReferences, ResolveTypes}
import vct.col.rewrite.{Generation, InitialGeneration, RewriterBuilder}
import vct.java.JavaLibraryLoader
import vct.main.Vercors.{FileSpanningOrigin, InputResolutionError, TemporarilyUnsupported}
import vct.options.{Options, PathOrStd}
import vct.parsers.{ParseResult, Parsers}
import vct.result.VerificationResult
import vct.result.VerificationResult.{Ok, UserError}
import viper.api.Silicon
import vct.col.feature

import java.nio.file.{Path, Paths}
import scala.collection.immutable.{AbstractSeq, LinearSeq}
import scala.xml.NodeSeq

case object Vercors {
  case object FileSpanningOrigin extends Origin {
    override def preferredName: String = "unknown"
    override def context: String = "[At node that spans multiple files]"
  }

  case class InputResolutionError(errors: Seq[CheckError]) extends UserError {
    override def code: String = "resolutionError"
    override def text: String = errors.map(_.toString).mkString("\n")
  }

  case class TemporarilyUnsupported(feature: String, examples: Seq[Node[_]]) extends UserError {
    override def code: String = "unsupported"
    override def text: String =
      examples.head.o.messageInContext(
        s"The feature `$feature` is temporarily unsupported.")
  }
}

case class Vercors(options: Options) extends ImportADTImporter with LazyLogging {
  def parse[G](paths: Path*): ParseResult[G] =
    ParseResult.reduce(Progress.map(paths, (p: Path) => p.toString)(Parsers.parse[G](_, this)).toSeq)

  def resolve[G <: Generation](parse: Seq[GlobalDeclaration[G]], withJava: Boolean): Either[Seq[CheckError], Program[_ <: Generation]] = {
    implicit val o: Origin = FileSpanningOrigin

    val parsedProgram = Program(parse, if(withJava) Some(Java.JAVA_LANG_OBJECT[G]) else None)(FileSpanningOrigin)
    val extraDecls = ResolveTypes.resolve(parsedProgram, if(withJava) Some(JavaLibraryLoader(options.jrePath, this)) else None)
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

  def passes: Seq[RewriterBuilder] = Seq(
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
  ) ++ options.simplifyPaths.map { case PathOrStd.Path(p) => ApplyTermRewriter.BuilderForFile(p, this) } ++ Seq(
    SimplifyQuantifiedRelations,
  ) ++ options.simplifyPathsAfterRelations.map { case PathOrStd.Path(p) => ApplyTermRewriter.BuilderForFile(p, this) } ++ Seq(
    // Translate internal types to domains
    ImportADT.withArg(this),

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

  def go(): VerificationResult = try {
    Progress.stages(Seq(
      ("Parsing", 4),
      ("Translation", 3),
      ("Verification", 10),
    )) {
      val ParseResult(decls, expectedErrors) = parse(options.inputs : _*)
      Progress.nextPhase()

      decls.foreach(_.transSubnodes.foreach {
        case decl: CGlobalDeclaration[_] => decl.decl.inits.foreach(init => {
          if(C.getDeclaratorInfo(init.decl).params.isEmpty) {
            throw TemporarilyUnsupported("GlobalCVariable", Seq(decl))
          }
        })
        case _ =>
      })

      var program = resolve(decls, withJava = true) match {
        case Left(errors) => throw InputResolutionError(errors)
        case Right(program) => program
      }

      val tempUnsupported = Set[feature.Feature](
        feature.JavaThreads,
        feature.MatrixVector,
        feature.NumericReductionOperator,
        feature.MagicWand,
        feature.Models,
      )

      feature.Feature.examples(program).foreach {
        case (feature, examples) if tempUnsupported.contains(feature) =>
          throw TemporarilyUnsupported(feature.getClass.getSimpleName.stripSuffix("$"), examples.toSeq)
        case (_, _) =>
      }

      Progress.foreach(passes, (pass: RewriterBuilder) => pass.key)(pass => {
        options.outputBeforePass.get(pass.key) match {
          case None =>
          case Some(PathOrStd.StdInOrOut) =>
            println(program)
          case Some(PathOrStd.Path(path)) => ???
        }

        program = pass().dispatch(program)

        options.outputAfterPass.get(pass.key) match {
          case None =>
          case Some(PathOrStd.StdInOrOut) =>
            println(program)
          case Some(PathOrStd.Path(path)) => ???
        }

        program = PrettifyBlocks().dispatch(program)
      })

      Progress.nextPhase()
      Silicon(Map.empty, options.z3Path).submit(program)

      expectedErrors.foreach(_.signalDone())

      Ok
    }
  } catch {
    case res: VerificationResult => res
  }

  def helpPasses(): Unit = {
    println("Available passes:")
    for(pass <- passes) {
      println(s"- ${pass.key}")
      println(s"    ${pass.desc}")
    }
  }

  override def loadAdt[G](adtName: String): Either[Seq[CheckError], Program[G]] =
    resolve(parse(options.adtPath.resolve(adtName + ".pvl")).decls, withJava = false).map(program =>
      Disambiguate().dispatch(program).asInstanceOf[Program[G]])
}
