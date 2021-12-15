package vct.main

import vct.col.ast.{Declaration, Program}
import vct.col.check.CheckError
import vct.col.debug.NotProcessed
import vct.col.feature.Feature
import vct.col.newrewrite._
import vct.col.newrewrite.lang._
import vct.col.newrewrite.exc._
import vct.col.origin.DiagnosticOrigin
import vct.col.resolve.{Java, ResolveReferences, ResolveTypes}
import vct.col.rewrite.{Generation, InitialGeneration, RewriterBuilder}
import vct.col.util.SuccessionMap
import vct.parsers.{ParseResult, Parsers}
import vct.result.VerificationResult.{SystemError, UserError}
import vct.test.CommandLineTesting
import viper.api.Silicon

import java.nio.file.{Path, Paths}
import scala.jdk.CollectionConverters._
import scala.collection.parallel.CollectionConverters._


case object Test {
  var files = 0
  var systemErrors = 0
  var errorCount = 0
  var crashes = 0

  val start = System.currentTimeMillis()

  def main(args: Array[String]): Unit = {
    try {
//      for(f <- new File("examples/arrays/array-example.pvl").listFiles()) {
//        tryParse(Seq(f.toPath))
//      }

      CommandLineTesting.getCases.values.filter(_.tools.contains("silicon")).toSeq.sortBy(_.files.asScala.toSeq.head).foreach(c => {
        if(c.files.asScala.forall(f =>
            f.toString.endsWith(".java") ||
              f.toString.endsWith(".c") ||
              f.toString.endsWith(".pvl"))) {
          tryParse(c.files.asScala.toSeq)
        } else {
          println(s"Skipping: ${c.files.asScala.mkString(", ")}")
        }
      })

//      tryParse(Seq(Path.of("examples/demo/demo3d.pvl")))
    } finally {
      println(s"Out of $files filesets, $systemErrors threw a SystemError, $crashes crashed and $errorCount errors were reported.")
      println(s"Time: ${(System.currentTimeMillis() - start)/1000.0}s")
    }
  }

  case object Exit extends RuntimeException

  def printErrors(errors: Seq[CheckError]): Unit = {
    errorCount += errors.size
    errors.foreach(println)
    if(errors.nonEmpty) throw Exit
  }

  def tryParse(paths: Seq[Path]): Unit = try {
    files += 1
    println(paths.mkString(", "))
    val ParseResult(decls, expectedErrors) = ParseResult.reduce(paths.map(Parsers.parse[InitialGeneration]))
    var parsedProgram = Program(decls, Some(Java.JAVA_LANG_OBJECT[InitialGeneration]))(DiagnosticOrigin)(DiagnosticOrigin)
    val extraDecls = ResolveTypes.resolve(parsedProgram)
    val untypedProgram = Program(parsedProgram.declarations ++ extraDecls, parsedProgram.rootClass)(DiagnosticOrigin)(DiagnosticOrigin)
    val typedProgram = LangTypesToCol().dispatch(untypedProgram)
    val errors = ResolveReferences.resolve(typedProgram)
    printErrors(errors)

    val passes: Seq[RewriterBuilder] = Seq(
      // Language-specific nodes -> COL (because of fragile references)
      LangSpecificToCol,

      // Delete stuff that may be declared unsupported at a later stage
      FilterSpecIgnore,

      // Normalize AST
      Disambiguate, // Resolve overloaded operators (+, subscript, etc.)
      CollectLocalDeclarations, // all decls in Scope
      DesugarPermissionOperators, // no PointsTo, \pointer, etc.
      PinCollectionTypes, // no anonymous sequences, sets, etc.
      QuantifySubscriptAny, // no arr[*]
      ResolveScale, // inline predicate scaling into predicate applications

      CheckProcessAlgebra,

      SwitchToGoto,
      EncodeCurrentThread,
      EncodeIntrinsicLock,
      InlineApplicables,
      PureMethodsToFunctions,

      // Encode parallel blocks
      IterationContractToParBlock,
      ParBlockEncoder,

      // Encode exceptional behaviour (no more continue/break/return/try/throw)
      ContinueToBreak,
      SpecifyImplicitLabels,
      EncodeBreakReturn,
      // Resolve side effects including method invocations, for encodetrythrowsignals.
      ResolveExpressionSideEffects,
      EncodeTryThrowSignals,

      // No more classes
      ConstantifyFinalFields,
      ClassToRef,

      // Simplify pure expressions (no more new complex expressions)
      ApplyTermRewriter.BuilderFor[InitialGeneration](Nil),
      SimplifyQuantifiedRelations,

      EncodeArrayValues, // maybe don't target shift lemmas on generated function for \values

      // Translate internal types to domains
      ImportADT,

      // Silver compat (basically no new nodes)
      ForLoopToWhileLoop,
      BranchToIfElse,
      DesugarCollectionOperators,
      EvaluationTargetDummy,

      // Final translation to rigid silver nodes
      PinSilverNodes,
    )

    SuccessionMap.breakOnMissingPredecessor {
      var program: Program[_ <: Generation] = typedProgram
      for(pass <- passes) {
        println(s"    ${pass.getClass.getSimpleName}")
        val oldProgram = program
        program = pass().dispatch(program)
        oldProgram.declarations.par.foreach(_.transSubnodes.foreach {
          case decl: Declaration[_] =>
            if(decl.debugRewriteState == NotProcessed) {
              println(s"Dropped without notice: $decl")
              throw Exit
            }
          case _ =>
        })
        assert(program.declarations.nonEmpty)
        printErrors(program.check)
      }
      println(Feature.scan(program))
      Silicon(Map.empty, Paths.get("/home/pieter/vercors/src/main/universal/res/deps/z3/4.8.6/Linux/x86_64/bin/z3")).submit(program)
    }
  } catch {
    case Exit =>
    case err: SystemError =>
      println(err.text)
      systemErrors += 1
    case res: UserError =>
      errorCount += 1
      println(res.text)
    case e: Throwable =>
      e.printStackTrace()
      crashes += 1
  }
}