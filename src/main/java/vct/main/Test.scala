package vct.main

import com.sun.management.HotSpotDiagnosticMXBean
import vct.col.ast.{Declaration, Program, SimplificationRule}
import vct.col.check.CheckError
import vct.col.debug.NotProcessed
import vct.col.feature.{Feature, TypeValuesAndGenerics, WildcardReadPermission}
import vct.col.newrewrite._
import vct.col.newrewrite.exc._
import vct.col.newrewrite.lang._
import vct.col.origin.DiagnosticOrigin
import vct.col.resolve.{Java, ResolveReferences, ResolveTypes}
import vct.col.rewrite.{Generation, InitialGeneration, RewriterBuilder, Rewritten}
import vct.col.util.SuccessionMap
import vct.java.JavaLibraryLoader
import vct.parsers.{ParseResult, Parsers}
import vct.result.VerificationResult.{SystemError, UserError}
import vct.test.CommandLineTesting
import viper.api.Silicon
import viper.silver.ast.WildcardPerm

import java.io.File
import java.lang.management.ManagementFactory
import scala.jdk.CollectionConverters._
import java.nio.file.{Path, Paths}
import javax.management.MBeanServer
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ForkJoinTasks


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

      if (args.nonEmpty) {
        tryParse(args.map(Paths.get(_)))
        return
      }

      var dumpCount = 0

      CommandLineTesting.getCases.values.filter(_.tools.contains("silicon")).toSeq.sortBy(_.files.asScala.toSeq.head).foreach(c => {
        if(c.files.asScala.forall(f =>
            f.toString.endsWith(".java") ||
              f.toString.endsWith(".c") ||
              f.toString.endsWith(".pvl"))) {
          tryParse(c.files.asScala.toSeq)
          /*
          System.gc()
          val server = ManagementFactory.getPlatformMBeanServer
          val mxBean = ManagementFactory.newPlatformMXBeanProxy(server, "com.sun.management:type=HotSpotDiagnostic", classOf[HotSpotDiagnosticMXBean])
          mxBean.dumpHeap(s"/home/pieter/vercors/tmp/heapdump-$dumpCount.hprof", true)
          dumpCount += 1
           */
        } else {
          println(s"Skipping: ${c.files.asScala.mkString(", ")}")
        }
      })

//      tryParse(Seq(Path.of("examples/arrays/array-example.pvl")))
    } finally {
      println(s"Out of $files filesets, $systemErrors threw a SystemError, $crashes crashed and $errorCount errors were reported.")
      println(s"Time: ${(System.currentTimeMillis() - start)/1000.0}s")
      ForkJoinTasks.defaultForkJoinPool.shutdown()
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
    val parsedProgram = Program(decls, Some(Java.JAVA_LANG_OBJECT[InitialGeneration]))(DiagnosticOrigin)(DiagnosticOrigin)
    val extraDecls = ResolveTypes.resolve(parsedProgram, Some(JavaLibraryLoader))
    val untypedProgram = Program(parsedProgram.declarations ++ extraDecls, parsedProgram.rootClass)(DiagnosticOrigin)(DiagnosticOrigin)
    val typedProgram = LangTypesToCol().dispatch(untypedProgram)
    val errors = ResolveReferences.resolve(typedProgram)
    printErrors(errors)

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
      ResolveScale, // inline predicate scaling into predicate applications
      PropagateContextEverywhere, // inline context_everywhere into loop invariants

      CheckProcessAlgebra,

      EncodeCurrentThread,
      EncodeIntrinsicLock,
      InlineApplicables,
      PureMethodsToFunctions,

      // Encode parallel blocks
      IterationContractToParBlock,
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

      EncodeArrayValues, // maybe don't target shift lemmas on generated function for \values

      // Translate internal types to domains
      ImportADT,

      ExtractInlineQuantifierPatterns,
      MonomorphizeContractApplicables,

      // Silver compat (basically no new nodes)
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

    SuccessionMap.breakOnMissingPredecessor {
      var program: Program[_ <: Generation] = typedProgram
      for(pass <- passes) {
//        println(s"    ${pass.getClass.getSimpleName}")
        val oldProgram = program
        program = pass().dispatch(program)
        oldProgram.declarations.par.foreach(_.transSubnodes.foreach {
          case decl: Declaration[_] =>
            if(decl.debugRewriteState == NotProcessed && !pass.isInstanceOf[ApplyTermRewriter.BuilderForFile]) {
              println(s"Dropped without notice: $decl")
              throw Exit
            }
          case _ =>
        })
        assert(program.declarations.nonEmpty)
        printErrors(program.check)
        program = PrettifyBlocks().dispatch(program)
      }
      for((feature, examples) <- Feature.examples(program).filter { case (feature, _) => !Set[Feature](TypeValuesAndGenerics, WildcardReadPermission).contains(feature) }) {
        println(f"$feature:")
        for(example <- examples.take(5)) {
          println(f"  $example")
        }
      }
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