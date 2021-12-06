package vct.main

import vct.col.ast.{Declaration, Program}
import vct.col.check.CheckError
import vct.col.debug.NotProcessed
import vct.col.newrewrite._
import vct.col.newrewrite.lang._
import vct.col.newrewrite.exc._
import vct.col.origin.DiagnosticOrigin
import vct.col.resolve.{ResolveReferences, ResolveTypes}
import vct.col.util.SuccessionMap
import vct.parsers.{ParseResult, Parsers}
import vct.result.VerificationResult.{SystemError, UserError}
import vct.test.CommandLineTesting

import java.nio.file.Path
import scala.jdk.CollectionConverters._

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
    val ParseResult(decls, expectedErrors) = ParseResult.reduce(paths.map(Parsers.parse))
    var input = Program(decls)(DiagnosticOrigin)(DiagnosticOrigin)
    val extraDecls = ResolveTypes.resolve(input)
    input = Program(input.declarations ++ extraDecls)(DiagnosticOrigin)(DiagnosticOrigin)
    val typesToCol = LangTypesToCol()
    input = typesToCol.dispatch(input)
    val errors = ResolveReferences.resolve(input)
    printErrors(errors)

    val passes = Seq(
      // Language-specific nodes -> COL (because of fragile references)
      LangSpecificToCol(),

      // Delete stuff that may be declared unsupported at a later stage
      FilterSpecIgnore(),

      // Normalize AST
      Disambiguate(), // Resolve overloaded operators (+, subscript, etc.)
      CollectLocalDeclarations(), // all decls in Scope
      DesugarPermissionOperators(), // no PointsTo, \pointer, etc.
      PinCollectionTypes(), // no anonymous sequences, sets, etc.
      QuantifySubscriptAny(), // no arr[*]

      CheckProcessAlgebra(),

      SwitchToGoto(),
      EncodeCurrentThread(),
      EncodeIntrinsicLock(),
      InlineApplicables(),
      PureMethodsToFunctions(),

      // Encode parallel blocks
      IterationContractToParBlock(),
      ParBlockEncoder(),

      // Encode exceptional behaviour (no more continue/break/return/try/throw)
      ContinueToBreak(),
      EncodeBreakReturn(),
      EncodeTryThrowSignals(),

      // No more classes
      ConstantifyFinalFields(),
      ClassToRef(),

      // Simplify pure expressions (no more new complex expressions)
      ApplyTermRewriter(Nil),
      SimplifyQuantifiedRelations(),

      EncodeArrayValues(), // maybe don't target shift lemmas on generated function for \values

      // Translate internal types to domains
      ImportADT(),

      // Silver compat (basically no new nodes)
      ForLoopToWhileLoop(),
      BranchToIfElse(),
      DesugarCollectionOperators(),
      ResolveExpressionSideEffects(),
      EvaluationTargetDummy(),
    )

    SuccessionMap.breakOnMissingPredecessor {
      var program = input
      for(pass <- passes) {
        println(s"    ${pass.getClass.getSimpleName}")
        val oldProgram = program
        program = pass.dispatch(program)
        oldProgram.transSubnodes.foreach {
          case decl: Declaration =>
            if(decl.debugRewriteState == NotProcessed) {
              println(s"Dropped without notice: $decl")
              throw Exit
            }
          case _ =>
        }
        assert(program.declarations.nonEmpty)
        printErrors(program.check)
      }
      println(program)
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