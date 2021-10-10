package vct.main

import vct.col.ast.{CheckError, DiagnosticOrigin, IncomparableTypes, OutOfScopeError, Program, TypeError, TypeErrorText}
import vct.col.newrewrite.JavaSpecificToCol
import vct.col.resolve.{ResolveReferences, ResolveTypes}
import vct.parsers.Parsers
import vct.result.VerificationResult
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
      CommandLineTesting.getCases.values.filter(_.tools.contains("silicon")).foreach(c => {
        if(c.files.asScala.forall(f =>
            f.toString.endsWith(".java") ||
              f.toString.endsWith(".c") ||
              f.toString.endsWith(".pvl"))) {
          tryParse(c.files.asScala.toSeq)
        } else {
          println(s"Skipping: ${c.files.asScala.mkString(", ")}")
        }
      })

//      tryParse(Path.of("examples/known-problems/futures/elect.pvl"))
    } finally {
      println(s"Out of $files filesets, $systemErrors threw a SystemError, $crashes crashed and $errorCount errors were reported.")
      println(s"Time: ${(System.currentTimeMillis() - start)/1000.0}s")
    }
  }

  def printErrorsOr(errors: Seq[CheckError])(otherwise: => Unit): Unit = {
    errorCount += errors.size
    if(errors.isEmpty) otherwise
    else errors.foreach {
      case TypeError(expr, expectedType) =>
        expectedType.superTypeOf(expr.t)
        println(expr.o.messageInContext(s"Expected to be of type $expectedType, but got ${expr.t}"))
      case TypeErrorText(expr, message) =>
        println(expr.o.messageInContext(message(expr.t)))
      case OutOfScopeError(ref) =>
        ref.decl.o.messageInContext("Out of scope!")
      case IncomparableTypes(left, right) =>
        println(s"Types $left and $right are incomparable")
    }
  }

  def tryParse(paths: Seq[Path]): Unit = try {
    files += 1
    println(paths.mkString(", "))
    var program = Program(paths.flatMap(Parsers.parse))(DiagnosticOrigin)
    val extraDecls = ResolveTypes.resolve(program)
    program = Program(program.declarations ++ extraDecls)(DiagnosticOrigin)
    val errors = ResolveReferences.resolve(program)
    printErrorsOr(errors) {
      program = JavaSpecificToCol().dispatch(program)
      printErrorsOr(program.check){}
    }
  } catch {
    case err: SystemError =>
      println(err.text)
      systemErrors += 1
    case res: VerificationResult =>
      println(res.text)
    case e: Throwable =>
      e.printStackTrace()
      crashes += 1
  }
}