package vct.main

import vct.col.ast.{CheckError, DiagnosticOrigin, IncomparableTypes, OutOfScopeError, Program, TypeError, TypeErrorText}
import vct.col.newrewrite.JavaSpecificToCol
import vct.col.resolve.{ResolveReferences, ResolveTypes}
import vct.parsers.Parsers
import vct.result.VerificationResult
import vct.test.CommandLineTesting

import java.nio.file.Path
import scala.jdk.CollectionConverters._

case object Test {
  def main(args: Array[String]): Unit = {
    CommandLineTesting.getCases.values.filter(_.tools.contains("silicon")).foreach(c => {
      c.files.asScala.filter(f => f.toString.endsWith(".java") || f.toString.endsWith(".c")).foreach(tryParse)
    })

//    tryParse(Path.of("examples/known-problems/threads/SpecifiedThread.java"))
  }

  def printErrorsOr(errors: Seq[CheckError])(otherwise: => Unit): Unit = {
    if(errors.isEmpty) otherwise
    else errors.foreach {
      case TypeError(expr, expectedType) =>
        println(expr.o.messageInContext(s"Expected to be of type $expectedType, but got ${expr.t}"))
      case TypeErrorText(expr, message) =>
        println(expr.o.messageInContext(message(expr.t)))
      case OutOfScopeError(ref) =>
        println("Out of scope")
      case IncomparableTypes(left, right) =>
        println(s"Types $left and $right are incomparable")
    }
  }

  def tryParse(path: Path): Unit = try {
    println(path)
    var program = Program(Parsers.parse(path))(DiagnosticOrigin)
    val extraDecls = ResolveTypes.resolve(program)
    program = Program(program.declarations ++ extraDecls)(DiagnosticOrigin)
    val errors = ResolveReferences.resolve(program)
    printErrorsOr(errors) {
      program = JavaSpecificToCol().dispatch(program)
      printErrorsOr(program.check){}
    }
  } catch {
    case res: VerificationResult =>
      println(res.text)
//      throw res
  }
}