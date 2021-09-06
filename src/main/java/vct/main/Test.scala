package vct.main

import vct.col.ast._
import vct.parsers.Parsers
import vct.result.VerificationResult

import java.io.File

case object Test {
  def main(args: Array[String]): Unit = {
    try {
      val program = Program(Parsers.parse(new File("examples/basic/For.java").toPath))(DiagnosticOrigin)
      ResolveTypes.resolve(program, ctx = Nil, ns = None)
      val errors = ResolveReferences.resolveAndCheck(program, ctx=Nil, ns=None, checkCtx=CheckContext(), returnType=None)
      errors.foreach {
        case TypeError(expr, expectedType) =>
          println(expr.o.messageInContext(s"Expected to be of type $expectedType, but got ${expr.t}"))
        case TypeErrorText(expr, message) =>
          println(expr.o.messageInContext(message(expr.t)))
        case OutOfScopeError(ref) =>
          println("Out of scope")
        case IncomparableTypes(left, right) =>
          println(s"Types $left and $right are incomparable")
      }
      println(program)
    } catch {
      case res: VerificationResult =>
        println(res.text)
        res.printStackTrace()
    }
  }
}