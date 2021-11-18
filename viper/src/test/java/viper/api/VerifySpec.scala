package viper.api

import vct.col.ast._
import vct.col.origin._
import Constant._
import org.scalatest.flatspec.AnyFlatSpec

abstract class VerifySpec(backend: Backend) extends AnyFlatSpec {
  implicit val noErrors: Blame[VerificationFailure] = NoErrors
  implicit val origin: Origin = DiagnosticOrigin
  private var _registry: Option[ExpectedErrorsRegistry] = None

  val vercors: ItWord = it

  val ref: SilverField = new SilverField(TRef())
  val int: SilverField = new SilverField(TInt())

  implicit def reg: ExpectedErrorsRegistry = _registry match {
    case Some(value) => value
    case None => fail(s"registry is only available while within a test")
  }

  def program(program: => Program): Unit = {
    _registry = Some(new ExpectedErrorsRegistry())
    backend.submit(program)
    _registry.get.check()
    _registry = None
  }

  def decl(global: => GlobalDeclaration): Unit = {
    program(Program(Seq(ref, int, global))(noErrors))
  }

  def procedure(returnType: => Type = TVoid(),
                args: => Seq[Variable] = Seq(), outArgs: => Seq[Variable] = Seq(),
                body: => Statement = Block(Seq()),
                requires: => Expr = true, ensures: => Expr = true, blame: => Blame[PostconditionFailed] = noErrors): Unit = {
    decl(new Procedure(returnType, args, outArgs, Nil, Option(body), ApplicableContract(requires, ensures, true, Seq(), Seq(), Seq()))(blame))
  }
}
