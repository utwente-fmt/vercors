package viper.api

import vct.col.ast._
import vct.col.origin._
import vct.col.util.AstBuildHelpers._
import org.scalatest.flatspec.AnyFlatSpec
import viper.api.backend.Backend

sealed trait G

abstract class VerifySpec[P](backend: Backend[P]) extends AnyFlatSpec {
  implicit val noErrors: Blame[VerificationFailure] = NoErrors
  implicit val origin: Origin = DiagnosticOrigin
  private var _registry: Option[ExpectedErrorsRegistry] = None

  val vercors: ItWord = it

  val ref: SilverField[G] = new SilverField(TRef())
  val int: SilverField[G] = new SilverField(TInt())

  implicit def reg: ExpectedErrorsRegistry = _registry match {
    case Some(value) => value
    case None => fail(s"registry is only available while within a test")
  }

  def program(program: => Program[G]): Unit = {
    _registry = Some(new ExpectedErrorsRegistry())
    val inter: P = backend.transform(program, None)
    backend.submit(inter)
    _registry.get.check()
    _registry = None
  }

  def decl(global: => GlobalDeclaration[G]): Unit = {
    program(Program(Seq(ref, int, global))(noErrors))
  }

  def procedure(returnType: => Type[G] = TVoid(),
                args: => Seq[Variable[G]] = Seq(), outArgs: => Seq[Variable[G]] = Seq(),
                body: => Statement[G] = Block(Seq()),
                requires: => Expr[G] = tt, ensures: => Expr[G] = tt, blame: => Blame[CallableFailure] = noErrors): Unit = {
    decl(new Procedure(returnType, args, outArgs, Nil, Option(body), ApplicableContract(UnitAccountedPredicate(requires), UnitAccountedPredicate(ensures), tt, Seq(), Seq(), Seq(), None)(noErrors))(blame))
  }
}
