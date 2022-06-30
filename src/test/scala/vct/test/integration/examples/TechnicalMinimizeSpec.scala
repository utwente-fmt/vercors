package vct.test.integration.examples

import ch.qos.logback.classic.{Level, Logger}
import com.typesafe.scalalogging.LazyLogging
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.slf4j.LoggerFactory
import vct.col.ast.{AccountedPredicate, ApplicableContract, Block, Class, Comparator, DecreasesClause, Eq, Expr, Function, GlobalDeclaration, InstanceFunction, InstanceMethod, PVLConstructor, Procedure, Program, Result, Return, SignalsClause, SplitAccountedPredicate, TInt, TVoid, Type, UnitAccountedPredicate, Variable}
import vct.col.newrewrite.FilterAndAbstractDeclarations
import vct.col.origin.{Blame, ContractedFailure, DiagnosticOrigin, NontrivialUnsatisfiable, Origin, PanicBlame}
import vct.col.print.Printer
import vct.col.rewrite.InitialGeneration
import vct.test.integration.helper.VercorsSpec
import vct.col.util.AstBuildHelpers._

class TechnicalMinimizeSpec3 extends VercorsSpec with LazyLogging {
  type G = InitialGeneration
  implicit val o = DiagnosticOrigin
  type GlobalDeclGen = (Correctness, FilterMode) => (String, GlobalDeclaration[G])

  sealed trait Correctness
  case object Failing extends Correctness
  case object Verifying extends Correctness

  sealed trait FilterMode;
  case object Focus extends FilterMode
  case object Ignore extends FilterMode
  case object Normal extends FilterMode

  def mkPost(r: Result[G], correctness: Correctness): AccountedPredicate[G] =
    UnitAccountedPredicate(correctness match {
      case Failing => r === const(1)
      case Verifying => const(true)
    })

  def minFunction(correctness: Correctness, filterMode: FilterMode): (String, GlobalDeclaration[G]) =
    (s"function($correctness, $filterMode)", function[G](
      PanicBlame(""), PanicBlame(""),
      returnType = TInt(),
      body = Some(const(0)),
      ensures = mkPost(_, correctness),
      focus = filterMode == Focus,
      ignore = filterMode == Ignore
    ))

  def minProcedure(correctness: Correctness, filterMode: FilterMode): (String, GlobalDeclaration[G]) =
    (s"procedure($correctness, $filterMode)", procedure(
      PanicBlame(""), PanicBlame(""),
      body = Some(Block[G](Seq())),
      ensures = mkPost(_, correctness),
      focus = filterMode == Focus,
      ignore = filterMode == Ignore
    ))

  def minInstanceFunction(correctness: Correctness, filterMode: FilterMode): (String, GlobalDeclaration[G]) =
    (s"instanceFunction($correctness, $filterMode)", `class`(Seq(instanceFunction[G](
      PanicBlame(""), PanicBlame(""),
      returnType = TInt(),
      body = Some(const(0)),
      ensures = mkPost(_, correctness),
      focus = filterMode == Focus,
      ignore = filterMode == Ignore
    ))))

  def minInstanceMethod(correctness: Correctness, filterMode: FilterMode): (String, GlobalDeclaration[G]) =
    (s"instanceMethod($correctness, $filterMode)", `class`(Seq(instanceMethod[G](
      PanicBlame(""), PanicBlame(""),
      body = Some(Block(Nil)),
      ensures = r => mkPost(r, correctness),
      focus = filterMode == Focus,
      ignore = filterMode == Ignore
    ))))

  val allContractApplicable: Seq[GlobalDeclGen] = Seq(minFunction, minProcedure) // , minInstanceMethod, minInstanceFunction)
  val allCorrectness = Seq(Failing, Verifying)
  val allFilterMode = Seq(Focus, Ignore, Normal)

  /* SPECS
    # After ignoring failing contract applicables, verification should succeed
    ALL ca1 ca2: ContractApplicable; failing(ca1) && verifying(ca2) && ignored(ca1) ==> verifying(ca1 * ca2)
    Alternative:
    ALL ca1 ca2: ca1(failing, ignored) + ca2(verifying, normal) ==pass==> ca2(verifying, normal)
    # If all contractapplicables fail, after ignoring failing contract applicables, verification should fail
    ALL ca1 ca2: ContractApplicable; failing(ca1) && failing(ca2) && ignored(ca1) ==> failing(ca1 * ca2)
    Alternative:
    ALL ca1 ca2:
    # If f is ok, g is failing, and you focus f, verification should succeed
    # If f is ok, g is failing, and you focus g, verification should fail
    # Final questions: 1) how to test transitivity is correctly done, how test if enough is kept around?
    # TODO: Should minimizing be touched upon in sat check? Obligation there should be ignored/focused as well?
   */

  for (
    ca1 <- allContractApplicable;
    ca2 <- allContractApplicable
  ) {
    val (d1, g1) = ca1(Failing, Ignore)
    val (d2, g2) = ca2(Verifying, Normal)
    val input = Program[G](Seq(g1, g2), None)(PanicBlame(""))

    val (d3, g3) = ca2(Verifying, Normal)
    val expected = Program[G](Seq(g3), None)(PanicBlame(""))

    val actual = FilterAndAbstractDeclarations().dispatch(input)

    s"$d1 + $d2" should s"transform into $d3" in {
      val diff = Comparator.compare(actual, expected).collect({
        case s @ Comparator.StructuralDifference(_, _) => s
      })
      if (!diff.isEmpty) {
        println("Oh no")
      }
      assert(diff.isEmpty, s"=== Diff ===\n$diff")
    }
  }
}

class TechnicalMinimizeSpec2 extends VercorsSpec with LazyLogging {
  type G = InitialGeneration
  implicit val o = DiagnosticOrigin
  type GlobalDeclGen = (Correctness, FilterMode) => GlobalDeclaration[G]

  sealed trait Correctness
  case object Failing extends Correctness
  case object Verifying extends Correctness

  sealed trait FilterMode;
  case object Focus extends FilterMode
  case object Ignore extends FilterMode
  case object Normal extends FilterMode

  def mkContract(correctness: Correctness): ApplicableContract[G] =
    contract(PanicBlame(""), ensures=UnitAccountedPredicate(const(if(correctness == Failing) false else true)))

  def function(correctness: Correctness, filterMode: FilterMode): GlobalDeclaration[G] =
    withResult[G, Function[G]]((r: Result[G]) =>
      new Function[G](
        TInt(), Seq(), Seq(), Some(const[G](0)),
        contract(PanicBlame(""), ensures=UnitAccountedPredicate(if(correctness == Failing) r === const(1) else const(true))),
        focus = filterMode == Focus, ignore = filterMode == Ignore)(PanicBlame(""))
    )

  def procedure(correctness: Correctness, filterMode: FilterMode): GlobalDeclaration[G] =
    new Procedure(
      TVoid(), Seq(), Seq(), Seq(), Some(Block[G](Seq())), mkContract(correctness),
      focus = filterMode == Focus, ignore = filterMode == Ignore)(PanicBlame(""))

  def instanceFunction(correctness: Correctness, filterMode: FilterMode): GlobalDeclaration[G] =
    new Class[G](Seq(
      withResult[G, InstanceFunction[G]] { r =>
        new InstanceFunction[G](TInt(), Seq(), Seq(), Some(const[G](0)),
          contract(PanicBlame(""), ensures=UnitAccountedPredicate(if(correctness == Failing) r === const(1) else const(true))),
          inline = false, focus = filterMode == Focus, ignore = filterMode == Ignore)(PanicBlame(""))
      }), Seq(), tt)

  def instanceMethod(correctness: Correctness, filterMode: FilterMode): GlobalDeclaration[G] =
    new Class[G](Seq(
      new InstanceMethod[G](TInt(), Seq(), Seq(), Seq(), Some(Return(const[G](0))), mkContract(correctness),
        focus = filterMode == Focus, ignore = filterMode == Ignore)(PanicBlame(""))
    ), Seq(), tt)

  def constructor(correctness: Correctness, filterMode: FilterMode): GlobalDeclaration[G] =
    new Class[G](Seq(
      new PVLConstructor[G](mkContract(correctness), Seq(), Some(Block(Nil)),
      focus = filterMode == Focus, ignore = filterMode == Ignore)(PanicBlame(""))
    ), Seq(), tt)

  val allContractApplicable: Seq[GlobalDeclGen] = Seq(function, procedure, instanceMethod, instanceFunction, constructor)
  val allCorrectness = Seq(Failing, Verifying)
  val allFilterMode = Seq(Focus, Ignore, Normal)

  var i = 0
  def mustVerify(p: Program[G]): Unit = {
    val sb = new java.lang.StringBuilder
    val printer = Printer(sb, syntax = vct.col.print.PVL)
    printer.print(p)
    val str = sb.toString

    LoggerFactory.getLogger("vct.test").asInstanceOf[Logger].setLevel(Level.ALL)
    logger.info(s"------- some test case $i\n$str")
    logger.info(s"------- program:\n$str")
    vercors should verify using silicon in s"some test case $i" pvl(str)
    i += 1
  }
  def mustFail(p: Program[G]): Unit = {
    val sb = new java.lang.StringBuilder
    val printer = Printer(sb, syntax = vct.col.print.PVL)
    printer.print(p)
    val str = sb.toString
    println(s"------- some test case $i\n$str")
    vercors should fail withCode "postFailed:false" using silicon in s"some test case $i" pvl(str)
    i += 1
  }

  /* SPECS
    # After ignoring failing contract applicables, verification should succeed
    ALL ca1 ca2: ContractApplicable; failing(ca1) && verifying(ca2) && ignored(ca1) ==> verifying(ca1 * ca2)
    # If all contractapplicables fail, after ignoring failing contract applicables, verification should fail
    ALL ca1 ca2: ContractApplicable; failing(ca1) && failing(ca2) && ignored(ca1) ==> failing(ca1 * ca2)
    # If f is ok, g is failing, and you focus f, verification should succeed
    # If f is ok, g is failing, and you focus g, verification should fail
    # Final questions: 1) how to test transitivity is correctly done, how test if enough is kept around?
    # TODO: Should minimizing be touched upon in sat check? Obligation there should be ignored/focused as well?
   */
  for (
    ca1 <- allContractApplicable;
    ca2 <- allContractApplicable
  ) {
    mustVerify(new Program[G](Seq(
      ca1(Failing, Ignore),
      ca2(Verifying, Normal)
    ), null)(PanicBlame("")))

    mustFail(new Program[G](Seq(
      ca1(Failing, Ignore),
      ca2(Failing, Normal)
    ), null)(PanicBlame("")))
  }
}

class TechnicalMinimizeSpec extends VercorsSpec {
  vercors should verify using anyBackend example "technical/minimize/FocusMethod.java"
  vercors should verify using anyBackend example "technical/minimize/FocusMethodTransitive.java"
  vercors should verify using anyBackend example "technical/minimize/FocusFunction.java"
  vercors should verify using anyBackend example "technical/minimize/FocusFunctionTransitive.java"
  vercors should verify using anyBackend example "technical/minimize/IgnoreFunction.java"
  vercors should verify using anyBackend example "technical/minimize/IgnoreMethod.java"

  vercors should verify using anyBackend example "technical/minimize/IgnoreConstructor.java"
  vercors should verify using anyBackend example "technical/minimize/IgnoreConstructor.pvl"
  vercors should verify using anyBackend example "technical/minimize/FocusConstructor.java"
  vercors should verify using anyBackend example "technical/minimize/FocusConstructor.pvl"

  vercors should verify using anyBackend example "technical/minimize/FocusTopLevelFunction.java"
  vercors should verify using anyBackend example "technical/minimize/FocusTopLevelFunction.pvl"
  vercors should verify using anyBackend example "technical/minimize/FocusTopLevelProcedure.pvl"


//  vercors should verify using anyBackend in "example asserting this instanceof the defining class" java """
//    class MyClass {
//      void foo() {
//        MyClass myClass = new MyClass();
//        assert myClass instanceof MyClass;
//      }
//    }
//  """
}
