package vct.test.integration.examples

import com.typesafe.scalalogging.LazyLogging
import vct.col.ast.{AccountedPredicate, AmbiguousResult, ApplicableContract, Block, Class, Comparator, Function, GlobalDeclaration, InstanceFunction, InstanceMethod, PVLConstructor, Procedure, Program, Result, Return, TInt, TVoid, UnitAccountedPredicate}
import vct.col.newrewrite.FilterAndAbstractDeclarations
import vct.col.origin.{BlameCollector, DiagnosticOrigin, PanicBlame}
import vct.col.rewrite.InitialGeneration
import vct.col.util.AstBuildHelpers._
import vct.parsers.transform.{BlameProvider, ConstantBlameProvider}
import vct.test.integration.helper.VercorsSpec

/* Should test multiple concerns here:
   Whether FilterAndAbstractDeclarations is working. Meaning: is transitive use respected, do focus/ignore work, are unused predicates filtered out
   Whether focusing/ignoring tagging works for pvl/java frontends (so focusing/ignoring constructor yields exactly one focused/ignored constructor, etc)
   Some integrating tests that do some (3 to 5) tests of this entire pipeline
*/
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

    it should s"$d1 + $d2 should transform into $d3" in {
      val diff = Comparator.compare(actual, expected).collect({
        case s @ Comparator.StructuralDifference(_, _) => s
      })
      assert(diff.isEmpty, s"=== Diff ===\n$diff")
    }
  }
}

class TechnicalMinimizeSpec2 extends VercorsSpec with LazyLogging {
  type G = InitialGeneration
  implicit val o = DiagnosticOrigin
  type GlobalDeclGen = (Correctness, FilterMode) => BlameProvider => GlobalDeclaration[G]

  sealed trait Correctness
  case object Failing extends Correctness
  case object Verifying extends Correctness

  sealed trait FilterMode;
  case object Focus extends FilterMode
  case object Ignore extends FilterMode
  case object Normal extends FilterMode

  def mkContract(correctness: Correctness): ApplicableContract[G] =
    contract(PanicBlame(""), ensures=UnitAccountedPredicate(const(if(correctness == Failing) false else true)))

  def function(correctness: Correctness, filterMode: FilterMode)(provider: BlameProvider): GlobalDeclaration[G] =
      new Function[G](
        TInt(), Seq(), Seq(), Some(const[G](0)),
        contract(PanicBlame(""), ensures=UnitAccountedPredicate(if(correctness == Failing) AmbiguousResult[G]() === const(1) else const(true))),
        focus = filterMode == Focus, ignore = filterMode == Ignore)(provider())

  def procedure(correctness: Correctness, filterMode: FilterMode)(provider: BlameProvider): GlobalDeclaration[G] =
    new Procedure(
      TVoid(), Seq(), Seq(), Seq(), Some(Block[G](Seq())), mkContract(correctness),
      focus = filterMode == Focus, ignore = filterMode == Ignore)(provider())

  def instanceFunction(correctness: Correctness, filterMode: FilterMode)(provider: BlameProvider): GlobalDeclaration[G] =
    new Class[G](Seq(
      withResult[G, InstanceFunction[G]] { result =>
        new InstanceFunction[G](TInt(), Seq(), Seq(), Some(const[G](0)),
          contract(PanicBlame(""), ensures=UnitAccountedPredicate(if(correctness == Failing) AmbiguousResult[G]() === const(1) else const(true))),
          inline = false, focus = filterMode == Focus, ignore = filterMode == Ignore)(provider())
      }), Seq(), tt)

  def instanceMethod(correctness: Correctness, filterMode: FilterMode)(provider: BlameProvider): GlobalDeclaration[G] =
    new Class[G](Seq(
      new InstanceMethod[G](TInt(), Seq(), Seq(), Seq(), Some(Return(const[G](0))), mkContract(correctness),
        focus = filterMode == Focus, ignore = filterMode == Ignore)(provider())
    ), Seq(), tt)

  def constructor(correctness: Correctness, filterMode: FilterMode)(provider: BlameProvider): GlobalDeclaration[G] =
    new Class[G](Seq(
      new PVLConstructor[G](mkContract(correctness), Seq(), Some(Block(Nil)),
      focus = filterMode == Focus, ignore = filterMode == Ignore)(provider())
    ), Seq(), tt)

  val allContractApplicable: Seq[GlobalDeclGen] = Seq(function, procedure, instanceMethod, instanceFunction, constructor)
  val allCorrectness = Seq(Failing, Verifying)
  val allFilterMode = Seq(Focus, Ignore, Normal)

  var i = 0
  def mustVerify(p: Program[G], collector: BlameCollector, provider: BlameProvider): Unit =
    vercors should verify using silicon in {i += 1; i.toString} col(collector, provider, p)

  def mustFail(p: Program[G], collector: BlameCollector, provider: BlameProvider): Unit =
    vercors should fail withCode "postFailed:false" using silicon in {i += 1; i.toString} col(collector, provider, p)

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
      {
        val collector = BlameCollector()
        val provider = ConstantBlameProvider(collector)
        mustVerify(new Program[G](Seq(
          ca1(Failing, Ignore)(provider),
          ca2(Verifying, Normal)(provider)
        ), null)(provider()), collector, provider)
      }

      {
        val collector = BlameCollector()
        val provider = ConstantBlameProvider(collector)
        mustFail(new Program[G](Seq(
          ca1(Failing, Ignore)(provider),
          ca2(Failing, Normal)(provider)
        ), null)(provider()), collector, provider)
      }
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
}
