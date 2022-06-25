package vct.test.integration.examples

import ch.qos.logback.classic.{Level, Logger}
import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import org.slf4j.LoggerFactory
import vct.col.ast.{AmbiguousResult, ApplicableContract, Block, Class, Eq, Function, GlobalDeclaration, InstanceFunction, InstanceMethod, PVLConstructor, Procedure, Program, Result, Return, SplitAccountedPredicate, TInt, TVoid, UnitAccountedPredicate}
import vct.col.origin.{BlameCollector, DiagnosticOrigin, PanicBlame}
import vct.col.print.Printer
import vct.col.rewrite.InitialGeneration
import vct.test.integration.helper.VercorsSpec
import vct.col.util.AstBuildHelpers._
import vct.main.stages.Stages
import vct.parsers.transform.{BlameProvider, ConstantBlameProvider}

class TechnicalMinimizeSpec2 extends VercorsSpec with LazyLogging {
  type G = InitialGeneration
  implicit val o = DiagnosticOrigin
  type GlobalDeclGen = (Correctness, FilterMode) => GlobalDeclaration[G]

  val collector: ScopedStack[BlameCollector] = ScopedStack()
  val blameProvider: ScopedStack[BlameProvider] = ScopedStack() // = ConstantBlameProvider(collector)

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
      new Function[G](
        TInt(), Seq(), Seq(), Some(const[G](0)),
        contract(PanicBlame(""), ensures=UnitAccountedPredicate(if(correctness == Failing) AmbiguousResult[G]() === const(1) else const(true))),
        focus = filterMode == Focus, ignore = filterMode == Ignore)(blameProvider.top())

  def procedure(correctness: Correctness, filterMode: FilterMode): GlobalDeclaration[G] =
    new Procedure(
      TVoid(), Seq(), Seq(), Seq(), Some(Block[G](Seq())), mkContract(correctness),
      focus = filterMode == Focus, ignore = filterMode == Ignore)(blameProvider.top())

  def instanceFunction(correctness: Correctness, filterMode: FilterMode): GlobalDeclaration[G] =
    new Class[G](Seq(
      withResult[G, InstanceFunction[G]] { result =>
        new InstanceFunction[G](TInt(), Seq(), Seq(), Some(const[G](0)),
          contract(PanicBlame(""), ensures=UnitAccountedPredicate(if(correctness == Failing) AmbiguousResult[G]() === const(1) else const(true))),
          inline = false, focus = filterMode == Focus, ignore = filterMode == Ignore)(blameProvider.top())
      }), Seq(), tt)

  def instanceMethod(correctness: Correctness, filterMode: FilterMode): GlobalDeclaration[G] =
    new Class[G](Seq(
      new InstanceMethod[G](TInt(), Seq(), Seq(), Seq(), Some(Return(const[G](0))), mkContract(correctness),
        focus = filterMode == Focus, ignore = filterMode == Ignore)(blameProvider.top())
    ), Seq(), tt)

  def constructor(correctness: Correctness, filterMode: FilterMode): GlobalDeclaration[G] =
    new Class[G](Seq(
      new PVLConstructor[G](mkContract(correctness), Seq(), Some(Block(Nil)),
      focus = filterMode == Focus, ignore = filterMode == Ignore)(blameProvider.top())
    ), Seq(), tt)

  val allContractApplicable: Seq[GlobalDeclGen] = Seq(function, procedure, instanceMethod, instanceFunction, constructor)
  val allCorrectness = Seq(Failing, Verifying)
  val allFilterMode = Seq(Focus, Ignore, Normal)

  var i = 0
  def mustVerify(p: Program[G]): Unit =
    vercors should verify using silicon in s"" col(collector.top, blameProvider.top, p)

  def mustFail(p: Program[G]): Unit =
    vercors should fail withCode "postFailed:false" using silicon in s"don't ask me" col(collector.top, blameProvider.top, p)

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
    collector.having(BlameCollector()) {
      blameProvider.having(ConstantBlameProvider(collector.top)) {
        mustVerify(new Program[G](Seq(
          ca1(Failing, Ignore),
          ca2(Verifying, Normal)
        ), null)(blameProvider.top()))
      }
    }

    collector.having(BlameCollector()) {
      blameProvider.having(ConstantBlameProvider(collector.top)) {
        mustFail(new Program[G](Seq(
          ca1(Failing, Ignore),
          ca2(Failing, Normal)
        ), null)(blameProvider.top()))
      }
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


//  vercors should verify using anyBackend in "example asserting this instanceof the defining class" java """
//    class MyClass {
//      void foo() {
//        MyClass myClass = new MyClass();
//        assert myClass instanceof MyClass;
//      }
//    }
//  """
}
