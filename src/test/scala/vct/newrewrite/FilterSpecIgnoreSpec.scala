package vct.newrewrite

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import vct.col.ast._
import vct.col.origin._
import vct.col.util.AstBuildHelpers._
import vct.col.rewrite.FilterSpecIgnore
import vct.col.ref.{DirectRef, LazyRef}
import vct.col.rewrite.{InitialGeneration, Rewritten}
import vct.helper.{ColHelper, SimpleProgramGenerator}


class FilterSpecIgnoreSpec extends AnyFlatSpec with Matchers {
  type G = InitialGeneration
  //nothing should change
  //should remove something
  //throw error with unbalanced tree 2x

  val rewriter = FilterSpecIgnore[G]()

  it should "not change anything given tree without filterSpecIgnore expect for DirectRef to LazyRef" in {
    var programInput: Program[G] = null
    var programExpectedOutput: Program[G] = null

    {
      implicit val origin: Origin = SimpleProgramGenerator.generateSimpleInputOrigin()
      val variable = new Variable[G](TInt())
      val body = Block(Seq[Statement[G]](
        LocalDecl(variable),
        Eval(Plus(Local(new DirectRef[G, Variable[G]](variable)), Local(new DirectRef[G, Variable[G]](variable)))),
        Return(Local(new DirectRef[G, Variable[G]](variable)))
      ))
      programInput = SimpleProgramGenerator.generateProgramWithSingleClassAndSingleMethod(body)
    }

    {
      implicit val origin: Origin = SimpleProgramGenerator.generateSimpleInputOrigin()
      val variable = new Variable[G](TInt())
      val body = Block(Seq[Statement[G]](
        LocalDecl(variable),
        Eval(Plus(Local(new LazyRef[G, Variable[G]](variable)), Local(new LazyRef[G, Variable[G]](variable)))),
        Return(Local(new LazyRef[G, Variable[G]](variable)))
      ))
      programExpectedOutput = SimpleProgramGenerator.generateProgramWithSingleClassAndSingleMethod(body)
    }
    ColHelper.assertEquals(rewriter.dispatch(programInput), programExpectedOutput)
  }

  it should "remove nodes within filterSpecIgnore" in {
    var programInput: Program[G] = null
    var programExpectedOutput: Program[G] = null

    {
      implicit val origin: Origin = SimpleProgramGenerator.generateSimpleInputOrigin()
      val variable = new Variable[G](TInt())
      val body = Block(Seq[Statement[G]](
        LocalDecl(variable),
        SpecIgnoreStart(),
        Eval(Plus(Local(new DirectRef[G, Variable[G]](variable)), Local(new DirectRef[G, Variable[G]](variable)))),
        SpecIgnoreEnd(),
        Return(Local(new DirectRef[G, Variable[G]](variable)))
      ))
      programInput = SimpleProgramGenerator.generateProgramWithSingleClassAndSingleMethod(body)
    }

    {
      implicit val origin: Origin = SimpleProgramGenerator.generateSimpleInputOrigin()
      val variable = new Variable[G](TInt())
      val body = Block(Seq[Statement[G]](
        LocalDecl(variable),
        Return(Local(new LazyRef[G, Variable[G]](variable)))
      ))
      programExpectedOutput = SimpleProgramGenerator.generateProgramWithSingleClassAndSingleMethod(body)
    }
    ColHelper.assertEquals(rewriter.dispatch(programInput), programExpectedOutput)
  }

  it should "throw error with two many SpecIgnoreStart" in {
    var programInput: Program[G] = null
    var programExpectedOutput: Program[G] = null

    {
      implicit val origin: Origin = SimpleProgramGenerator.generateSimpleInputOrigin()
      val variable = new Variable[G](TInt())
      val body = Block(Seq[Statement[G]](
        LocalDecl(variable),
        SpecIgnoreStart(),
        Eval(Plus(Local(new DirectRef[G, Variable[G]](variable)), Local(new DirectRef[G, Variable[G]](variable)))),
        Return(Local(new DirectRef[G, Variable[G]](variable)))
      ))
      programInput = SimpleProgramGenerator.generateProgramWithSingleClassAndSingleMethod(body)
    }

    assertThrows[FilterSpecIgnore.DanglingIgnoreStart] {
      rewriter.dispatch(programInput)
    }
  }

  it should "throw error with two many SpecIgnoreEnd" in {
    var programInput: Program[G] = null
    var programExpectedOutput: Program[G] = null

    {
      implicit val origin: Origin = SimpleProgramGenerator.generateSimpleInputOrigin()
      val variable = new Variable[G](TInt())
      val body = Block(Seq[Statement[G]](
        LocalDecl(variable),
        Eval(Plus(Local(new DirectRef[G, Variable[G]](variable)), Local(new DirectRef[G, Variable[G]](variable)))),
        SpecIgnoreEnd(),
        Return(Local(new DirectRef[G, Variable[G]](variable)))
      ))
      programInput = SimpleProgramGenerator.generateProgramWithSingleClassAndSingleMethod(body)
    }

    assertThrows[FilterSpecIgnore.ExtraIgnoreEnd] {
      rewriter.dispatch(programInput)
    }
  }
}
