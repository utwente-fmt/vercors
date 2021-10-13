package vct.newrewrite

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import vct.col.ast._
import Constant._
import vct.col.newrewrite.FilterSpecIgnore
import vct.helper.{ColHelper, SimpleProgramGenerator}


class FilterSpecIgnoreSpec extends AnyFlatSpec with Matchers {

  //nothing should change
  //should remove something
  //throw error with unbalanced tree 2x

  val rewriter = FilterSpecIgnore()

  it should "not change anything given tree without filterSpecIgnore expect for DirectRef to LazyRef" in {
    var programInput: Program = null
    var programExpectedOutput: Program = null

    {
      implicit val origin: Origin = SimpleProgramGenerator.generateSimpleInputOrigin()
      val variable = new Variable(TInt())
      val body = Block(Seq(
        LocalDecl(variable),
        Eval(Plus(Local(new DirectRef[Variable](variable)), Local(new DirectRef[Variable](variable)))),
        Return(Local(new DirectRef[Variable](variable)))
      ))
      programInput = SimpleProgramGenerator.generateProgramWithSingleClassAndSingleMethod(body)
    }

    {
      implicit val origin: Origin = SimpleProgramGenerator.generateSimpleInputOrigin()
      val variable = new Variable(TInt())
      val body = Block(Seq(
        LocalDecl(variable),
        Eval(Plus(Local(new LazyRef[Variable](variable)), Local(new LazyRef[Variable](variable)))),
        Return(Local(new LazyRef[Variable](variable)))
      ))
      programExpectedOutput = SimpleProgramGenerator.generateProgramWithSingleClassAndSingleMethod(body)
    }
    ColHelper.assertEquals(rewriter.dispatch(programInput), programExpectedOutput)
  }

  it should "remove nodes within filterSpecIgnore" in {
    var programInput: Program = null
    var programExpectedOutput: Program = null

    {
      implicit val origin: Origin = SimpleProgramGenerator.generateSimpleInputOrigin()
      val variable = new Variable(TInt())
      val body = Block(Seq(
        LocalDecl(variable),
        SpecIgnoreStart(),
        Eval(Plus(Local(new DirectRef[Variable](variable)), Local(new DirectRef[Variable](variable)))),
        SpecIgnoreEnd(),
        Return(Local(new DirectRef[Variable](variable)))
      ))
      programInput = SimpleProgramGenerator.generateProgramWithSingleClassAndSingleMethod(body)
    }

    {
      implicit val origin: Origin = SimpleProgramGenerator.generateSimpleInputOrigin()
      val variable = new Variable(TInt())
      val body = Block(Seq(
        LocalDecl(variable),
        Return(Local(new LazyRef[Variable](variable)))
      ))
      programExpectedOutput = SimpleProgramGenerator.generateProgramWithSingleClassAndSingleMethod(body)
    }
    ColHelper.assertEquals(rewriter.dispatch(programInput), programExpectedOutput)
  }

  it should "throw error with two many SpecIgnoreStart" in {
    var programInput: Program = null
    var programExpectedOutput: Program = null

    {
      implicit val origin: Origin = SimpleProgramGenerator.generateSimpleInputOrigin()
      val variable = new Variable(TInt())
      val body = Block(Seq(
        LocalDecl(variable),
        SpecIgnoreStart(),
        Eval(Plus(Local(new DirectRef[Variable](variable)), Local(new DirectRef[Variable](variable)))),
        Return(Local(new DirectRef[Variable](variable)))
      ))
      programInput = SimpleProgramGenerator.generateProgramWithSingleClassAndSingleMethod(body)
    }

    rewriter.dispatch(programInput)
    throw new NotImplementedError()
  }

  it should "throw error with two many SpecIgnoreEnd" in {
    var programInput: Program = null
    var programExpectedOutput: Program = null

    {
      implicit val origin: Origin = SimpleProgramGenerator.generateSimpleInputOrigin()
      val variable = new Variable(TInt())
      val body = Block(Seq(
        LocalDecl(variable),
        Eval(Plus(Local(new DirectRef[Variable](variable)), Local(new DirectRef[Variable](variable)))),
        SpecIgnoreEnd(),
        Return(Local(new DirectRef[Variable](variable)))
      ))
      programInput = SimpleProgramGenerator.generateProgramWithSingleClassAndSingleMethod(body)
    }

    rewriter.dispatch(programInput)
    throw new NotImplementedError()
  }

  it should "do some testing" in {
    implicit val o: Origin = DiagnosticOrigin

    ColHelper.assertEquals(
      left = Plus(1, 1),
      right = Minus(1, 1)
    )
  }
}
