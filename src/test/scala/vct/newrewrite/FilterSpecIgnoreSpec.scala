package vct.newrewrite

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import vct.col.ast.Constant._
import vct.col.ast.{ApplicableContract, Block, Class, DirectRef, Eval, FileOrigin, InputOrigin, InstanceMethod, Local, LocalDecl, Plus, Program, Return, TInt, TVoid, Variable}
import vct.col.newrewrite.FilterSpecIgnore

import java.nio.file.Paths


class FilterSpecIgnoreSpec extends AnyFlatSpec with Matchers {

  //Iets van een rewrite gebruiken om alle nodes langs te gaan.

  "2" should "be equal to 2" in {
    val i = 2
    assert(i==2)
  }



  it should "not change anything given tree without filterSpecIgnore" in {
    var treeInput: Program = null
    var programExpectedOutput: Program = null

    {
      implicit val origin: InputOrigin = FileOrigin(Paths.get(""), 1, 1, 1, 1)
      val contract1 = ApplicableContract(BooleanValue(value = true),BooleanValue(value = true),BooleanValue(value = true),Seq(),Seq(),Seq())
      val variable1 = new Variable(TInt())
      val body1 = Block(Seq(
        LocalDecl(variable1),
        Eval(Plus(Local(new DirectRef(variable1)), Local(new DirectRef(variable1)))),
        Return(Local(new DirectRef(variable1)))
      ))
      val blame1 = origin
      val method1 = new InstanceMethod(TVoid(), Seq(), Seq(), Option(body1), contract1)(blame1)
      val classNode1 = new Class(Seq(method1))
      treeInput = Program(Seq(classNode1))
    }

    {
      implicit val origin2: InputOrigin = FileOrigin(Paths.get(""), 1, 1, 1, 1)
      val variable2 = new Variable(TInt())
      val contract2 = ApplicableContract(BooleanValue(value = true),BooleanValue(value = true),BooleanValue(value = true),Seq(),Seq(),Seq())
      val body2 = Block(Seq(
        LocalDecl(variable2),
        Eval(Plus(Local(new DirectRef(variable2)), Local(new DirectRef(variable2)))),
        // Eval(Plus(Local(new DirectRef(variable2)), Local(new DirectRef(variable2)))),
        Return(Local(new DirectRef(variable2)))
      ))
      val blame2 = origin2
      val method2 = new InstanceMethod(TVoid(), Seq(), Seq(), Option(body2), contract2)(blame2)
      val classNode2 = new Class(Seq(method2))
      programExpectedOutput = Program(Seq(classNode2))

    }

    val rewriter = FilterSpecIgnore()
    val programActualOutput = rewriter.rewrite(treeInput)

    assert(programActualOutput==programExpectedOutput)
  }

}
