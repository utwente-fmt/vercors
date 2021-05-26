package vct.newrewrite

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import vct.col.ast.Constant._
import vct.col.ast._
import vct.col.newrewrite.FilterSpecIgnore

import java.nio.file.Paths
import scala.Console


class FilterSpecIgnoreSpec extends AnyFlatSpec with Matchers {

  it should "not change anything given tree without filterSpecIgnore" in {
    var treeInput: Program = null
    var programExpectedOutput: Program = null

    {
      implicit val origin: InputOrigin = FileOrigin(Paths.get(""), 1, 1, 1, 1)
      val contract1 = ApplicableContract(BooleanValue(value = true),BooleanValue(value = true),BooleanValue(value = true),Seq(),Seq(),Seq())
      val variable1 = new Variable(TInt())
      val body1 = Block(Seq(
        LocalDecl(variable1),
        SpecIgnoreStart(),
        Eval(Plus(Local(new DirectRef[Variable](variable1)), Local(new DirectRef[Variable](variable1)))),
        SpecIgnoreEnd(),
        Return(Local(new DirectRef[Variable](variable1)))
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
        Return(Local(new DirectRef[Variable](variable2)))
      ))
      val blame2 = origin2
      val method2 = new InstanceMethod(TVoid(), Seq(), Seq(), Option(body2), contract2)(blame2)
      val classNode2 = new Class(Seq(method2))
      programExpectedOutput = Program(Seq(classNode2))

    }

    val rewriter = FilterSpecIgnore()
    val programActualOutput = rewriter.rewrite(treeInput)

    val result = programEquals(programActualOutput,programExpectedOutput)
    result match{
      case Right(_) =>
        assert(true)
      case Left(errorMessage) =>
        Console.println(errorMessage)
        assert(false)
    }
    assert(result.isRight)
  }

  //TODO: replace to string to a col printer?
  //TODO: replace all left with error throwing. It is easier with debugger.
  def programEquals(left: Program, right: Program): Either[String, Unit] ={

    throw AstUnequalException("test",left,right)

    val succession: Map[Declaration, Declaration] = Map()
    val leftGlobalDeclarations = left.decls
    val rightGlobalDeclarations = right.decls
    if (leftGlobalDeclarations.size != rightGlobalDeclarations.size) {
      return Left(s"Mismatched globalDeclaration length in $left and $right")
    }
    val moreSuccession = succession ++ leftGlobalDeclarations.zip(rightGlobalDeclarations).toMap

    for((leftDeclaration, rightDeclaration) <- leftGlobalDeclarations.zip(rightGlobalDeclarations)) {
      val result = astEquals(leftDeclaration, rightDeclaration, moreSuccession)
      if (result.isLeft) return result
    }
    Right()
  }

  def astEquals(left: Node, right: Node, succession: Map[Declaration, Declaration]): Either[String, Unit] = {
    val leftDecls = left.subnodes.collect { case decl: Declaration => decl }
    val rightDecls = right.subnodes.collect { case decl: Declaration => decl }
    if (leftDecls.size != rightDecls.size) {
      return Left(s"Mismatched scope length in $left and $right")
    }
    val moreSuccession = succession ++ leftDecls.zip(rightDecls).toMap

    if (left.getClass != right.getClass) {
      val leftClass = left.getClass
      val rightClass = right.getClass
      return Left(s"The kind $leftClass is not the same as $rightClass. From objects $left and $right respectively")
    }

    if (left.subnodes.size != right.subnodes.size) {
      return Left(s"The tree $left does not have the same number of subnodes as $right")
    }

    (left, right) match {
      case (left: Declaration, right: Declaration) =>
        astDeclarationEquals(left, right, moreSuccession)
      case (left: scala.Product, right: scala.Product) =>
        astProductEquals(left,right,succession)
      case (_, _) =>
        Left(s"Incomparable: $left and $right")
    }
  }

  def astProductEquals(left: scala.Product, right: scala.Product, succession: Map[Declaration, Declaration]): Either[String, Unit] = {
    for ((leftValue, rightValue) <- left.productIterator.zip(right.productIterator)) {
      val result = astValueEquals(leftValue, rightValue, succession)
      if (result.isLeft) return result
    }
    Right()
  }

  def astValueEquals(left: Any, right: Any, succession: Map[Declaration, Declaration]): Either[String, Unit] = (left, right) match {
    case (left: Ref[Declaration], right: Ref[Declaration]) =>
      astDeclarationEquals(left.decl, right.decl, succession)
    case (left: Node, right: Node) =>
      astEquals(left, right, succession)
    case (left, right) =>
      if (left == right) Right(())
      else Left(s"Inequal values: $left and $right")
  }

  def astDeclarationEquals(left: Declaration, right: Declaration, succession: Map[Declaration, Declaration]): Either[String, Unit] = {
    succession.get(left) match {
      case Some(decl) if decl == right =>
        //TODO: does not check all values of the declaration
        for ((left, right) <- left.subnodes.zip(right.subnodes)) {
          val result = astEquals(left, right, succession)
          if (result.isLeft) return result
        }
        Right(())
      case Some(other) =>
        Left(s"Comparing $right we expected a declaration matching $left, " +
          s"but the matching declaration of that is $other")
      case None =>
        Left(s"Comparing $right we expected a declaration matching $left, " +
          s"but it has no matching declaration.")
    }
  }


  case class AstUnequalException(message: String,left: Any, right: Any) extends Exception(this.toString()) {

    override def toString: String = {
      var completeMessage = message
      completeMessage += "\n";
      completeMessage += left;
      completeMessage += "\n";
      completeMessage += right;
      return completeMessage
    }

  }


}
