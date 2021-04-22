package vct.ast

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import vct.col.ast._
import vct.col.ast.expr.{OperatorExpression, StandardOperator}
import vct.col.ast.expr.constant.ConstantExpression
import vct.col.ast.generic.ASTNode

class OperatorExpressionSpec extends AnyFlatSpec with Matchers {
  
  "A operator expression" should "successfully instantiate with valid/matching input arguments (from an array)" in {
    val op = StandardOperator.Plus
    val args = Array[ASTNode](new ConstantExpression(1), new ConstantExpression(2))
    var oe = new OperatorExpression(op, args)
    oe.argslength should be (2)
  }
  
  it should "successfully instantiate with valid/matching input arguments (from an list)" in {
    val op = StandardOperator.Plus
    val args = List[ASTNode](new ConstantExpression(1), new ConstantExpression(2))
    var oe = new OperatorExpression(op, args)
    oe.argslength should be (2)
  }
  
  it should "not be able to instantie with invalid input (fron an array)" in {
    val op = StandardOperator.Plus
    val args = Array[ASTNode](new ConstantExpression(1))
    a [IllegalArgumentException] should be thrownBy {
      var oe = new OperatorExpression(op, args)
    }
  }
  
  it should "not be able to instantie with invalid input (fron an list)" in {
    val op = StandardOperator.Plus
    val args = List[ASTNode](new ConstantExpression(1))
    a [IllegalArgumentException] should be thrownBy {
      var oe = new OperatorExpression(op, args)
    }
  }
  
  it should "not instantiate when given a null arguments list" in {
    val op = StandardOperator.Plus
    val args : List[ASTNode] = null
    a [IllegalArgumentException] should be thrownBy {
      var oe = new OperatorExpression(op, args)
    }
  }
  
  it should "not instantiate when given a null arguments array" in {
    val op = StandardOperator.Plus
    val args : Array[ASTNode] = null
    a [NullPointerException] should be thrownBy {
      var oe = new OperatorExpression(op, args)
    }
  }
}