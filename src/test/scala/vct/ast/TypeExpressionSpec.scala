package vct.ast

import org.scalatest._
import vct.col.ast.`type`.{ClassType, Type, TypeExpression, TypeOperator}

class TypeExpressionSpec extends FlatSpec with Matchers {

  "A type expression" should "successfully instantiate when given valid input" in {
    val op = TypeOperator.Static
    val types = List[Type](new ClassType("A"), new ClassType("B"))
    var expr = new TypeExpression(op, types)

    expr.operator should be(TypeOperator.Static)
    expr.types.isEmpty should be(false)
    expr.types.count(_ => true) should be(2)
  }

  it should "give the corect first (heading) type when queries for it" in {
    val op = TypeOperator.Static
    var t1 = new ClassType("A")
    var t2 = new ClassType("B")
    var expr = new TypeExpression(op, List[Type](t1, t2))
    expr.firstType should be(t1)
  }

  it should "successfully instantiate when given an empty list as input" in {
    val op = TypeOperator.Static
    var expr = new TypeExpression(op, List[Type]())
    expr.types.isEmpty should be(true)
    expr.types.count(_ => true) should be(0)
  }

  it should "fail to instantiate when given a null list as input" in {
    val op = TypeOperator.Static
    val types: List[Type] = null
    a[IllegalArgumentException] should be thrownBy {
      var expr = new TypeExpression(op, types)
    }
  }

  it should "successfully instantiate when given an array of types as input" in {
    val op = TypeOperator.Static
    val types = Array[Type](new ClassType("A"), new ClassType("B"))
    var expr = new TypeExpression(op, types)

    expr.operator should be(TypeOperator.Static)
    expr.types.isEmpty should be(false)
    expr.types.count(_ => true) should be(2)
  }

  it should "fail to instantiate when given a null array as input" in {
    val op = TypeOperator.Static
    val types: Array[Type] = null
    a[NullPointerException] should be thrownBy {
      var expr = new TypeExpression(op, types)
    }
  }

  it should "not allow the Java wrapper to change the type list contents" in {
    val op = TypeOperator.Static
    val types = List[Type](new ClassType("A"), new ClassType("B"))
    var expr = new TypeExpression(op, types)
    a[UnsupportedOperationException] should be thrownBy {
      expr.typesJava.add(new ClassType("B"))
    }
  }
}
