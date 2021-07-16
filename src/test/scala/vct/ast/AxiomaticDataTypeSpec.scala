package vct.ast

import org.scalatest._
import vct.col.ast.`type`.{PrimitiveSort, PrimitiveType}
import vct.col.ast.expr.constant.{ConstantExpression, IntegerValue}
import vct.col.ast.stmt.decl.{AxiomaticDataType, DeclarationStatement}

class AxiomaticDataTypeSpec extends FlatSpec with Matchers {

  def intDeclaration(name: String, value: Int) =
    DeclarationStatement(name, new PrimitiveType(PrimitiveSort.Integer), Some(new ConstantExpression(new IntegerValue(value))))

  "An axiomatic data type" should "instantiate when given valid input" in {
    val params = List[DeclarationStatement](
      intDeclaration("a", 2),
      intDeclaration("b", 5)
    )
    val adt = new AxiomaticDataType("test", params)
    adt.name should be("test")
    adt.parameters.isEmpty should be(false)
    adt.parameters.count(_ => true) should be(2)
  }

  it should "instantiate when given an array of parameters" in {
    val params = Array[DeclarationStatement](
      intDeclaration("a", 2),
      intDeclaration("b", 5)
    )
    val adt = new AxiomaticDataType("test", params)
    adt.name should be("test")
    adt.parameters.isEmpty should be(false)
    adt.parameters.count(_ => true) should be(2)
  }

  it should "not instantiate when the given parameter list is null" in {
    val params: List[DeclarationStatement] = null
    a[IllegalArgumentException] should be thrownBy {
      val adt = new AxiomaticDataType("test", params)
    }
  }

  it should "not instantiate when the given parameter array is null" in {
    val params: Array[DeclarationStatement] = null
    a[NullPointerException] should be thrownBy {
      val adt = new AxiomaticDataType("test", params)
    }
  }

  it should "not allow Java clients to alter the parameter list" in {
    val params = List[DeclarationStatement](intDeclaration("a", 2))
    val adt = new AxiomaticDataType("test", params)
    a[UnsupportedOperationException] should be thrownBy {
      adt.parametersJava.add(intDeclaration("b", 5))
    }
  }
}