package vct.ast

import org.scalatest._
import vct.col.ast.`type`.ASTReserved
import vct.col.ast.expr.NameExpression
import vct.col.ast.stmt.composite.{BlockStatement, Constraining}

class ConstrainingSpec extends FlatSpec with Matchers {

  "A constrained block" should "successfully instantiate when given proper input" in {
    val block = new BlockStatement
    val vars = List[NameExpression](
      new NameExpression(ASTReserved.Final),
      new NameExpression(ASTReserved.Static)
    )

    val cblock = new Constraining(block, vars)
    cblock.block should be(block)
    cblock.vars.isEmpty should be(false)
    cblock.vars.count(_ => true) should be(2)
  }

  it should "instantiate from an array of restrictions" in {
    val block = new BlockStatement
    val vars = Array[NameExpression](
      new NameExpression(ASTReserved.Final),
      new NameExpression(ASTReserved.Static)
    )

    val cblock = new Constraining(block, vars)
    cblock.block should be(block)
    cblock.vars.isEmpty should be(false)
    cblock.vars.count(_ => true) should be(2)
  }

  it should "not accept a null list when instantiating" in {
    val block = new BlockStatement
    val vars: List[NameExpression] = null
    a[IllegalArgumentException] should be thrownBy {
      val cblock = new Constraining(block, vars)
    }
  }

  it should "not accept a null array when instantiating" in {
    val block = new BlockStatement
    val vars: Array[NameExpression] = null
    a[NullPointerException] should be thrownBy {
      val cblock = new Constraining(block, vars)
    }
  }

  it should "not allow Java clients to alter the underlying list of vars" in {
    val block = new BlockStatement
    val vars = List[NameExpression](new NameExpression(ASTReserved.Final))
    val cblock = new Constraining(block, vars)
    a[UnsupportedOperationException] should be thrownBy {
      cblock.varsJava.add(new NameExpression(ASTReserved.Static))
    }
  }
}