package vct.ast

import org.scalatest._
import vct.col.ast._
import vct.col.ast.`type`.{PrimitiveSort, PrimitiveType}
import vct.col.ast.generic.ASTNode
import vct.mock.TypeMock

class TypeSpec extends FlatSpec with Matchers {
  
  "A type" should "yield an empty string when calling toString when there are not arguments" in {
    var tupletype = new TypeMock(List[ASTNode]())
    tupletype.toString should be ("")
  }
  
  it should "yield a string representing all argument types when calling toString" in {
    var inttype = new PrimitiveType(PrimitiveSort.Integer)
    var booltype = new PrimitiveType(PrimitiveSort.Boolean)
    var tupletype = new TypeMock(List[ASTNode](inttype, booltype))
    tupletype.toString should be ("<Integer,Boolean>")
  }
}