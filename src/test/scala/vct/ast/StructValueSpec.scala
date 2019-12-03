package vct.ast

import org.scalatest._
import vct.col.ast._
import vct.col.ast.`type`.ClassType
import vct.col.ast.expr.constant.StructValue

class StructValueSpec extends FlatSpec with Matchers {
  
  "A struct value" should "instantiate with an empty map" in {
    var t = new ClassType(Array("Integer"))
    val sv = new StructValue(t)
    sv.map.isEmpty should be (true)
  }
  
  it should "prevents the map to be altered" in {
    var t = new ClassType(Array("Integer"))
    val sv = new StructValue(t)
    
    a [UnsupportedOperationException] should be thrownBy {
      sv.mapJava.put("test", 12)
    }
  }
}