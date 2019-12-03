package vct.ast

import org.scalatest._

import scala.collection.JavaConverters._
import vct.col.ast._
import vct.col.ast.`type`.{PrimitiveSort, PrimitiveType, RecordType, RecordTypeEntry}

class RecordTypeSpec extends FlatSpec with Matchers {
  
  "A record type" should "successfully instantiate after invoking the default constructor" in {
    var record = new RecordType(List(
      new RecordTypeEntry("field1", new PrimitiveType(PrimitiveSort.Integer)),
      new RecordTypeEntry("field2", new PrimitiveType(PrimitiveSort.Boolean))
    ))
    
    record.types.size should be (2)
    record.fieldName(0) should be ("field1")
    record.fieldName(1) should be ("field2")
  }
  
  it should "successfully instantiate after calling the alternative constructor (with expected input)" in {
    var names = List("field1", "field2")
    var types = List(new PrimitiveType(PrimitiveSort.Integer), new PrimitiveType(PrimitiveSort.Boolean))
    var record = new RecordType(names, types)
    
    record.types.size should be (2)
    record.fieldName(0) should be ("field1")
    record.fieldName(1) should be ("field2")
  }
  
  it should "successfully instantiate after providing too many names" in {
    var names = List("field1", "field2", "field3")
    var types = List(new PrimitiveType(PrimitiveSort.Integer), new PrimitiveType(PrimitiveSort.Boolean))
    var record = new RecordType(names, types)
    
    record.types.size should be (2)
    record.fieldName(0) should be ("field1")
    record.fieldName(1) should be ("field2")
  }
  
  it should "successfully instantiate after providing too many types" in {
    var names = List("field1", "field2")
    var types = List(
        new PrimitiveType(PrimitiveSort.Integer), 
        new PrimitiveType(PrimitiveSort.Boolean), 
        new PrimitiveType(PrimitiveSort.Integer))
    var record = new RecordType(names, types)
    
    record.types.size should be (2)
  }
  
  it should "not accept instantiation with an empty list" in {
    a [IllegalArgumentException] should be thrownBy {
      var record = new RecordType(List())
    }
  }
  
  it should "not accept instantiation with too few field entries" in {
    var names = List("field1", "field2")
        
    a [IllegalArgumentException] should be thrownBy {
      var record = new RecordType(names, List())
    }
  }
}