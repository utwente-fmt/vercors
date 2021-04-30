package vct.ast

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters._
import vct.col.ast._
import vct.col.ast.`type`.{PrimitiveSort, PrimitiveType, TupleType, Type}

class TupleTypeSpec extends AnyFlatSpec with Matchers {
  
  "A tuple type" should "yield the correct type when queried for one" in {
    var inttype = new PrimitiveType(PrimitiveSort.Integer)
    var booltype = new PrimitiveType(PrimitiveSort.Boolean)
    var tupletype = new TupleType(Array[Type](inttype, booltype))
    tupletype.types(0) should be (inttype)
    tupletype.types(1) should be (booltype)
  }
  
  it should "throw out-of-bounds when accessing a non-existing type" in {
    var inttype = new PrimitiveType(PrimitiveSort.Integer)
    var tupletype = new TupleType(Array[Type](inttype))
    
    a [IndexOutOfBoundsException] should be thrownBy {
      tupletype.types(1) should be (inttype)
    }
  }

  it should "be immune to additions to the types array given to the constructor" in {
    var types = new ArrayBuffer[Type]()
    var inttype = new PrimitiveType(PrimitiveSort.Integer)
    var booltype = new PrimitiveType(PrimitiveSort.Boolean)
    
    types += inttype
    var tupletype = new TupleType(types.toArray)
    types += booltype
    
    a [IndexOutOfBoundsException] should be thrownBy {
      tupletype.types(1) should be (booltype)
    }
  }
  
  it should "be immune to updates to the types array given to the constructor" in {
    var types = new ArrayBuffer[Type]()
    var inttype = new PrimitiveType(PrimitiveSort.Integer)
    var booltype = new PrimitiveType(PrimitiveSort.Boolean)
    
    types += inttype
    var tupletype = new TupleType(types.toArray)
    types(0) = booltype
    
    tupletype.types(0) should be (inttype)
  }
  
  it should "also be immune to type updates from a Java array (after conversion)" in {
    var types = new java.util.ArrayList[Type]()
    var inttype = new PrimitiveType(PrimitiveSort.Integer)
    var booltype = new PrimitiveType(PrimitiveSort.Boolean)

    types.add(inttype)
    var tupletype = new TupleType(types.asScala.toArray)
    types.add(booltype)
    
    a [IndexOutOfBoundsException] should be thrownBy {
      tupletype.types(1) should be (booltype)
    }
  }
  
  it should "(always) yield false when queried for a subtype" in {
    var inttype = new PrimitiveType(PrimitiveSort.Integer)
    var tupletype = new TupleType(Array[Type](inttype))
    tupletype.supertypeof(null, inttype) should be (false)
  }
  
  it should "not be able to accept an empty type array" in {
    a [IllegalArgumentException] should be thrownBy {
      var tupletype = new TupleType(Array[Type]())
    }
  }
  
  it should "not be able to accept an empty type list" in {
    a [IllegalArgumentException] should be thrownBy {
      var tupletype = new TupleType(List[Type]())
    }
  }
  
  it should "yield a type list of equal size as the one provided when queried for it" in {
    var inttype = new PrimitiveType(PrimitiveSort.Integer)
    var booltype = new PrimitiveType(PrimitiveSort.Boolean)
    var tupletype = new TupleType(Array[Type](inttype, booltype))
    tupletype.types.size should be (2)
  }
  
  it should "make the types getter immune for type updates" in {
    var types = new ArrayBuffer[Type]()
    var inttype = new PrimitiveType(PrimitiveSort.Integer)
    var booltype = new PrimitiveType(PrimitiveSort.Boolean)

    types += inttype
    var tupletype = new TupleType(types.toArray)
    types += booltype
    
    tupletype.types.size should be (1)
  }
  
  it should "prevent that updates to the internal types arrays reflect to the array given to the constructor" in {
    var types = new ArrayBuffer[Type]()
    var inttype = new PrimitiveType(PrimitiveSort.Integer)
    var booltype = new PrimitiveType(PrimitiveSort.Boolean)
    types += inttype
    var tupletype = new TupleType(types.toArray)
    
    a [UnsupportedOperationException] should be thrownBy {
      tupletype.typesJava.set(0, booltype)
    }
  }
}