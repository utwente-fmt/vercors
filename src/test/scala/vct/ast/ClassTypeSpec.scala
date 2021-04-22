package vct.ast

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec

import scala.jdk.CollectionConverters._
import vct.col.ast._
import vct.col.ast.`type`.{ClassType, PrimitiveSort, PrimitiveType}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.decl.ProgramUnit

class ClassTypeSpec extends AnyFlatSpec with Matchers {
 
  "A class type" should "reject definitions without name" in {
    a [IllegalArgumentException] should be thrownBy {
      var classtype = new ClassType(Array[String]())
    }
  }
  
  it should "accept a definition if it has a name" in {
    var classtype = new ClassType(Array("TestClass"))
    classtype.getName should be ("TestClass")
  }
  
  it should "give the last of the names list when queried for it" in {
    var classtype = new ClassType(Array("java", "lang", "object", "TestClass"))
    classtype.getName should be ("TestClass")
  }
  
  it should "give the full class name when queried for it" in {
    var nameparts = Array("java", "lang", "object", "TestClass")
    var classtype = new ClassType(nameparts)
    classtype.getFullName should be ("java.lang.object.TestClass")
  }
  
  it should "give the full list of name parts when queried for it" in {
    var nameparts = Array("java", "lang", "object", "TestClass")
    var classtype = new ClassType(nameparts)
    classtype.getNameFull should be (nameparts)
  }
  
  it should "accept a single name instead of an array of name parts" in {
    var classtype = new ClassType("Test")
    classtype.getName should be ("Test")
  }
  
  it should "give the full name, also if there is only one name part" in {
    var classtype = new ClassType("Test")
    classtype.getFullName should be ("Test")
  }
  
  it should "be immune to changes to a given array of names" in {
    var nameparts = Array("java", "lang", "TestClass")
    var classtype = new ClassType(nameparts)
    nameparts(1) = "object"
    classtype.getFullName should be ("java.lang.TestClass")
  }
  
  it should "have a default label type name <<label>>" in {
    ClassType.labelType.getName should be ("<<label>>")
  }
  
  it should "have a default null type name <<null>>" in {
    ClassType.nullType.getName should be ("<<null>>")
  }

  // These tests should pass, but java.lang.Object as a supertype of everything is not yet supported, see the note at
  // ClassType.supertypeof
  //
  //  it should "always be a supertype of java.lang.Object" in {
  //    var classtype = new ClassType(Array("java", "lang", "Object"))
  //    var someunit = new ProgramUnit
  //    var sometype = new PrimitiveType(PrimitiveSort.Boolean)
  //    classtype.supertypeof(someunit, sometype) should be (true)
  //  }
  //
  //  it should "always be a supertype of java_DOT_lang_DOT_Object" in {
  //    var classtype = new ClassType("java_DOT_lang_DOT_Object")
  //    var someunit = new ProgramUnit
  //    var sometype = new PrimitiveType(PrimitiveSort.Boolean)
  //    classtype.supertypeof(someunit, sometype) should be (true)
  //  }
  
  it should "always be the supertype of the default <<label>> type" in {
    var classtype = new ClassType("TestClass")
    var somecontext = new ProgramUnit
    classtype.supertypeof(somecontext, ClassType.labelType) should be (true)
  }
  
  it should "always be the supertype of the default <<null>> type" in {
    var classtype = new ClassType("TestClass")
    var somecontext = new ProgramUnit
    classtype.supertypeof(somecontext, ClassType.nullType) should be (true)
  }
  
  it should "not be the supertype of any type that is not a class type" in {
    var classtype = new ClassType("TestClass")
    var somecontext = new ProgramUnit
    var sometype = new PrimitiveType(PrimitiveSort.Boolean)
    classtype.supertypeof(somecontext, sometype) should be (false)
  }
  
  it should "be equal to any class type with the same (full) name" in {
    var classtype1 = new ClassType(Array("java", "lang", "TestClass"))
    var classtype2 = new ClassType("java.lang.TestClass")
    classtype1.equals(classtype2) should be (true)
  }
  
  it should "not be equal to any class type with a different name" in {
    var classtype1 = new ClassType(Array("java", "lang", "TestClass"))
    var classtype2 = new ClassType("java.object.TestClass")
    classtype1.equals(classtype2) should be (false)
  }
  
  it should "not be equal to anything that is not a class type" in {
    var classtype1 = new ClassType(Array("java", "lang", "TestClass"))
    var sometype = new PrimitiveType(PrimitiveSort.Boolean)
    classtype1.equals(sometype) should be (false)
  }
  
  it should "return the full type name when calling toString without args" in {
    var nameparts = Array("java", "lang", "TestClass")
    var inttype = new PrimitiveType(PrimitiveSort.Integer)
    var booltype = new PrimitiveType(PrimitiveSort.Boolean)
    var classtype = new ClassType(nameparts, Array[ASTNode](inttype, booltype))
    classtype.toString should be ("java.lang.TestClass<Integer,Boolean>")
  }
}
