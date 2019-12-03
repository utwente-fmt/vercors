package vct.ast

import org.scalatest._
import vct.col.ast._
import vct.col.ast.`type`.ClassType
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.ActionBlock

class ActionBlockSpec extends FlatSpec with Matchers {
  
  "An action block" should "successfully instantiate when given valid input" in {
    val hist = new ClassType("Some history")
    val frac = new ClassType("Some fraction")
    val proc = new ClassType("Some process")
    val act = new ClassType("Some action")
    val block = new ClassType("Some block")
    val map = Map[String,ASTNode](
      "a" -> new ClassType("a"),
      "b" -> new ClassType("b")
    )
    
    val ab = new ActionBlock(hist, frac, proc, act, map, block)
    ab.map.isEmpty should be (false)
    ab.map.count(_ => true) should be (2)
  }
  
  it should "successfully instantiate when given a java map as input" in {
    val hist = new ClassType("Some history")
    val frac = new ClassType("Some fraction")
    val proc = new ClassType("Some process")
    val act = new ClassType("Some action")
    val block = new ClassType("Some block")
    val map = new java.util.HashMap[String,ASTNode]()
    
    map.put("a", new ClassType("a"))
    map.put("b", new ClassType("b"))
    
    val ab = new ActionBlock(hist, frac, proc, act, map, block)
    ab.map.isEmpty should be (false)
    ab.map.count(_ => true) should be (2)
  }
  
  it should "not construct when the input map is null" in {
    val hist = new ClassType("Some history")
    val frac = new ClassType("Some fraction")
    val proc = new ClassType("Some process")
    val act = new ClassType("Some action")
    val block = new ClassType("Some block")
    val map : Map[String,ASTNode] = null
    
    a [IllegalArgumentException] should be thrownBy {
      val ab = new ActionBlock(hist, frac, proc, act, map, block)
    }
  }
  
  it should "not construct when the input hash table is null" in {
    val hist = new ClassType("Some history")
    val frac = new ClassType("Some fraction")
    val proc = new ClassType("Some process")
    val act = new ClassType("Some action")
    val block = new ClassType("Some block")
    val map : java.util.HashMap[String,ASTNode] = null
    
    a [NullPointerException] should be thrownBy {
      val ab = new ActionBlock(hist, frac, proc, act, map, block)
    }
  }
  
  it should "be immune to hash table additions" in {
    val hist = new ClassType("Some history")
    val frac = new ClassType("Some fraction")
    val proc = new ClassType("Some process")
    val act = new ClassType("Some action")
    val block = new ClassType("Some block")
    val map = new java.util.HashMap[String,ASTNode]()
    val ab = new ActionBlock(hist, frac, proc, act, map, block)
    
    ab.map.isEmpty should be (true)
    map.put("a", new ClassType("Some action"))
    ab.map.isEmpty should be (true)
  }
  
  it should "be immune to hash table deletions" in {
    val hist = new ClassType("Some history")
    val frac = new ClassType("Some fraction")
    val proc = new ClassType("Some process")
    val act = new ClassType("Some action")
    val block = new ClassType("Some block")
    val map = new java.util.HashMap[String,ASTNode]()
    map.put("a", new ClassType("Some action"))
    
    val ab = new ActionBlock(hist, frac, proc, act, map, block)
    ab.map.count(_ => true) should be (1)
    map.remove("a")
    ab.map.count(_ => true) should be (1)
  }
}