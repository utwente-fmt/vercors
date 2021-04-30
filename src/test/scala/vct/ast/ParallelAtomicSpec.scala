package vct.ast

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import vct.col.ast._
import vct.col.ast.`type`.ClassType
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.{BlockStatement, ParallelAtomic}

class ParallelAtomicSpec extends AnyFlatSpec with Matchers {
  
  "A parallel atomic" should "successfully instantiate with given valid input" in {
    val block = new BlockStatement()
    val synclist = List[ASTNode](new ClassType("A"), new ClassType("B"))
    val pa = new ParallelAtomic(block, synclist)
    
    pa.block should be (block)
    pa.synclist.isEmpty should be (false)
    pa.synclist.count(_ => true) should be (2)
  }
  
  it should "successfully instantiate with given an synclist array as input (instead of a List)" in {
    val block = new BlockStatement()
    val synclist = Array[ASTNode](new ClassType("A"), new ClassType("B"))
    val pa = new ParallelAtomic(block, synclist)
    
    pa.block should be (block)
    pa.synclist.isEmpty should be (false)
    pa.synclist.count(_ => true) should be (2)
  }
  
  it should "successfully instantiate when an empty list is given as input" in {
    val block = new BlockStatement()
    val synclist = List[ASTNode]()
    val pa = new ParallelAtomic(block, synclist)
    pa.synclist.isEmpty should be (true)
  }
  
  it should "successfully instantiate when an empty array is given as input" in {
    val block = new BlockStatement()
    val synclist = Array[ASTNode]()
    val pa = new ParallelAtomic(block, synclist)
    pa.synclist.isEmpty should be (true)
  }
  
  it should "fail to instantiate when an empty list is given as input" in {
    val block = new BlockStatement()
    val synclist : List[ASTNode] = null
    a [IllegalArgumentException] should be thrownBy {
      val pa = new ParallelAtomic(block, synclist)
    }
  }
  
  it should "fail to instantiate when an empty array is given as input" in {
    val block = new BlockStatement()
    val synclist : Array[ASTNode] = null
    a [NullPointerException] should be thrownBy {
      val pa = new ParallelAtomic(block, synclist)
    }
  }
  
  it should "not allow Java clients to alter the list of synchronisation elements" in {
    val block = new BlockStatement()
    val synclist = List[ASTNode](new ClassType("A"))
    val pa = new ParallelAtomic(block, synclist)
    a [UnsupportedOperationException] should be thrownBy {
      pa.synclistJava.add(new ClassType("B"))
    }
  }
}