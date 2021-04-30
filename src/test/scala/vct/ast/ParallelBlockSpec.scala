package vct.ast

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import vct.col.ast._
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.{BlockStatement, ParallelBlock}
import vct.col.ast.stmt.decl.DeclarationStatement
import vct.col.ast.util.ContractBuilder

class ParallelBlockSpec extends AnyFlatSpec with Matchers {
  
  "A parallel block" should "successfully instantiate with non-null (lists) arguments" in {
    val label = "block1"
    val builder = new ContractBuilder
    val contract = builder.getContract
    val iters = List[DeclarationStatement](new DeclarationStatement("iter1", null))
    val block = new BlockStatement
    val deps = Array[ASTNode](new DeclarationStatement("dep1", null))
    var parblock = new ParallelBlock(label, contract, iters, block, deps)
    
    parblock.iterslength should be (1)
    parblock.depslength should be (1)
  }
  
  it should "successfully instantiate with non-null (array) arguments" in {
    val label = "block1"
    val builder = new ContractBuilder
    val contract = builder.getContract
    val iters = Array[DeclarationStatement](new DeclarationStatement("iter1", null))
    val block = new BlockStatement
    val deps = Array[ASTNode](new DeclarationStatement("dep1", null))
    var parblock = new ParallelBlock(label, contract, iters, block, deps)
    
    parblock.iterslength should be (1)
    parblock.depslength should be (1)
  }
  
  it should "not be able to instantiate when the deps array is given null (with an iters list)" in {
    val label = "block1"
    val builder = new ContractBuilder
    val contract = builder.getContract
    val iters = List[DeclarationStatement](new DeclarationStatement("iter1", null))
    val block = new BlockStatement
    val deps : Array[ASTNode] = null
    
    a [IllegalArgumentException] should be thrownBy {
      var parblock = new ParallelBlock(label, contract, iters, block, deps)
    }
  }
  
  it should "successfully instantiate when the deps array is given null (with an iters array)" in {
    val label = "block1"
    val builder = new ContractBuilder
    val contract = builder.getContract
    val iters = Array[DeclarationStatement](new DeclarationStatement("iter1", null))
    val block = new BlockStatement
    val deps : Array[ASTNode] = null
    
    a [IllegalArgumentException] should be thrownBy {
      var parblock = new ParallelBlock(label, contract, iters, block, deps)
    }
  }
}
