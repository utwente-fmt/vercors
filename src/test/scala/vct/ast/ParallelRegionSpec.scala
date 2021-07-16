package vct.ast

import org.scalatest._
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite
import vct.col.ast.stmt.composite.{BlockStatement, ParallelBlock, ParallelRegion}
import vct.col.ast.stmt.decl.DeclarationStatement
import vct.col.ast.util.ContractBuilder

class ParallelRegionSpec extends FlatSpec with Matchers {

  "A parallel region" should "instantiate when given valid input" in {
    val builder = new ContractBuilder
    val contract = builder.getContract
    val decs = List[DeclarationStatement](new DeclarationStatement("iter1", null))
    val deps = Array[ASTNode](new DeclarationStatement("dep1", null))
    val blocks = List[ParallelBlock](
      composite.ParallelBlock("B1", builder.getContract, decs, new BlockStatement, deps)
    )

    val region = new ParallelRegion(contract, blocks)
    region.contract should be(contract)
    region.blocks.isEmpty should be(false)
    region.blocks.count(_ => true) should be(1)
  }

  it should "successfully instantiate with an array of blocks" in {
    val builder = new ContractBuilder
    val contract = builder.getContract
    val decs = List[DeclarationStatement](new DeclarationStatement("iter1", null))
    val deps = Array[ASTNode](new DeclarationStatement("dep1", null))
    val blocks = Array[ParallelBlock](
      composite.ParallelBlock("B1", builder.getContract, decs, new BlockStatement, deps)
    )

    val region = new ParallelRegion(contract, blocks)
    region.contract should be(contract)
    region.blocks.isEmpty should be(false)
    region.blocks.count(_ => true) should be(1)
  }

  it should "successfully instantiate via Java constructs" in {
    val builder = new ContractBuilder
    val contract = builder.getContract
    val decs = List[DeclarationStatement](new DeclarationStatement("iter1", null))
    val deps = Array[ASTNode](new DeclarationStatement("dep1", null))

    val blocks = new java.util.ArrayList[ParallelBlock]()
    blocks.add(composite.ParallelBlock("B1", builder.getContract, decs, new BlockStatement, deps))

    val region = new ParallelRegion(contract, blocks)
    region.contract should be(contract)
    region.blocks.isEmpty should be(false)
    region.blocks.count(_ => true) should be(1)
  }

  it should "not instantiate when a null list of blocks is provided" in {
    val builder = new ContractBuilder
    val contract = builder.getContract
    val blocks: List[ParallelBlock] = null
    a[IllegalArgumentException] should be thrownBy {
      val region = new ParallelRegion(contract, blocks)
    }
  }

  it should "not instantiate when a null array of block is provided" in {
    val builder = new ContractBuilder
    val contract = builder.getContract
    val blocks: Array[ParallelBlock] = null
    a[NullPointerException] should be thrownBy {
      val region = new ParallelRegion(contract, blocks)
    }
  }

  it should "now allow changes from Java constructs to propagate to the list of blocks" in {
    val builder = new ContractBuilder
    val contract = builder.getContract
    val decs = List[DeclarationStatement](new DeclarationStatement("iter1", null))
    val deps = Array[ASTNode](new DeclarationStatement("dep1", null))
    val blocks = new java.util.ArrayList[ParallelBlock]()
    blocks.add(composite.ParallelBlock("B1", builder.getContract, decs, new BlockStatement, deps))
    val region = new ParallelRegion(contract, blocks)

    region.blocks.count(_ => true) should be(1)
    blocks.add(composite.ParallelBlock("B2", builder.getContract, decs, new BlockStatement, deps))
    region.blocks.count(_ => true) should be(1)
    blocks.clear
    region.blocks.count(_ => true) should be(1)
  }

  it should "not allow Java clients to alter the list of blocks" in {
    val builder = new ContractBuilder
    val contract = builder.getContract
    val decs = List[DeclarationStatement](new DeclarationStatement("iter1", null))
    val deps = Array[ASTNode](new DeclarationStatement("dep1", null))
    val blocks = List[ParallelBlock](
      composite.ParallelBlock("B1", builder.getContract, decs, new BlockStatement, deps)
    )

    val region = new ParallelRegion(contract, blocks)
    a[UnsupportedOperationException] should be thrownBy {
      region.blocksJava.add(composite.ParallelBlock("B1", builder.getContract, decs, new BlockStatement, deps))
    }
  }
}