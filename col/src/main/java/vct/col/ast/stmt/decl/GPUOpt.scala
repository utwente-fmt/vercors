package vct.col.ast.stmt.decl

import vct.col.ast.expr.NameExpression
import vct.col.ast.expr.constant.{ConstantExpression, IntegerValue}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.decl.Major.Major
import vct.col.ast.stmt.decl.TilingConfig.TilingConfig
import vct.col.ast.util.{ASTMapping, ASTMapping1, ASTVisitor}

import scala.collection.JavaConverters._

object GPUOptFlags extends Enumeration {
    val loopUnrolling =     Value("loop_unroll")
    val matrixLin =         Value("matrix_lin")
    val dataLoc =           Value("glob_to_reg")
    val iterMerge =         Value("iter_merge")
    val tiling =            Value("tile")
    val fusion =            Value("fuse")
}

abstract case class GPUOpt(val args: List[ASTNode]) extends ASTNode {
    require(args != null, "args is null")
    require(!args.exists(_ == null), s"None of the ${args.length} arguments should be null")

    def argsJava = args.asJava

    override def accept_simple[T,A](m:ASTMapping1[T,A], arg:A) = m.map(this, arg)
    override def accept_simple[T](v:ASTVisitor[T]) = v.visit(this)
    override def accept_simple[T](m:ASTMapping[T]) = m.map(this)

    override def debugTreeChildrenFields: Iterable[String] = Seq("args")

    override def debugTreePropertyFields: Iterable[String] = Seq()
}

class LoopUnrolling(val itervar: NameExpression, val K: ConstantExpression)
  extends GPUOpt(List(itervar, K)) {
    require(K.value.isInstanceOf[IntegerValue], "The constant K is not an integer constant")

    def getK: Int = K.value.asInstanceOf[IntegerValue].value.intValue
}

class IterationMerging(val itervar: NameExpression, val M: ConstantExpression)
  extends GPUOpt(List(itervar, M)) {
    require(M.value.isInstanceOf[IntegerValue], "The constant K is not an integer constant")
}

object Major extends Enumeration {
    type Major = Value
    val Row, Column = Value
}

class MatrixLinearization(val matrixName: NameExpression, val rowOrColumn: Major, val dimX: ASTNode, val dimY: ASTNode)
  extends GPUOpt(List(matrixName, dimX, dimY)) {
}

class DataLocation(val arrayName: ASTNode, val locations: List[ASTNode])
  extends GPUOpt(arrayName+: locations) {
    require(locations.nonEmpty, "There must be at least one location")
}

object TilingConfig extends Enumeration {
    type TilingConfig = Value
    val Inter, Intra = Value
}


class Tiling(val interOrIntra: TilingConfig, val tileSize: ConstantExpression)
  extends GPUOpt(List(tileSize)) {
    require(tileSize.value.isInstanceOf[IntegerValue], "The tilesize is not an integer constant")

    val tileSizeInt: Int = tileSize.value.asInstanceOf[IntegerValue].value.intValue
}

class KernelFusion(val F: ConstantExpression, val N: ConstantExpression)
  extends GPUOpt(List(F, N)) {
    require(F.value.isInstanceOf[IntegerValue], "The constant F is not an integer constant")
    require(N.value.isInstanceOf[IntegerValue], "The constant N is not an integer constant")
}

class HostSync(val F: ConstantExpression)
  extends GPUOpt(List(F)) {
    require(F.value.isInstanceOf[IntegerValue], "The constant F is not an integer constant")
}