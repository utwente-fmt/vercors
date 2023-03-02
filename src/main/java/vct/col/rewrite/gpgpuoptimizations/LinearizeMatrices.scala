package vct.col.rewrite.gpgpuoptimizations

import vct.col.ast.`type`.{PrimitiveSort, PrimitiveType, Type}
import vct.col.ast.expr.{NameExpression, OperatorExpression, StandardOperator}
import vct.col.ast.expr.StandardOperator._
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.decl.{DeclarationStatement, Major, MatrixLinearization, Method, ProgramUnit}
import vct.col.ast.util.{AbstractRewriter, SequenceUtils}

import scala.collection.JavaConverters._

case class LinearizeMatrices(override val source: ProgramUnit) extends AbstractRewriter(source) {

  private var matrixOpts: Seq[MatrixLinearization] = Seq.empty

  private var inDecl = false;

  private val prefix = "vct_ml_"

  private val idxname = prefix + "Idx"

  def addIdx = {
    if (currentTargetClass.find_predicate(idxname) == null) {

      val args: Seq[DeclarationStatement] = Seq(
        create.field_decl(prefix + "X", create.primitive_type(PrimitiveSort.Integer)),
        create.field_decl(prefix + "a", create.primitive_type(PrimitiveSort.Integer)),
        create.field_decl(prefix + "b", create.primitive_type(PrimitiveSort.Integer))
      )

      val body: ASTNode = create.expression(StandardOperator.Plus,
        create.expression(StandardOperator.Mult, create.argument_name(prefix + "X"), create.argument_name(prefix + "a")),
        create.argument_name(prefix + "b")
      )
      val result = create.function_decl(
        create.primitive_type(PrimitiveSort.Integer),
        null,
        idxname,
        args.toArray,
        body
      )
      result.setStatic(true)

      currentTargetClass.add(result)
    }
  }



  override def visit(m: Method): Unit = {
    val opts = m.getGpuOpts.asScala.filter(_.isInstanceOf[MatrixLinearization]).toList
    if (opts.isEmpty) {
      super.visit(m)
      return
    }

    addIdx
    matrixOpts = opts.map(_.asInstanceOf[MatrixLinearization])

    super.visit(m)

    matrixOpts = Seq.empty
  }

  override def visit(e: OperatorExpression): Unit = {
    if (matrixOpts.isEmpty) {
      super.visit(e)
      return
    }
    e.operator match {
      case Subscript => e.first match {
        case o: OperatorExpression if o.operator == Subscript =>
          if (!o.first.isInstanceOf[NameExpression] || !matrixOpts.exists(_.matrixName.equals(o.first))) {
            super.visit(e)
            return
          }
          val array = rewrite(o.first)

          val matrixOpt = matrixOpts.find(_.matrixName.equals(o.first)).get

          val (innerI, outerI) = if (Major.Row.eq(matrixOpt.rowOrColumn)) (rewrite(e.second), rewrite(o.second)) else (rewrite(o.second), rewrite(e.second))
          val dimension = if (Major.Row.eq(matrixOpt.rowOrColumn)) matrixOpt.dimY else matrixOpt.dimX

          val newIndex = create.invokation(null, null, idxname, dimension, outerI, innerI)


          result = create expression(Subscript, array, newIndex)
        case _ => super.visit(e)
      }
      case NewArray if e.args.length > 2 => {
        if (!inDecl) {
          super.visit(e)
          return
        }
        val matrix = SequenceUtils.getTypeInfo(e.first.asInstanceOf[Type])
        if (matrix == null) {
          super.visit(e)
          return
        }
        val array = SequenceUtils.getTypeInfo(matrix.getElementType)
        if (array == null) {
          super.visit(e)
          return
        }
        val elementType = array.getElementType
        result = create.expression(NewArray, SequenceUtils.optArrayCell(create, elementType), create.expression(Mult, rewrite(e.arg(1)), rewrite(e.arg(2))))
      }
      case ValidMatrix => {
        result = create.expression(ValidArray, rewrite(e.arg(0)), create.expression(Mult, rewrite(e.arg(1)), rewrite(e.arg(2))))
      }
      case _ => super.visit(e)
    }
  }


  override def visit(s: DeclarationStatement): Unit = {
    if (matrixOpts.exists(_.matrixName.name.equals(s.name))) {
      inDecl = true;
    }
    super.visit(s)
    inDecl = false;
  }

  override def visit(t: PrimitiveType): Unit = {
    if (matrixOpts.isEmpty || !inDecl) {
      super.visit(t)
      return
    }

    val matrix = SequenceUtils.getTypeInfo(t)
     if (matrix == null) {
       super.visit(t)
       return
     }
    val array = SequenceUtils.getTypeInfo(matrix.getElementType)
    if (array == null) {
      super.visit(t)
      return
    }
    if (matrix.isOpt && matrix.getSequenceSort == PrimitiveSort.Array && array.getSequenceSort == PrimitiveSort.Array && array.isCell) {
      val elementType = array.getElementType
      result = SequenceUtils.optArrayCell(create, elementType)
      } else {
        super.visit(t)
    }
  }


}