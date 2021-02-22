package vct.col.rewrite.gpgpuoptimizations

import vct.col.ast.expr.{NameExpression, OperatorExpression}
import vct.col.ast.expr.StandardOperator._
import vct.col.ast.expr.constant.{ConstantExpression, IntegerValue}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.decl.{GPUOptName, Method, ProgramUnit}
import vct.col.ast.util.AbstractRewriter

import scala.collection.JavaConverters._

case class MatrixLinearization(override val source: ProgramUnit) extends AbstractRewriter(source) {

  private var matrixOpt: (NameExpression, Boolean, ConstantExpression) = null

  override def visit(m: Method): Unit = {
    val maybeOpt = m.getGpuOpts.asScala.find(_.name == GPUOptName.MatrixLinearization)
    if (maybeOpt.isEmpty) {
      super.visit(m)
      return
    }
    val opt = maybeOpt.get
    matrixOpt = (opt.args(0).asInstanceOf[NameExpression], opt.args(1).asInstanceOf[NameExpression].name.equals("R"), opt.args(2).asInstanceOf[ConstantExpression])
    super.visit(m)
    matrixOpt = null
  }

  override def visit(e: OperatorExpression): Unit = {
    if (matrixOpt == null) {
      super.visit(e)
      return
    }
    e.operator match {
      case Subscript => e.first match {
        case o: OperatorExpression if o.operator == Subscript =>
          //TODO how do we deal with 3+ dimensional matrices?
          if (!o.first.isInstanceOf[NameExpression] || !o.first.equals(matrixOpt._1)) {
            super.visit(e)
            return
          }
          val array = rewrite(o.first)

          //TODO
          val (innerI, outerI) = if (matrixOpt._2) (rewrite(e.second), rewrite(o.second)) else (rewrite(o.second), rewrite(e.second))

          val newIndex = create expression(Plus, create expression(Mult, matrixOpt._3, innerI), outerI)
          result = create expression(Subscript, array, newIndex)
        case _ => super.visit(e)
      }
      case _ => super.visit(e)
    }
  }
}