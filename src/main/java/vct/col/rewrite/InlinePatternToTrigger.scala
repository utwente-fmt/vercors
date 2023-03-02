package vct.col.rewrite

import vct.col.ast.expr.{BindingExpression, InlineQuantifierPattern, SetComprehension}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.decl.ProgramUnit
import vct.col.ast.util.AbstractRewriter

import scala.collection.mutable.ArrayBuffer

class InlinePatternToTrigger(source: ProgramUnit) extends AbstractRewriter(source) {
  var patternStack: ArrayBuffer[ArrayBuffer[ASTNode]] = ArrayBuffer()
  // Add a bottom element, to support floating patterns
  patternStack += new ArrayBuffer()

  override def visit(pattern: InlineQuantifierPattern): Unit = {
    result = rewrite(pattern.inner)

    /* Parallel block contracts are propagated in quantified and un-quantified form, so floating patterns are allowed. */
    patternStack(patternStack.size - 1) += result
  }

  override def visit(quantifier: BindingExpression): Unit = {
    patternStack += new ArrayBuffer()

    val select = rewrite(quantifier.select)
    val main = rewrite(quantifier.main)

    val patterns = patternStack.remove(patternStack.size - 1)

    val triggers = (Option(quantifier.triggers) match {
      case None => Array[Array[ASTNode]]()
      case Some(_) => rewrite(quantifier.javaTriggers)
    }) ++ (if(patterns.nonEmpty) Array(patterns.toArray) else Array.empty[Array[ASTNode]])

    quantifier match {
      case comprehension: SetComprehension =>
        result = create setComp(
          rewrite(quantifier.result_type),
          select,
          main,
          rewrite(comprehension.variables),
          quantifier.getDeclarations)
      case _ =>
        result = create binder(
          quantifier.binder,
          rewrite(quantifier.result_type),
          rewrite(quantifier.getDeclarations),
          triggers,
          select,
          main)
    }
  }
}
