package vct.rewrite.runtime.util

import vct.col.ast.RewriteHelpers.{RewriteDeref, RewriteVariable}
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.rewrite.runtime.util.AbstractQuantifierRewriter.LoopBodyContent
import vct.rewrite.runtime.util.permissionTransfer._

import scala.Int.{MaxValue, MinValue}
import scala.collection.mutable.ArrayBuffer

object AbstractQuantifierRewriter{

  case class LoopBodyContent[G](expr: Expr[G], quantifier: Expr[G])
}


/**
 * Abstract class for rewriting quantifiers
 * @param pd
 * @param program
 * @tparam Pre
 */
abstract class AbstractQuantifierRewriter[Pre <: Generation](pd: PermissionData[Pre])(implicit program: Program[Pre]) extends Rewriter[Pre] {
  override val allScopes: AllScopes[Pre, Post] = pd.outer.allScopes

  /**
   * Dispatches the loop body, can be overridden by other rewriters
   * @param loopBodyContent
   * @param origin
   * @return
   */
  def dispatchLoopBody(loopBodyContent: LoopBodyContent[Pre])(implicit origin: Origin): Block[Post] = Block[Post](Seq())

  /**
   * Dispatches the quantifier expression
   * @param quantifier
   * @return
   */
  final def dispatchQuantifier(quantifier: Expr[Pre]): Scope[Post] = {
    variables.collectScoped{
      quantifier match {
        case q: Starall[Pre] => dispatchQuantifier(q, q.bindings, q.body)
        case q: Exists[Pre] => dispatchQuantifier(q, q.bindings, q.body)
        case q: Forall[Pre] => dispatchQuantifier(q, q.bindings, q.body)
        case _ => ???
      }
    }._2
  }

  /**
   * Dispatches the quantifier expression, with its bindings and body.
   * It unfolds the body and calls the createQuantifierStatement method to create a new quantifier
   * @param quantifier
   * @param bindings
   * @param body
   * @return
   */
  private final def dispatchQuantifier(quantifier: Expr[Pre], bindings: Seq[Variable[Pre]], body: Expr[Pre]): Scope[Post] = {
    body match {
      case imp: Implies[Pre] => createQuantifierStatement(quantifier, bindings, imp.left, imp.right)
      case and: And[Pre] => createQuantifierStatement(quantifier, bindings, and.left, and.right)
      case _ => ???
    }
  }

  /**
   * createQuantifierStatement creates a new quantifier statement based on the bindings
   * @param expr
   * @param bindings
   * @param left
   * @param right
   * @return
   */
  private final def createQuantifierStatement(expr: Expr[Pre], bindings: Seq[Variable[Pre]], left: Expr[Pre], right: Expr[Pre]): Scope[Post] = {
    implicit val origin: Origin = expr.o
    val newBindings = bindings.map(b => variables.succeed(b, b.rewrite()))
    val bodyLoop = createBodyQuantifier(expr, bindings, left, right)
    Scope(newBindings, bodyLoop)
  }

  /**
   * Creates the quantifier body and starts folding the quantifiers into loops
   * @param expr
   * @param bindings
   * @param left
   * @param right
   * @return
   */
  private final def createBodyQuantifier(expr: Expr[Pre], bindings: Seq[Variable[Pre]], left: Expr[Pre], right: Expr[Pre]): Statement[Post] = {
    implicit val origin: Origin = expr.o
    val bounds: ArrayBuffer[(Variable[Pre], Option[Expr[Pre]], Option[Expr[Pre]])] = FindBoundsQuantifier[Pre](this).findBounds(expr)
    val loopCondition = Branch[Post](Seq((!dispatch(left), Continue[Post](None))))
    val loopOperation: Block[Post] = dispatchLoopBody(LoopBodyContent(right, expr))
    if(loopOperation.statements.isEmpty) {
      return Block[Post](Seq.empty)
    }
    val loopBody : Block[Post] = Block(Seq(loopCondition, loopOperation))
    bindings.reverse.foldLeft[Statement[Post]](loopBody)((acc, element) =>
      createQuantifier(expr, acc, element, bounds.filter(i => i._1 == element))
    )
  }

  /**
   * Creates the quantifier statement using the bounds
   * @param expr
   * @param acc
   * @param element
   * @param filteredBounds
   * @return
   */
  private final def createQuantifier(expr: Expr[Pre], acc: Statement[Post], element: Variable[Pre], filteredBounds: ArrayBuffer[(Variable[Pre], Option[Expr[Pre]], Option[Expr[Pre]])]): Loop[Post] = {
    implicit val origin: Origin = expr.o
    val minValue: Expr[Pre] = filteredBounds.map(i => i._2)
      .collectFirst { case Some(value: Expr[Pre]) => value }
      .getOrElse(const(MinValue))
    val maxValue: Expr[Pre] = filteredBounds.map(i => i._3)
      .collectFirst { case Some(value: Expr[Pre]) => value }
      .getOrElse(const(MaxValue))
    val localBinding = Local[Post](variables.freeze.succ(element))
    Loop[Post](
      Assign[Post](localBinding, dispatch(minValue))(null),
      localBinding < dispatch(maxValue),
      Assign[Post](localBinding, localBinding + const(1))(null),
      LoopInvariant[Post](tt, None)(null),
      acc
    )
  }

  /**
   * Dispatches the expression and changes the dereferences and thisObjects
   * @param e
   * @return
   */
  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = {
    e match {
      case d: Deref[Pre] => getNewExpr(d)
      case t: ThisObject[Pre] => getNewExpr(t)
      case _ => super.dispatch(e)
    }
  }

  /**
   * Creates a new expression for the dereferences and thisObjects
   * @param e
   * @return
   */
  def getNewExpr(e: Expr[Pre]): Expr[Post] = {
    e match {
      case d: Deref[Pre] => d.rewrite(obj = getNewExpr(d.obj))
      case t: ThisObject[Pre] => pd.getOffset(t)
      case _ => dispatch(e)
    }
  }

}
