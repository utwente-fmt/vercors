package vct.rewrite.runtime.util

import vct.col.ast.RewriteHelpers.RewriteVariable
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter}
import vct.col.util.AstBuildHelpers._
import vct.rewrite.runtime.util.AbstractQuantifierRewriter.LoopBodyContent
import vct.rewrite.runtime.util.permissionTransfer._

import scala.Int.{MaxValue, MinValue}
import scala.collection.mutable.ArrayBuffer

object AbstractQuantifierRewriter{

  case class LoopBodyContent[G](expr: Expr[G], quantifier: Expr[G])
}


abstract class AbstractQuantifierRewriter[Pre <: Generation](pd: PermissionData[Pre])(implicit program: Program[Pre]) extends Rewriter[Pre] {
  override val allScopes: AllScopes[Pre, Post] = pd.outer.allScopes

  def dispatchLoopBody(loopBodyContent: LoopBodyContent[Pre])(implicit origin: Origin): Block[Post] = Block[Post](Seq())

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

  private final def dispatchQuantifier(quantifier: Expr[Pre], bindings: Seq[Variable[Pre]], body: Expr[Pre]): Scope[Post] = {
    body match {
      case imp: Implies[Pre] => createQuantifierStatement(quantifier, bindings, imp.left, imp.right)
      case and: And[Pre] => createQuantifierStatement(quantifier, bindings, and.left, and.right)
      case _ => ???
    }
  }

  private final def createQuantifierStatement(expr: Expr[Pre], bindings: Seq[Variable[Pre]], left: Expr[Pre], right: Expr[Pre]): Scope[Post] = {
    implicit val origin: Origin = expr.o
    val newBindings = bindings.map(b => variables.succeed(b, b.rewrite()))
    val bodyLoop = createBodyQuantifier(expr, bindings, left, right)
    Scope(newBindings, bodyLoop)
  }

  private final def createBodyQuantifier(expr: Expr[Pre], bindings: Seq[Variable[Pre]], left: Expr[Pre], right: Expr[Pre]): Statement[Post] = {
    implicit val origin: Origin = expr.o
    val bounds: ArrayBuffer[(Variable[Pre], Option[Expr[Post]], Option[Expr[Post]])] = FindBoundsQuantifier[Pre](this).findBounds(expr)
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

  private final def createQuantifier(expr: Expr[Pre], acc: Statement[Post], element: Variable[Pre], filteredBounds: ArrayBuffer[(Variable[Pre], Option[Expr[Post]], Option[Expr[Post]])]): Loop[Post] = {
    implicit val origin: Origin = expr.o
    val minValue = filteredBounds.map(i => i._2).collectFirst { case Some(value: Expr[Post]) => value }.getOrElse(IntegerValue[Post](MinValue))
    val maxValue = filteredBounds.map(i => i._3).collectFirst { case Some(value: Expr[Post]) => value }.getOrElse(IntegerValue[Post](MaxValue))
    val localBinding = Local[Post](variables.freeze.succ(element))
    Loop[Post](
      Assign[Post](localBinding, minValue)(null),
      localBinding < maxValue,
      Assign[Post](localBinding, localBinding + const(1))(null),
      LoopInvariant[Post](tt, None)(null),
      acc
    )
  }


}
