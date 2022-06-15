package vct.col.newrewrite

import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.newrewrite.NameQuantifiers.NamedQuantifierOrigin
import vct.col.newrewrite.ResolveScale.{CheckScale, ScaleNegativePreconditionFailed, WrongScale}
import vct.col.origin.{NoContext, Origin, PanicBlame}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.AstBuildHelpers._

case object NameQuantifiers extends RewriterBuilder {
  override def key: String = "nameQuantifiers"
  override def desc: String = "Gives all quantifiers a specific name so they can be referenced in viper/z3 debug output"

  case class NamedQuantifierOrigin(n: String, o: Origin) extends Origin {
    override def preferredName: String = n
    override def context: String = o.context
    override def inlineContext: String = o.inlineContext
    override def shortPosition: String = o.shortPosition
    override def toString: String = s"$n at $o"
  }
}

case class NameQuantifiers[Pre <: Generation]() extends Rewriter[Pre] {
  var i: Int = 0

  def next(): Int = {
    val x = i
    i += 1
    x
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case s: Starall[Pre] => s.rewrite(o = NamedQuantifierOrigin(s"vq${next()}", s.o))
    case s: Forall[Pre] => s.rewrite(o = NamedQuantifierOrigin(s"vq${next()}", s.o))
    case s: Exists[Pre] => s.rewrite(o = NamedQuantifierOrigin(s"vq${next()}", s.o))
    case _ => rewriteDefault(e)
  }
}
