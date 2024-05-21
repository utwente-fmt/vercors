package vct.col.rewrite

import vct.col.ast._
import vct.col.origin.Origin
import vct.col.ref.Ref
import vct.col.rewrite.TupledQuantifiers.{CollectedBindingsOrigin, vsToString}
import vct.col.origin.OriginContent

import scala.collection.mutable

case object TupledQuantifiers extends RewriterBuilder {
  override def key: String = "tupledQuant"
  override def desc: String = "Collect the bindings of a quantifier into one binding: a tuple"

  case class CollectedBindingsOrigin(vs: Seq[Variable[_]]) extends OriginContent

  def vsToString(vs: Seq[Variable[_]]): String = {
    vs.map(_.o.getPreferredNameOrElse().camel).mkString("_")
  }
}

case class TupledQuantifiers[Pre <: Generation]() extends Rewriter[Pre] {
  val tupledVar: mutable.Map[Variable[Pre], (Variable[Post], Int)] = mutable.Map()

  def collect(bindings: Seq[Variable[Pre]])(implicit o: Origin): Variable[Post] = {
    val collectedBinding = new Variable[Post](TTuple(bindings.map(_.t).map(dispatch)))(
      o.where(name = vsToString(bindings)).withContent(CollectedBindingsOrigin(bindings)))
    for ((binding, i) <- bindings.zipWithIndex) {
      tupledVar(binding) = (collectedBinding, i)
    }
    collectedBinding
  }

  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = {
    implicit val o: Origin = e.o
    e match {
      case Forall(bindings, triggers, body) =>
        Forall(Seq(collect(bindings)), triggers.map(_.map(dispatch)), dispatch(body))

      case Exists(bindings, triggers, body) =>
        Exists(Seq(collect(bindings)), triggers.map(_.map(dispatch)), dispatch(body))

      case s @ Starall(bindings, triggers, body) =>
        Starall(Seq(collect(bindings)), triggers.map(_.map(dispatch)), dispatch(body))(s.blame)

      case Local(Ref(v)) if tupledVar.contains(v) =>
        TupGet(Local(tupledVar(v)._1.ref), tupledVar(v)._2)

      case other => rewriteDefault(other)
    }
  }
}
