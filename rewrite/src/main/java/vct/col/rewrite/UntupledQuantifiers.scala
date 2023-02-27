package vct.col.rewrite

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.ref.Ref
import vct.col.util.AstBuildHelpers._

import scala.collection.mutable

case object UntupledQuantifiers extends RewriterBuilder {
  override def key: String = "untupledQuant"
  override def desc: String = "Expand out bindings in quantifiers, such as tuples"
}

case class UntupledQuantifiers[Pre <: Generation]() extends Rewriter[Pre] {
  val bindingValue: ScopedStack[Map[Variable[Pre], Expr[Post]]] = ScopedStack()

  def expand(bindings: Seq[Variable[Pre]],
             builder: Seq[Variable[Post]] => Expr[Post],
             joiner: (Expr[Post], Expr[Post]) => Expr[Post],
             initBindings: Seq[Variable[Post]] = Nil)(implicit o: Origin): Expr[Post] = {

    if(bindings.isEmpty) return builder(initBindings)

    bindings.head.t match {
      case TBool() => joiner(
        bindingValue.having(Map(bindings.head -> ff)) { expand(bindings.tail, builder, joiner, initBindings) },
        bindingValue.having(Map(bindings.head -> tt)) { expand(bindings.tail, builder, joiner, initBindings) },
      )

      case TTuple(preTs) =>
        val ts = preTs.map(dispatch)
        val vs = ts.zipWithIndex.map { case (t, i) => new Variable[Post](t)(bindings.head.o match {
          case TupledQuantifiers.CollectedBindingsOrigin(vs, _) => vs(i).o
          case _ => o
        }) }
        val value = LiteralTuple[Post](ts, vs.map(_.get))
        bindingValue.having(Map(bindings.head -> value)) { expand(bindings.tail, builder, joiner, initBindings ++ vs) }

      case other =>
        val v = new Variable[Post](dispatch(other))
        bindingValue.having(Map(bindings.head -> v.get)) { expand(bindings.tail, builder, joiner, initBindings :+ v) }
    }
  }


  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = {
    implicit val o: Origin = e.o
    e match {
      case Forall(bindings, triggers, body) =>
        expand(bindings, bindings => Forall(bindings, triggers.map(_.map(dispatch)), dispatch(body)), And(_, _))
      case Exists(bindings, triggers, body) =>
        expand(bindings, bindings => Exists(bindings, triggers.map(_.map(dispatch)), dispatch(body)), Or(_, _))
      case s @ Starall(bindings, triggers, body) =>
        expand(bindings, bindings => Starall(bindings, triggers.map(_.map(dispatch)), dispatch(body))(s.blame), Star(_, _))

      case TupGet(v, i) => dispatch(v) match {
        case LiteralTuple(_, vs) => vs(i)
        case other => TupGet(other, i)
      }

      case Local(Ref(v)) if bindingValue.exists(_.contains(v)) =>
        bindingValue.find(_.contains(v)).get(v)

      case other => rewriteDefault(other)
    }
  }
}
