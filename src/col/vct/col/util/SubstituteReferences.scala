package vct.col.util

import vct.col.ast._
import vct.col.origin.Origin
import vct.col.ref.{DirectRef, Ref}
import vct.col.rewrite.NonLatchingRewriter

import scala.reflect.ClassTag

/** Substitute all references in expressions, resulting AST can be used for
  * analysis but not output since it doesn't contain the right declarations
  */
case class SubstituteReferences[G](subs: Map[Object, Object])
    extends NonLatchingRewriter[G, G] {

  case class SuccOrIdentity() extends SuccessorsProviderTrafo[G, G](allScopes) {
    override def postTransform[T <: Declaration[G]](
        pre: Declaration[G],
        post: Option[T],
    ): Option[T] = Some(post.getOrElse(pre.asInstanceOf[T]))
  }

  override def succProvider: SuccessorsProvider[G, G] = SuccOrIdentity()

  private def substitute[T <: Declaration[G]](obj: T)(
      implicit tag: ClassTag[T]
  ): DirectRef[G, T] = new DirectRef(subs.getOrElse(obj, obj).asInstanceOf[T])

  override def dispatch(e: Expr[G]): Expr[G] = {
    implicit val o: Origin = e.o
    e match {
      // Matching on everything with a reference in it
      case Local(Ref(v)) => Local(substitute(v))
      case HeapLocal(Ref(v)) => HeapLocal(substitute(v))
      case EnumUse(Ref(a), Ref(b)) => EnumUse(substitute(a), substitute(b))
      case deref @ DerefHeapVariable(Ref(v)) =>
        DerefHeapVariable[G](substitute(v))(deref.blame)
      case deref @ Deref(obj, Ref(f)) =>
        Deref[G](dispatch(obj), substitute(f))(deref.blame)
      case deref @ ModelDeref(obj, Ref(f)) =>
        ModelDeref[G](dispatch(obj), substitute(f))(deref.blame)
      case FunctionOf(Ref(b), vars) =>
        FunctionOf[G](substitute(b), vars.map { case Ref(v) => substitute(v) })
      case NewObject(Ref(c)) => NewObject(substitute(c))
      case old @ Old(expr, None) => Old(dispatch(expr), None)(old.blame)
      case old @ Old(expr, Some(Ref(l))) =>
        Old[G](dispatch(expr), Some(substitute(l)))(old.blame)
      case ProcessApply(Ref(p), args) =>
        ProcessApply(substitute(p), args.map(dispatch))
      case EndpointName(Ref(e)) => EndpointName(substitute(e))
      case ChorPerm(Ref(e), loc, perm) =>
        ChorPerm(substitute(e), dispatch(loc), dispatch(perm))
      case Sender(Ref(s)) => Sender(substitute(s))
      case Receiver(Ref(r)) => Receiver(substitute(r))
      case Message(Ref(m)) => Message(substitute(m))
      case _ => e.rewriteDefault()
    }
  }
}
