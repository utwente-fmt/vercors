package vct.col.rewrite

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.ref.Ref
import vct.col.rewrite.FinalizeArguments.FormalParameterOrigin
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap

case object FinalizeArguments extends RewriterBuilder {
  override def key: String = "finalize"
  override def desc: String = "Make it so the formal parameters of methods are not assigned to."

  case class FormalParameterOrigin(inner: Origin) extends Origin {
    override def preferredName: String = "formal" + inner.preferredName.capitalize
    override def context: String = inner.context
    override def inlineContext: String = inner.inlineContext
    override def shortPosition: String = inner.shortPosition
  }
}

case class FinalizeArguments[Pre <: Generation]() extends Rewriter[Pre] {
  val argLocal: ScopedStack[Map[Variable[Pre], Variable[Post]]] = ScopedStack()

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case method: AbstractMethod[Pre] =>
      val assignedLocals = method.body.map(_.collect {
        case Assign(Local(Ref(v)), _) => v
      }).getOrElse(Nil)

      val argLocals = method.args.intersect(assignedLocals).map(v => v -> new Variable(dispatch(v.t))(v.o)).toMap

      implicit val o: Origin = method.o

      allScopes.anyDeclare(allScopes.anySucceedOnly(method, method.rewrite(
        args = variables.collect {
          method.args.map { v =>
            if(assignedLocals.contains(v)) {
              variables.succeed(v, new Variable(dispatch(v.t))(FormalParameterOrigin(v.o)))
            } else { rewriteDefault(v) }
          }
        }._1,
        body = method.body.map(body => Scope[Post](
          argLocals.values.toSeq,
          Block(Seq(
            Block(argLocals.map { case (formal, local) => assignLocal[Post](local.get, Local(succ(formal))) }.toSeq),
            argLocal.having(argLocals) { dispatch(body) },
          )),
        )),
      )))

    case other => rewriteDefault(other)
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case Local(Ref(v)) if argLocal.nonEmpty && argLocal.top.contains(v) =>
      Local[Post](argLocal.top(v).ref)(e.o)
    case other => rewriteDefault(other)
  }
}
