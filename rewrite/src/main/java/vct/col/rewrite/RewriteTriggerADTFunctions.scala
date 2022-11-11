package vct.col.rewrite

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.ref.Ref
import vct.col.util.AstBuildHelpers.VarBuildHelpers

case class RewriteTriggerADTFunctions[Pre <: Generation]() extends Rewriter[Pre]  {

    val inTrigger: ScopedStack[Unit] = ScopedStack()

    override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
      case f @ Forall(_, triggers, _) =>
        f.rewrite(triggers = inTrigger.having(()) {
          triggers.map(_.map(dispatch))
        })

      case s @ Starall(_, triggers, _) =>
        s.rewrite(triggers = inTrigger.having(()) {
          triggers.map(_.map(dispatch))
        })

      case e @ Exists(_, triggers, _) =>
        e.rewrite(triggers = inTrigger.having(()) {
          triggers.map(_.map(dispatch))
        })

      case FunctionInvocation(Ref(func), argsOut, typeArgsOut, Nil, Nil) if inTrigger.nonEmpty =>
        implicit val origin: Origin = e.o
        func.body match {
          case Some(ADTFunctionInvocation(typeArgs, ref, args))
            if func.args.map(_.get) == args && func.typeArgs.map( v => TVar[Pre](v.ref)) == typeArgs.map(_._2).getOrElse(Nil) =>
              ADTFunctionInvocation[Post](
                typeArgs.map(someTypeArgs => (succ(someTypeArgs._1.decl), typeArgsOut.map(dispatch))),
                succ(ref.decl),
                argsOut.map(dispatch))
          case _ => rewriteDefault(e)

        }


      case other => rewriteDefault(other)

    }
}
