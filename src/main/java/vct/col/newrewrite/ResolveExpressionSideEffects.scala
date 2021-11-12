package vct.col.newrewrite

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.util.AstBuildHelpers._
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.Rewriter

import scala.collection.mutable.ArrayBuffer

case object ResolveExpressionSideEffects {
  case object SideEffectOrigin extends Origin {
    override def preferredName: String = "flatten"
    override def messageInContext(message: String): String =
      s"[At node generated to collect side effects]: $message"
  }
}

case class ResolveExpressionSideEffects() extends Rewriter {
  import ResolveExpressionSideEffects._

  val executionContext: ScopedStack[Statement => Unit] = ScopedStack()

  def evaluateOne(e: Expr): (Seq[Variable], Seq[Statement], Expr) = {
    val statements = ArrayBuffer[Statement]()
    variableScopes.push(ArrayBuffer[Variable]())

    val result = executionContext.having(statements.append) {
      dispatch(e)
    }

    (variableScopes.pop().toSeq, statements.toSeq, result)
  }

  def evaluateAll(es: Seq[Expr]): (Seq[Variable], Seq[Statement], Seq[Expr]) = {
    implicit val o: Origin = SideEffectOrigin
    /* PB: We do a bit of a hack here, and place all the expressions we want to evaluate into one tuple. This ensures
       that we don't have to think about manually joining the side effects of two subsequent evaluations. For example,
       given that expression #1 has no side effects that occur after evaluation, we may use it immediately without
       storing it in an intermediate variable. However, if expression #2 has side effects (before or after evaluation),
       we have to store it in an intermediate variable after all. A tuple encodes exactly this process for us, and so
       we do not duplicate the logic here.
     */
    val (variables, sideEffects, result) = evaluateOne(LiteralTuple(Nil, es))
    (variables, sideEffects, result.asInstanceOf[LiteralTuple].values)
  }

  def frameAll(exprs: Seq[Expr], make: Seq[Expr] => Statement): Statement = {
    implicit val o: Origin = SideEffectOrigin
    val (variables, sideEffects, result) = evaluateAll(exprs)
    val statement = make(result)

    val allStements =
      if(sideEffects.isEmpty) statement
      else Block(sideEffects :+ statement)

    if(variables.isEmpty) allStements
    else Scope(variables, allStements)
  }

  def frame(expr: Expr, make: Expr => Statement): Statement =
    frameAll(Seq(expr), es => make(es.head))
  def frame(e1: Expr, e2: Expr, make: (Expr, Expr) => Statement): Statement =
    frameAll(Seq(e1, e2), es => make(es(0), es(1)))

  def doBranches(branches: Seq[(Expr, Statement)])(implicit o: Origin): Statement =
    branches match {
      case Nil => Branch(Nil)
      case (cond, impl) :: tail =>
        doBranches(tail) match {
          case Branch(branches) =>
            frame(cond, cond => Branch((cond, dispatch(impl)) +: branches))
          case otherwise =>
            frame(cond, cond => Branch(Seq( (cond, dispatch(impl)), (tt, otherwise) )))
        }
    }

  override def dispatch(stat: Statement): Statement = {
    implicit val o: Origin = stat.o
    stat match {
      case Eval(e) => frame(e, Eval(_))
      case decl: LocalDecl => rewriteDefault(decl)
      case Return(result) => frame(result, Return(_))
      case Assign(target, value) => frame(target, value, Assign(_, _))
      case block: Block => rewriteDefault(block)
      case scope: Scope => rewriteDefault(scope)
      case Branch(branches) => doBranches(branches)
      case Switch(expr, body) => frame(expr, Switch(_, dispatch(body)))
      case Loop(init, cond, update, contract, body) =>
        evaluateOne(cond) match {
          case (Nil, Nil, cond) =>
            Loop(dispatch(init), cond, dispatch(update), dispatch(contract), dispatch(body))
          case (variables, sideEffects, cond) =>
            Scope(variables, Loop(
              init = Block(dispatch(init) +: sideEffects),
              cond = cond,
              update = Block(dispatch(update) +: sideEffects),
              contract = dispatch(contract),
              body = dispatch(body),
            ))
        }
      case attempt: TryCatchFinally => rewriteDefault(attempt)
      case Synchronized(obj, body) => frame(obj, Synchronized(_, dispatch(body)))
      case inv: ParInvariant => rewriteDefault(inv)
      case atomic: ParAtomic => rewriteDefault(atomic)
      case barrier: ParBarrier => rewriteDefault(barrier)
      case region: ParRegion => rewriteDefault(region)
      case vec: VecBlock => rewriteDefault(vec) // PB: conceivably we can support side effect in iterator ranges; let's see if someone wants that :)
      case send: Send => rewriteDefault(send)
      case recv: Recv => rewriteDefault(recv)
      case default: DefaultCase => rewriteDefault(default)
      case Case(pattern) => Case(dispatch(pattern))
      case label: Label => rewriteDefault(label)
      case goto: Goto => rewriteDefault(goto)
      case exhale: Exhale => rewriteDefault(exhale)
      case assert: Assert => rewriteDefault(assert)
      case refute: Refute => rewriteDefault(refute)
      case inhale: Inhale => rewriteDefault(inhale)
      case assume: Assume => rewriteDefault(assume)
      case ignore: SpecIgnoreStart => rewriteDefault(ignore)
      case ignore: SpecIgnoreEnd => rewriteDefault(ignore)
      case Throw(obj) => frame(obj, Throw(_))
      case Wait(obj) => frame(obj, Wait(_))
      case Notify(obj) => frame(obj, Notify(_))
      case Fork(obj) => frame(obj, Fork(_))
      case Join(obj) => frame(obj, Join(_))
      case Lock(obj) => frame(obj, Lock(_))
      case Unlock(obj) => frame(obj, Unlock(_))
      case fold: Fold => rewriteDefault(fold)
      case unfold: Unfold => rewriteDefault(unfold)
      case create: WandCreate => rewriteDefault(create)
      case qed: WandQed => rewriteDefault(qed)
      case apply: WandApply => rewriteDefault(apply)
      case use: WandUse => rewriteDefault(use)
      case modelDo: ModelDo => rewriteDefault(modelDo)
      case havoc: Havoc => rewriteDefault(havoc) // PB: pretty sure you can only havoc locals?
      case break: Break => rewriteDefault(break)
      case continue: Continue => rewriteDefault(continue)
    }
  }
}
