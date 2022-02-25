package vct.col.newrewrite

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.util.AstBuildHelpers._
import vct.col.ast._
import vct.col.ast.expr.MethodInvokation
import vct.col.newrewrite.error.ExtraNode
import vct.col.origin.{DerefAssignTarget, Origin, SubscriptAssignTarget}
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, NonLatchingRewriter, Rewriter, RewriterBuilder}
import vct.result.VerificationResult.{Unreachable, UserError}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

case object ResolveExpressionSideEffects extends RewriterBuilder {
  case object SideEffectOrigin extends Origin {
    override def preferredName: String = "flatten"
    override def context: String = "[At node generated to collect side effects]"
  }

  case object ResultVar extends Origin {
    override def preferredName: String = "res"
    override def context: String = "[At node generated to contain the result of a method]"
  }

  case class DisallowedSideEffect(effector: Expr[_]) extends UserError {
    override def code: String = "sideEffect"
    override def text: String =
      effector.o.messageInContext("This expression may have side effects, but it is in a position where that is not allowed.")
  }
}

case class ResolveExpressionSideEffects[Pre <: Generation]() extends Rewriter[Pre] {
  import ResolveExpressionSideEffects._

  val currentResultVar: ScopedStack[Local[Post]] = ScopedStack()

  // executionContext contains an acceptor of statements, if side effects in expressions currently have a logical
  // place to be put.
  val executionContext: ScopedStack[Statement[Post] => Unit] = ScopedStack()

  def inPure: Boolean = executionContext.isEmpty

  // All expressions are in principle extracted:
  // 1 + 2 --> flat1 = 1; flat2 = 2; flat3 = flat1 + flat2; flat3
  // However, extractions are first stored in this map, and are only flushed to the executionContext if an actual
  // side effect occurs. If not, they are removed from the map and inlined again.
  val currentlyExtracted: mutable.Map[Variable[Post], (Seq[Expr[Post]], Expr[Post])] = mutable.Map()

  // conditions may be duplicated, so they have to be duplicable for free probably? i.e. no internal declarations
  // like let.
  val currentConditions: ScopedStack[Expr[Post]] = ScopedStack()

  // When an actual side effect occurs, this flushes out the extracted pure expressions as side effects.
  def flushExtractedExpressions(): Unit = {
    currentlyExtracted.foreach {
      case (flat, (condition, pureExpr)) => executionContext.topOption match {
        case None => throw Unreachable("flushExtractedExpressions is not called from pure context.")
        case Some(acceptor) =>
          flat.declareDefault(this)
          implicit val o: Origin = SideEffectOrigin
          acceptor(Branch(Seq((
            foldAnd(condition),
            assignLocal(Local(flat.ref), pureExpr),
          ))))
      }
    }
    currentlyExtracted.clear()
  }

  def effect(stat: Statement[Post]): Unit = {
    flushExtractedExpressions()
    implicit val o: Origin = SideEffectOrigin
    executionContext.top(Branch(Seq((foldAnd(currentConditions.toSeq), stat))))
  }

  case class ReInliner() extends NonLatchingRewriter[Post, Post] {
    // ReInliner does not latch declarations ...
    override def succ[DPost <: Declaration[Post]](ref: Ref[Post, _ <: Declaration[Post]])(implicit tag: ClassTag[DPost]): Ref[Post, DPost] =
      ref.asInstanceOf[Ref[Post, DPost]]

    // ... but since we need to be able to unpack locals, we latch those, and they are not latched in
    // ResolveExpressionSideEffects.
    override def dispatch(e: Expr[Post]): Expr[Post] = e match {
      case Local(Ref(v)) =>
        currentlyExtracted.remove(v) match {
          case Some((_, pureExpression)) =>
            pureExpression
          case None =>
            val preV = (v: Variable[Post]).asInstanceOf[Variable[Pre]]
            Local[Post](ResolveExpressionSideEffects.this.succ(preV))(e.o)
        }
      case other => rewriteDefault(other)
    }
  }

  def evaluateOne(e: Expr[Pre]): (Seq[Variable[Post]], Seq[Statement[Post]], Expr[Post]) = {
    val statements = ArrayBuffer[Statement[Post]]()
    variableScopes.push(ArrayBuffer[Variable[Post]]())

    val result = executionContext.having(statements.append) {
      ReInliner().dispatch(dispatch(e))
    }

    assert(currentlyExtracted.isEmpty)

    (variableScopes.pop().toSeq, statements.toSeq, result)
  }

  def evaluateAll(es: Seq[Expr[Pre]]): (Seq[Variable[Post]], Seq[Statement[Post]], Seq[Expr[Post]]) = {
    implicit val o: Origin = SideEffectOrigin
    /* PB: We do a bit of a hack here, and place all the expressions we want to evaluate into one tuple. This ensures
       that we don't have to think about manually joining the side effects of two subsequent evaluations. For example,
       given that expression #1 has no side effects that occur after evaluation, we may use it immediately without
       storing it in an intermediate variable. However, if expression #2 has side effects (before or after evaluation),
       we have to store it in an intermediate variable after all. A tuple encodes exactly this process for us, and so
       we do not duplicate the logic here.
     */
    val (variables, sideEffects, result) = evaluateOne(LiteralTuple(Nil, es))
    (variables, sideEffects, result.asInstanceOf[LiteralTuple[Post]].values)
  }

  def frameAll(exprs: Seq[Expr[Pre]], make: Seq[Expr[Post]] => Statement[Post]): Statement[Post] = {
    implicit val o: Origin = SideEffectOrigin
    val (variables, sideEffects, result) = evaluateAll(exprs)
    val statement = make(result)

    val allStements =
      if(sideEffects.isEmpty) statement
      else Block(sideEffects :+ statement)

    if(variables.isEmpty) allStements
    else Scope(variables, allStements)
  }

  def frame(expr: Expr[Pre], make: Expr[Post] => Statement[Post]): Statement[Post] =
    frameAll(Seq(expr), es => make(es.head))
  def frame(e1: Expr[Pre], e2: Expr[Pre], make: (Expr[Post], Expr[Post]) => Statement[Post]): Statement[Post] =
    frameAll(Seq(e1, e2), es => make(es(0), es(1)))

  def doBranches(branches: Seq[(Expr[Pre], Statement[Pre])])(implicit o: Origin): Statement[Post] =
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

  override def dispatch(stat: Statement[Pre]): Statement[Post] = {
    implicit val o: Origin = stat.o
    stat match {
      case Eval(e) => frame(e, Eval(_))
      case inv @ InvokeMethod(obj, Ref(method), args, outArgs, typeArgs, givenMap, yields) =>
        frameAll(obj +: args, {
          case obj :: args => InvokeMethod[Post](obj, succ(method), args, outArgs.map(succ[Variable[Post]]), typeArgs.map(dispatch),
            givenMap.map { case (Ref(v), e) => (succ(v), dispatch(e)) },
            yields.map { case (Ref(e), Ref(v)) => (succ(e), succ(v)) })(inv.blame)
        })
      case inv @ InvokeProcedure(Ref(method), args, outArgs, typeArgs, givenMap, yields) =>
        frameAll(args, args =>
          InvokeProcedure[Post](succ(method), args, outArgs.map(succ[Variable[Post]]), typeArgs.map(dispatch),
            givenMap.map { case (Ref(v), e) => (succ(v), dispatch(e)) },
            yields.map { case (Ref(e), Ref(v)) => (succ(e), succ(v)) })(inv.blame))
      case decl: LocalDecl[Pre] => rewriteDefault(decl)
      case Return(result) =>
        frame(result, e => Block(Seq(
          assignLocal(currentResultVar.top, e),
          Return(Void()),
        )))
      case ass @ Assign(target, value) => frame(target, value, Assign(_, _)(ass.blame))
      case block: Block[Pre] => rewriteDefault(block)
      case scope: Scope[Pre] => rewriteDefault(scope)
      case Branch(branches) => doBranches(branches)
      case Switch(expr, body) => frame(expr, Switch(_, dispatch(body)))
      case loop @ Loop(init, cond, update, contract, body) =>
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
      case attempt: TryCatchFinally[Pre] => rewriteDefault(attempt)
      case sync @ Synchronized(obj, body) => frame(obj, Synchronized(_, dispatch(body))(sync.blame))
      case inv: ParInvariant[Pre] => rewriteDefault(inv)
      case atomic: ParAtomic[Pre] => rewriteDefault(atomic)
      case barrier: ParBarrier[Pre] => rewriteDefault(barrier)
      case vec: VecBlock[Pre] => rewriteDefault(vec) // PB: conceivably we can support side effect in iterator ranges; let's see if someone wants that :)
      case send: Send[Pre] => rewriteDefault(send)
      case recv: Recv[Pre] => rewriteDefault(recv)
      case default: DefaultCase[Pre] => rewriteDefault(default)
      case Case(pattern) => Case(dispatch(pattern))
      case label: Label[Pre] => rewriteDefault(label)
      case goto: Goto[Pre] => rewriteDefault(goto)
      case exhale: Exhale[Pre] => rewriteDefault(exhale)
      case assert: Assert[Pre] => rewriteDefault(assert)
      case refute: Refute[Pre] => rewriteDefault(refute)
      case inhale: Inhale[Pre] => rewriteDefault(inhale)
      case assume: Assume[Pre] => rewriteDefault(assume)
      case ignore: SpecIgnoreStart[Pre] => rewriteDefault(ignore)
      case ignore: SpecIgnoreEnd[Pre] => rewriteDefault(ignore)
      case t @ Throw(obj) => frame(obj, Throw(_)(t.blame))
      case wait @ Wait(obj) => frame(obj, Wait(_)(wait.blame))
      case notify @ Notify(obj) => frame(obj, Notify(_)(notify.blame))
      case Fork(obj) => frame(obj, Fork(_))
      case Join(obj) => frame(obj, Join(_))
      case Lock(obj) => frame(obj, Lock(_))
      case unlock @ Unlock(obj) => frame(obj, Unlock(_)(unlock.blame))
      case fold: Fold[Pre] => rewriteDefault(fold)
      case unfold: Unfold[Pre] => rewriteDefault(unfold)
      case create: WandCreate[Pre] => rewriteDefault(create)
      case qed: WandQed[Pre] => rewriteDefault(qed)
      case apply: WandApply[Pre] => rewriteDefault(apply)
      case use: WandUse[Pre] => rewriteDefault(use)
      case modelDo: ModelDo[Pre] => rewriteDefault(modelDo)
      case havoc: Havoc[Pre] => rewriteDefault(havoc) // PB: pretty sure you can only havoc locals?
      case break: Break[Pre] => rewriteDefault(break)
      case continue: Continue[Pre] => rewriteDefault(continue)
      case commit: Commit[Pre] => rewriteDefault(commit)
      case par: ParStatement[Pre] => rewriteDefault(par)
      case n: SilverNewRef[Pre] => rewriteDefault(n)
      case assn: SilverFieldAssign[Pre] => rewriteDefault(assn)
      case assn: SilverLocalAssign[Pre] => rewriteDefault(assn)
      case _: CStatement[Pre] => throw ExtraNode
      case _: JavaStatement[Pre] => throw ExtraNode
    }
  }

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case method: AbstractMethod[Pre] =>
      val res = new Variable[Post](dispatch(method.returnType))(ResultVar)
      currentResultVar.having(Local[Post](res.ref)(ResultVar)) {
        method.rewrite(
          returnType = TVoid()(method.o),
          outArgs = collectInScope(variableScopes) {
            res.declareDefault(this)
            method.outArgs.foreach(dispatch)
          },
        ).succeedDefault(method)
      }
    case other => rewriteDefault(other)
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] =
    if(inPure) dispatchPure(e)
    else dispatchImpure(e)

  def dispatchPure(e: Expr[Pre]): Expr[Post] = e match {
    case Result(_) if currentResultVar.nonEmpty => currentResultVar.top
    case _: PreAssignExpression[Pre] | _: PostAssignExpression[Pre] | _: With[Pre] | _: Then[Pre] |
         _: MethodInvocation[Pre] | _: ProcedureInvocation[Pre] =>
      throw DisallowedSideEffect(e)
    case other => rewriteDefault(other)
  }

  def inlined(e: Expr[Pre]): Expr[Post] =
    ReInliner().dispatch(dispatchImpure(e))

  def notInlined(e: Expr[Pre]): Local[Post] =
    dispatchImpure(e) match {
      case Local(Ref(v)) if !currentlyExtracted.contains(v) =>
        val preV = (v: Variable[Post]).asInstanceOf[Variable[Pre]]
        Local[Post](succ(preV))(e.o)
      case other => other
    }

  def stored(e: Expr[Post], t: Type[Pre]): Local[Post] = {
    val v = new Variable[Post](dispatch(t))(SideEffectOrigin)
    currentlyExtracted(v) = (currentConditions.toSeq, e)
    Local[Post](v.ref)(SideEffectOrigin)
  }

  def assignTarget(target: Expr[Pre]): Expr[Post] = {
    val result = target match {
      case Local(Ref(v)) => Local[Post](succ(v))(target.o)
      case Deref(obj, Ref(f)) => Deref[Post](notInlined(obj), succ(f))(DerefAssignTarget)(target.o)
      case ArraySubscript(arr, index) => ArraySubscript[Post](notInlined(arr), notInlined(index))(SubscriptAssignTarget)(target.o)
      case PointerSubscript(arr, index) => PointerSubscript[Post](notInlined(arr), notInlined(index))(SubscriptAssignTarget)(target.o)
      case other => ???
    }
    flushExtractedExpressions()
    result
  }

  def dispatchImpure(e: Expr[Pre]): Local[Post] = e match {
    case Local(Ref(v)) =>
      // We do not take the successor here: ReInliner will do that.
      val postV = (v : Variable[Pre]).asInstanceOf[Variable[Post]]
      Local[Post](postV.ref)(e.o)
    case Result(_) if currentResultVar.nonEmpty => currentResultVar.top

    // ## Nodes that induce an implicit evaluation condition: ##
    case Select(cond, whenTrue, whenFalse) =>
      val cond1 = dispatchImpure(cond)
      val whenTrue1 = currentConditions.having(cond1) { dispatchImpure(whenTrue) }
      val whenFalse1 = currentConditions.having(Not(cond1)(e.o)) { dispatchImpure(whenFalse) }
      stored(ReInliner().dispatch(Select(cond1, whenTrue1, whenFalse1)(e.o)), e.t)
    case Implies(left, right) =>
      val left1 = dispatchImpure(left)
      val right1 = currentConditions.having(left1) { dispatchImpure(right) }
      stored(ReInliner().dispatch(Implies(left1, right1)(e.o)), e.t)
    case And(left, right) =>
      val left1 = dispatchImpure(left)
      val right1 = currentConditions.having(left1) { dispatchImpure(right) }
      stored(ReInliner().dispatch(And(left1, right1)(e.o)), e.t)
    // Star ????
    case Or(left, right) =>
      val left1 = dispatchImpure(left)
      val right1 = currentConditions.having(Not(left1)(e.o)) { dispatchImpure(right) }
      stored(ReInliner().dispatch(Or(left1, right1)(e.o)), e.t)

    // ## Nodes that induce side effects: ##
    case ass @ PreAssignExpression(oldTarget, oldValue) =>
      // target and value could be inline, if it were not for the fact that we need value to return it (since value
      // may have a more specific type than the target type)
      val target = assignTarget(oldTarget)
      val value = notInlined(oldValue)
      effect(Assign(target, value)(ass.blame)(e.o))
      stored(value, oldValue.t)
    case ass @ PostAssignExpression(oldTarget, value) =>
      val target = assignTarget(oldTarget)
      val cachedTarget = stored(target, oldTarget.t)
      effect(Assign(target, inlined(value))(ass.blame)(e.o))
      stored(cachedTarget, oldTarget.t)
    case With(pre, value) =>
      effect(dispatch(pre))
      dispatchImpure(value)
    case Then(oldValue, post) =>
      val value = notInlined(oldValue)
      effect(dispatch(post))
      stored(value, oldValue.t)
    case inv @ MethodInvocation(obj, Ref(method), args, outArgs, typeArgs, givenMap, yields) =>
      val res = new Variable[Post](dispatch(method.returnType))(ResultVar)
      res.succeedDefault(res.asInstanceOf[Variable[Pre]])
      effect(InvokeMethod[Post](
        obj = inlined(obj),
        ref = succ(method),
        args = args.map(inlined),
        outArgs = res.ref[Variable[Post]] +: outArgs.map(succ[Variable[Post]]),
        typeArgs = typeArgs.map(dispatch),
        givenMap.map { case (Ref(v), e) => (succ(v), dispatch(e)) },
        yields.map { case (Ref(e), Ref(v)) => (succ(e), succ(v)) },
      )(inv.blame)(e.o))
      stored(res.get(SideEffectOrigin), method.returnType)
    case inv @ ProcedureInvocation(Ref(method), args, outArgs, typeArgs, givenMap, yields) =>
      val res = new Variable[Post](dispatch(method.returnType))(ResultVar)
      res.succeedDefault(res.asInstanceOf[Variable[Pre]])
      effect(InvokeProcedure[Post](
        ref = succ(method),
        args = args.map(inlined),
        outArgs = res.ref[Variable[Post]] +: outArgs.map(succ[Variable[Post]]),
        typeArgs = typeArgs.map(dispatch),
        givenMap.map { case (Ref(v), e) => (succ(v), inlined(e)) },
        yields.map { case (Ref(e), Ref(v)) => (succ(e), succ(v)) },
      )(inv.blame)(e.o))
      stored(res.get(SideEffectOrigin), method.returnType)
    case other =>
      stored(ReInliner().dispatch(rewriteDefault(other)), other.t)
  }
}
