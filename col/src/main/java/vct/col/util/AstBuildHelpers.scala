package vct.col.util

import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.ast.expr.apply.FunctionInvocationImpl
import vct.col.origin._
import vct.col.ref.{DirectRef, Ref}

/**
 * Collection of general AST building utilities. This is meant to organically grow, so add helpers as you see fit.
 */
object AstBuildHelpers {
  val ZERO: BigInt = BigInt(0)
  val ONE: BigInt = BigInt(1)

  /**
   * <strong>IMPORTANT</strong>: operators that end with a colon (`:`) are <strong>right-associative</strong>. For example:
   *
   * {{{
   * trait Exp {
   *   // wrong assumption: this /: right is Divide(this, right)
   *   def /:(right: Exp) = Divide(this, right)
   * }
   *
   * case class Number(x: Int) extends Exp
   * case class Divide(left: Exp, right: Exp)
   *
   * val one = Number(1)
   * val two = Number(2)
   * assert( (one /: two) == Divide(two, one) ) // !!!
   * assert( (one /: two) == two./:(one) )
   * }}}
   *
   * Also take into account how scala defines
   * <a href="https://docs.scala-lang.org/tour/operators.html#precedence">operator precedence</a>
   * when using or defining operators here. The precedence generally does <strong>not</strong> match e.g. the syntax
   * of PVL.
   */
  implicit class ExprBuildHelpers[G](left: Expr[G]) {
    def +(right: Expr[G])(implicit origin: Origin): Plus[G] = Plus(left, right)
    def -(right: Expr[G])(implicit origin: Origin): Minus[G] = Minus(left, right)
    def *(right: Expr[G])(implicit origin: Origin): Mult[G] = Mult(left, right)
    def /(right: Expr[G])(implicit origin: Origin, blame: Blame[DivByZero]): FloorDiv[G] = FloorDiv(left, right)(blame)
    def /:/(right: Expr[G])(implicit origin: Origin, blame: Blame[DivByZero]): Div[G] = Div(left, right)(blame)
    def %(right: Expr[G])(implicit origin: Origin, blame: Blame[DivByZero]): Mod[G] = Mod(left, right)(blame)

    def ===(right: Expr[G])(implicit origin: Origin): Eq[G] = Eq(left, right)
    def !==(right: Expr[G])(implicit origin: Origin): Neq[G] = Neq(left, right)
    def <(right: Expr[G])(implicit origin: Origin): Less[G] = Less(left, right)
    def >(right: Expr[G])(implicit origin: Origin): Greater[G] = Greater(left, right)
    def <=(right: Expr[G])(implicit origin: Origin): LessEq[G] = LessEq(left, right)
    def >=(right: Expr[G])(implicit origin: Origin): GreaterEq[G] = GreaterEq(left, right)

    def unary_!(implicit origin: Origin): Not[G] = Not(left)
    def &&(right: Expr[G])(implicit origin: Origin): And[G] = And(left, right)
    def ||(right: Expr[G])(implicit origin: Origin): Or[G] = Or(left, right)
    def &*(right: Expr[G])(implicit origin: Origin): Star[G] = Star(left, right)

    def ==>(right: Expr[G])(implicit origin: Origin): Implies[G] = Implies(left, right)

    def ~>(field: SilverField[G])(implicit blame: Blame[InsufficientPermission], origin: Origin): SilverDeref[G] = SilverDeref[G](left, field.ref)(blame)

    def @@(index: Expr[G])(implicit blame: Blame[SeqBoundFailure], origin: Origin): SeqSubscript[G] = SeqSubscript(left, index)(blame)
  }

  implicit class VarBuildHelpers[G](left: Variable[G]) {
    def get(implicit origin: Origin): Local[G] = Local(new DirectRef(left))
    def <~(right: Expr[G])(implicit origin: Origin): SilverLocalAssign[G] = SilverLocalAssign(new DirectRef(left), right)
  }

  implicit class FieldBuildHelpers[G](left: SilverDeref[G]) {
    def <~(right: Expr[G])(implicit blame: Blame[AssignFailed], origin: Origin): SilverFieldAssign[G] = SilverFieldAssign(left.obj, left.field, right)(blame)
  }

  implicit class ApplicableBuildHelpers[Pre, Post](applicable: Applicable[Pre])(implicit rewriter: AbstractRewriter[Pre, Post]) {
    def rewrite(args: => Seq[Variable[Post]] = rewriter.variables.dispatch(applicable.args),
               ): Applicable[Post] = applicable match {
      case inlineable: InlineableApplicable[Pre] =>
        new InlineableApplicableBuildHelpers(inlineable).rewrite(args = args)
      case function: ADTFunction[Pre] =>
        new RewriteADTFunction(function).rewrite(args = args)
      case process: ModelProcess[Pre] =>
        new RewriteModelProcess(process).rewrite(args = args)
      case action: ModelAction[Pre] =>
        new RewriteModelAction(action).rewrite(args = args)
    }
  }

  implicit class InlineableApplicableBuildHelpers[Pre, Post](inlineable: InlineableApplicable[Pre])(implicit rewriter: AbstractRewriter[Pre, Post]) {
    def rewrite(args: => Seq[Variable[Post]] = rewriter.variables.dispatch(inlineable.args),
                inline: => Boolean = inlineable.inline,
               ): InlineableApplicable[Post] = inlineable match {
      case pred: AbstractPredicate[Pre] =>
        new PredicateBuildHelpers(pred).rewrite(args = args, inline = inline)
      case contracted: ContractApplicable[Pre] =>
        new ContractApplicableBuildHelpers(contracted).rewrite(args = args, inline = inline)
    }
  }

  implicit class ContractApplicableBuildHelpers[Pre, Post](contracted: ContractApplicable[Pre])(implicit rewriter: AbstractRewriter[Pre, Post]) {
    def rewrite(args: => Seq[Variable[Post]] = rewriter.variables.dispatch(contracted.args),
                returnType: => Type[Post] = rewriter.dispatch(contracted.returnType),
                contract: => ApplicableContract[Post] = rewriter.dispatch(contracted.contract),
                typeArgs: => Seq[Variable[Post]] = rewriter.variables.dispatch(contracted.typeArgs),
                inline: => Boolean = contracted.inline,
               ): ContractApplicable[Post] = contracted match {
      case function: Function[Pre] =>
        new RewriteFunction(function).rewrite(args = args, returnType = returnType, inline = inline, contract = contract, typeArgs = typeArgs)
      case function: InstanceFunction[Pre] =>
        new RewriteInstanceFunction(function).rewrite(args = args, returnType = returnType, inline = inline, contract = contract, typeArgs = typeArgs)
      case method: AbstractMethod[Pre] =>
        new MethodBuildHelpers(method).rewrite(args = args, returnType = returnType, inline = inline, contract = contract, typeArgs = typeArgs)
    }
  }

  implicit class MethodBuildHelpers[Pre, Post](method: AbstractMethod[Pre])(implicit rewriter: AbstractRewriter[Pre, Post]) {
    def rewrite(args: => Seq[Variable[Post]] = rewriter.variables.dispatch(method.args),
                returnType: => Type[Post] = rewriter.dispatch(method.returnType),
                outArgs: => Seq[Variable[Post]] = rewriter.variables.dispatch(method.outArgs),
                body: => Option[Statement[Post]] = method.body.map(rewriter.dispatch),
                contract: => ApplicableContract[Post] = rewriter.dispatch(method.contract),
                typeArgs: => Seq[Variable[Post]] = rewriter.variables.dispatch(method.typeArgs),
                inline: => Boolean = method.inline,
                pure: => Boolean = method.pure,
                blame: Blame[CallableFailure] = method.blame,
               ): AbstractMethod[Post] = method match {
      case procedure: Procedure[Pre] =>
        new RewriteProcedure(procedure).rewrite(args = args, returnType = returnType, body = body, inline = inline, contract = contract, typeArgs = typeArgs, outArgs = outArgs, pure = pure, blame = blame)
      case method: InstanceMethod[Pre] =>
        new RewriteInstanceMethod(method).rewrite(args = args, returnType = returnType, body = body, inline = inline, contract = contract, typeArgs = typeArgs, outArgs = outArgs, pure = pure, blame = blame)
      case method: InstanceOperatorMethod[Pre] =>
        new RewriteInstanceOperatorMethod(method).rewrite(returnType = returnType, operator = rewriter.dispatch(method.operator), args = args, body = body, contract = contract, inline = inline, pure = pure, blame = blame)
    }
  }

  implicit class FunctionBuildHelpers[Pre, Post](function: AbstractFunction[Pre])(implicit rewriter: AbstractRewriter[Pre, Post]) {
    def rewrite(args: => Seq[Variable[Post]] = rewriter.variables.dispatch(function.args),
                returnType: => Type[Post] = rewriter.dispatch(function.returnType),
                body: => Option[Expr[Post]] = function.body.map(rewriter.dispatch),
                contract: => ApplicableContract[Post] = rewriter.dispatch(function.contract),
                typeArgs: => Seq[Variable[Post]] = rewriter.variables.dispatch(function.typeArgs),
                inline: => Boolean = function.inline,
                threadLocal: => Boolean = function.threadLocal,
                blame: => Blame[ContractedFailure] = function.blame,
               ): ContractApplicable[Post] = function match {
      case function: Function[Pre] =>
        new RewriteFunction(function).rewrite(args = args, returnType = returnType, body = body, inline = inline, threadLocal = threadLocal, contract = contract, typeArgs = typeArgs, blame = blame)
      case function: InstanceFunction[Pre] =>
        new RewriteInstanceFunction(function).rewrite(args = args, returnType = returnType, body = body, inline = inline, threadLocal = threadLocal, contract = contract, typeArgs = typeArgs, blame = blame)
      case function: InstanceOperatorFunction[Pre] =>
        new RewriteInstanceOperatorFunction(function).rewrite(returnType = returnType, operator = rewriter.dispatch(function.operator), args = args, body = body, contract = contract, inline = inline, threadLocal = threadLocal, blame = blame)
    }
  }

  implicit class PredicateBuildHelpers[Pre, Post](predicate: AbstractPredicate[Pre])(implicit rewriter: AbstractRewriter[Pre, Post]) {
    def rewrite(args: => Seq[Variable[Post]] = rewriter.variables.dispatch(predicate.args),
                inline: => Boolean = predicate.inline,
                threadLocal: => Boolean = predicate.threadLocal,
               ): AbstractPredicate[Post] = predicate match {
      case predicate: Predicate[Pre] =>
        new RewritePredicate(predicate).rewrite(args = args, inline = inline, threadLocal = threadLocal)
      case predicate: InstancePredicate[Pre] =>
        new RewriteInstancePredicate(predicate).rewrite(args = args, inline = inline, threadLocal = threadLocal)
    }
  }

  implicit class ApplyBuildHelpers[Pre, Post](apply: Apply[Pre])(implicit rewriter: AbstractRewriter[Pre, Post]) {
    def rewrite(args: => Seq[Expr[Post]] = apply.args.map(rewriter.dispatch)): Apply[Post] = apply match {
      case inv: ADTFunctionInvocation[Pre] =>
        new RewriteADTFunctionInvocation(inv).rewrite(args = args)
      case apply: ApplyAnyPredicate[Pre] =>
        new ApplyAnyPredicateBuildHelpers(apply).rewrite(args = args)
      case inv: Invocation[Pre] =>
        new InvocationBuildHelpers(inv).rewrite(args = args)
    }
  }

  implicit class ApplyAnyPredicateBuildHelpers[Pre, Post](apply: ApplyAnyPredicate[Pre])(implicit rewriter: AbstractRewriter[Pre, Post]) {
    def rewrite(args: => Seq[Expr[Post]] = apply.args.map(rewriter.dispatch), perm: => Expr[Post] = rewriter.dispatch(apply.perm)): ApplyAnyPredicate[Post] = apply match {
      case inv: PredicateApply[Pre] =>
        new RewritePredicateApply(inv).rewrite(args = args, perm = perm)
      case inv: InstancePredicateApply[Pre] =>
        new RewriteInstancePredicateApply(inv).rewrite(args = args, perm = perm)
      case inv: CoalesceInstancePredicateApply[Pre] =>
        new RewriteCoalesceInstancePredicateApply(inv).rewrite(args = args, perm = perm)
    }
  }

  implicit class InvocationBuildHelpers[Pre, Post](apply: Invocation[Pre])(implicit rewriter: AbstractRewriter[Pre, Post]) {
    def rewrite(args: => Seq[Expr[Post]] = apply.args.map(rewriter.dispatch), givenMap: => Seq[(Ref[Post, Variable[Post]], Expr[Post])] = apply.givenMap.map { case (Ref(v), e) => (rewriter.succ(v), rewriter.dispatch(e)) }, yields: Seq[(Expr[Post], Ref[Post, Variable[Post]])] = apply.yields.map { case (a, b) => (rewriter.dispatch(a), rewriter.succ(b.decl)) }): Invocation[Post] = apply match {
      case apply: AnyFunctionInvocation[Pre] =>
        new ApplyAnyFunctionBuildHelpers(apply).rewrite(args = args, givenMap = givenMap, yields = yields)
      case apply: AnyMethodInvocation[Pre] =>
        new ApplyAnyMethodBuildHelpers(apply).rewrite(args = args, givenMap = givenMap, yields = yields)
    }
  }

  implicit class ApplyAnyFunctionBuildHelpers[Pre, Post](apply: AnyFunctionInvocation[Pre])(implicit rewriter: AbstractRewriter[Pre, Post]) {
    def rewrite(args: => Seq[Expr[Post]] = apply.args.map(rewriter.dispatch), typeArgs: => Seq[Type[Post]] = apply.typeArgs.map(rewriter.dispatch), givenMap: => Seq[(Ref[Post, Variable[Post]], Expr[Post])] = apply.givenMap.map { case (Ref(v), e) => (rewriter.succ(v), rewriter.dispatch(e)) }, yields: Seq[(Expr[Post], Ref[Post, Variable[Post]])] = apply.yields.map { case (a, b) => (rewriter.dispatch(a), rewriter.succ(b.decl)) }): AnyFunctionInvocation[Post] = apply match {
      case inv: FunctionInvocation[Pre] =>
        new RewriteFunctionInvocation(inv).rewrite(args = args, typeArgs = typeArgs, givenMap = givenMap, yields = yields)
      case inv: InstanceFunctionInvocation[Pre] =>
        new RewriteInstanceFunctionInvocation(inv).rewrite(args = args, typeArgs = typeArgs, givenMap = givenMap, yields = yields)
    }
  }

  implicit class ApplyAnyMethodBuildHelpers[Pre, Post](apply: AnyMethodInvocation[Pre])(implicit rewriter: AbstractRewriter[Pre, Post]) {
    def rewrite(args: => Seq[Expr[Post]] = apply.args.map(rewriter.dispatch), outArgs: => Seq[Expr[Post]] = apply.outArgs.map(rewriter.dispatch), typeArgs: => Seq[Type[Post]] = apply.typeArgs.map(rewriter.dispatch), givenMap: => Seq[(Ref[Post, Variable[Post]], Expr[Post])] = apply.givenMap.map { case (Ref(v), e) => (rewriter.succ(v), rewriter.dispatch(e)) }, yields: => Seq[(Expr[Post], Ref[Post, Variable[Post]])] = apply.yields.map { case (a, b) => (rewriter.dispatch(a), rewriter.succ(b.decl)) }): AnyMethodInvocation[Post] = apply match {
      case inv: ProcedureInvocation[Pre] =>
        new RewriteProcedureInvocation(inv).rewrite(args = args, outArgs = outArgs, typeArgs = typeArgs, givenMap = givenMap, yields = yields)
      case inv: MethodInvocation[Pre] =>
        new RewriteMethodInvocation(inv).rewrite(args = args, outArgs = outArgs, typeArgs = typeArgs, givenMap = givenMap, yields = yields)
    }
  }

  implicit class InvocationStatementBuildHelpers[Pre, Post](apply: InvocationStatement[Pre])(implicit rewriter: AbstractRewriter[Pre, Post]) {
    def rewrite(args: => Seq[Expr[Post]] = apply.args.map(rewriter.dispatch), outArgs: => Seq[Expr[Post]] = apply.outArgs.map(rewriter.dispatch), typeArgs: => Seq[Type[Post]] = apply.typeArgs.map(rewriter.dispatch), givenMap: => Seq[(Ref[Post, Variable[Post]], Expr[Post])] = apply.givenMap.map { case (Ref(v), e) => (rewriter.succ(v), rewriter.dispatch(e)) }, yields: => Seq[(Expr[Post], Ref[Post, Variable[Post]])] = apply.yields.map { case (a, b) => (rewriter.dispatch(a), rewriter.succ(b.decl)) }): InvocationStatement[Post] = apply match {
      case inv: InvokeProcedure[Pre] =>
        new RewriteInvokeProcedure(inv).rewrite(args = args, outArgs = outArgs, typeArgs = typeArgs, givenMap = givenMap, yields = yields)
      case inv: InvokeMethod[Pre] =>
        new RewriteInvokeMethod(inv).rewrite(args = args, outArgs = outArgs, typeArgs = typeArgs, givenMap = givenMap, yields = yields)
    }
  }

  private case class ConstOrigin(value: scala.Any) extends Origin {
    override def preferredName: String = "unknown"
    override def shortPosition: String = "generated"
    override def context: String = s"[At generated constant `$value`]"
    override def inlineContext: String = value.toString
  }

  def tt[G]: BooleanValue[G] = BooleanValue(true)(ConstOrigin(true))
  def ff[G]: BooleanValue[G] = BooleanValue(false)(ConstOrigin(false))

  def const[G](i: Int)(implicit o: Origin): IntegerValue[G] =
    IntegerValue(i)

  def const[G](i: BigInt)(implicit o: Origin): IntegerValue[G] =
    IntegerValue(i)

  def contract[G]
              (blame: Blame[NontrivialUnsatisfiable],
               requires: AccountedPredicate[G] = UnitAccountedPredicate(tt[G])(ConstOrigin(true)),
               ensures: AccountedPredicate[G] = UnitAccountedPredicate(tt[G])(ConstOrigin(true)),
               contextEverywhere: Expr[G] = tt[G],
               signals: Seq[SignalsClause[G]] = Nil,
               givenArgs: Seq[Variable[G]] = Nil, yieldsArgs: Seq[Variable[G]] = Nil,
               decreases: Option[DecreasesClause[G]] = None,
              )
              (implicit o: Origin): ApplicableContract[G] =
    ApplicableContract(requires, ensures, contextEverywhere, signals, givenArgs, yieldsArgs, decreases)(blame)

  def withResult[G, T <: ContractApplicable[G]](builder: Result[G] => T)(implicit o: Origin): T = {
    val box = SuccessionMap[Unit, ContractApplicable[G]]()
    val result = Result[G](box.ref(()))
    val applicable = builder(result)
    box(()) = applicable
    applicable
  }

  def procedure[G]
               (blame: Blame[CallableFailure],
                contractBlame: Blame[NontrivialUnsatisfiable],
                returnType: Type[G] = TVoid[G](),
                args: Seq[Variable[G]] = Nil, outArgs: Seq[Variable[G]] = Nil, typeArgs: Seq[Variable[G]] = Nil,
                body: Option[Statement[G]] = None,
                requires: AccountedPredicate[G] = UnitAccountedPredicate(tt[G])(ConstOrigin(true)),
                ensures: AccountedPredicate[G] = UnitAccountedPredicate(tt[G])(ConstOrigin(true)),
                contextEverywhere: Expr[G] = tt[G],
                signals: Seq[SignalsClause[G]] = Nil,
                givenArgs: Seq[Variable[G]] = Nil, yieldsArgs: Seq[Variable[G]] = Nil,
                decreases: Option[DecreasesClause[G]] = None,
                inline: Boolean = false, pure: Boolean = false)
               (implicit o: Origin): Procedure[G] =
    new Procedure(returnType, args, outArgs, typeArgs, body,
      ApplicableContract(requires, ensures, contextEverywhere, signals, givenArgs, yieldsArgs, decreases)(contractBlame),
      inline, pure)(blame)

  def function[G]
              (blame: Blame[ContractedFailure],
               contractBlame: Blame[NontrivialUnsatisfiable],
               returnType: Type[G] = TVoid(),
               args: Seq[Variable[G]] = Nil, typeArgs: Seq[Variable[G]] = Nil,
               body: Option[Expr[G]] = None,
               requires: AccountedPredicate[G] = UnitAccountedPredicate(tt[G])(ConstOrigin(true)),
               ensures: AccountedPredicate[G] = UnitAccountedPredicate(tt[G])(ConstOrigin(true)),
               contextEverywhere: Expr[G] = tt[G],
               signals: Seq[SignalsClause[G]] = Nil,
               givenArgs: Seq[Variable[G]] = Nil, yieldsArgs: Seq[Variable[G]] = Nil,
               decreases: Option[DecreasesClause[G]] = Some(DecreasesClauseNoRecursion[G]()(ConstOrigin("decreases"))),
               inline: Boolean = false)(implicit o: Origin): Function[G] =
    new Function(returnType, args, typeArgs, body,
      ApplicableContract(requires, ensures, contextEverywhere, signals, givenArgs, yieldsArgs, decreases)(contractBlame),
      inline)(blame)

  def functionInvocation[G]
                        (blame: Blame[InvocationFailure],
                         ref: Ref[G, Function[G]],
                         args: Seq[Expr[G]] = Nil,
                         typeArgs: Seq[Type[G]] = Nil,
                         givenMap: Seq[(Ref[G, Variable[G]], Expr[G])] = Nil,
                         yields: Seq[(Expr[G], Ref[G, Variable[G]])] = Nil)(implicit o: Origin): FunctionInvocation[G] =
    FunctionInvocation(ref, args, typeArgs, givenMap, yields)(blame)

  def methodInvocation[G]
                      (blame: Blame[InstanceInvocationFailure],
                       obj: Expr[G],
                       ref: Ref[G, InstanceMethod[G]],
                       args: Seq[Expr[G]] = Nil,
                       outArgs: Seq[Expr[G]] = Nil,
                       typeArgs: Seq[Type[G]] = Nil,
                       givenMap: Seq[(Ref[G, Variable[G]], Expr[G])] = Nil,
                       yields: Seq[(Expr[G], Ref[G, Variable[G]])] = Nil)(implicit o: Origin): MethodInvocation[G] =
    MethodInvocation(obj, ref, args, outArgs, typeArgs, givenMap, yields)(blame)

  case object GeneratedQuantifier extends Origin {
    override def preferredName: String = "i"
    override def shortPosition: String = "generated"
    override def context: String = "[At generated quantifier]"
    override def inlineContext: String = "[Generated quantifier]"
  }

  def starall[G]
             (blame: Blame[ReceiverNotInjective],
              t: Type[G],
              body: Local[G] => Expr[G],
              triggers: Local[G] => Seq[Seq[Expr[G]]] = (_: Local[G]) => Nil,
             ): Starall[G] = {
    implicit val o: Origin = GeneratedQuantifier
    val i_var = new Variable[G](t)
    val i = Local[G](i_var.ref)
    Starall(
      bindings = Seq(i_var),
      triggers = triggers(i),
      body = body(i),
    )(blame)
  }

  def forall[G]
            (t: Type[G],
             body: Local[G] => Expr[G],
             triggers: Local[G] => Seq[Seq[Expr[G]]] = (_: Local[G]) => Nil,
            ): Forall[G] = {
    implicit val o: Origin = GeneratedQuantifier
    val i_var = new Variable[G](t)
    val i = Local[G](i_var.ref)
    Forall(
      bindings = Seq(i_var),
      triggers = triggers(i),
      body = body(i),
    )
  }

  def foralls[G]
            (ts: Seq[Type[G]],
             body: Seq[Local[G]] => Expr[G],
             triggers: Seq[Local[G]] => Seq[Seq[Expr[G]]] = (_: Seq[Local[G]]) => Nil,
            ): Forall[G] = {
    implicit val o: Origin = GeneratedQuantifier
    val i_vars: Seq[Variable[G]] = ts.map(new Variable[G](_))
    val is: Seq[Local[G]] = i_vars.map((x: Variable[G]) => Local[G](x.ref))
    Forall(
      bindings = i_vars,
      triggers = triggers(is),
      body = body(is),
    )
  }

  case object GeneratedLet extends Origin {
    override def preferredName: String = "x"
    override def shortPosition: String = "generated"
    override def context: String = "[At generated let]"
    override def inlineContext: String = "[Generated let]"
  }

  def let[G](t: Type[G], x: Expr[G], body: Local[G] => Expr[G]): Let[G] = {
    implicit val o: Origin = GeneratedQuantifier
    val x_var: Variable[G] = new Variable[G](t)
    val x_local: Local[G] = Local(x_var.ref)
    Let(x_var, x, body(x_local))
  }

  def assignLocal[G](local: Local[G], value: Expr[G])(implicit o: Origin): Assign[G] =
    Assign(local, value)(AssignLocalOk)

  def assignField[G](obj: Expr[G], field: Ref[G, InstanceField[G]], value: Expr[G], blame: Blame[AssignFailed])(implicit o: Origin): Assign[G] =
    Assign(Deref(obj, field)(DerefAssignTarget), value)(blame)

  def fieldPerm[G](obj: Expr[G], field: Ref[G, InstanceField[G]], amount: Expr[G])(implicit o: Origin): Perm[G] =
    Perm(FieldLocation(obj, field), amount)

  def arrayPerm[G](arr: Expr[G], index: Expr[G], amount: Expr[G], arrayLocationError: Blame[ArrayLocationError])(implicit o: Origin): Perm[G] =
    Perm(ArrayLocation(arr, index)(arrayLocationError), amount)

  def foldAnd[G](exprs: Iterable[Expr[G]])(implicit o: Origin): Expr[G] =
    exprs.reduceOption(And(_, _)).getOrElse(tt)

  def implies[G](conds: Seq[Expr[G]], body: Expr[G])(implicit o: Origin): Expr[G] = conds match {
    case Nil => body
    case conds => Implies(foldAnd(conds), body)
  }

  def unfoldPredicate[G](p: AccountedPredicate[G]): Seq[Expr[G]] = p match {
    case UnitAccountedPredicate(pred) => Seq(pred)
    case SplitAccountedPredicate(left, right) => unfoldPredicate(left) ++ unfoldPredicate(right)
  }

  def mapPredicate[G1, G2](p: AccountedPredicate[G1], f: Expr[G1] => Expr[G2]): AccountedPredicate[G2] = p match {
    case UnitAccountedPredicate(pred) => UnitAccountedPredicate(f(pred))(p.o)
    case SplitAccountedPredicate(left, right) => SplitAccountedPredicate(mapPredicate(left, f), mapPredicate(right, f))(p.o)
  }

  def unfoldImplies[G](expr: Expr[G]): (Seq[Expr[G]], Expr[G]) = expr match {
    case Implies(left, right) =>
      val (antecedent, consequent) = AstBuildHelpers.unfoldImplies(right)
      (unfoldStar(left) ++ antecedent, consequent)
    case other => (Nil, other)
  }

  def unfoldStar[G](expr: Expr[G]): Seq[Expr[G]] = expr match {
    case Star(left, right) => AstBuildHelpers.unfoldStar(left) ++ AstBuildHelpers.unfoldStar(right)
    case And(left, right) => AstBuildHelpers.unfoldStar(left) ++ AstBuildHelpers.unfoldStar(right)
    case BooleanValue(true) => Nil
    case other => Seq(other)
  }

  // For if you want to be fussy about origins
  def mapUnfoldedStar[G1, G2](expr: Expr[G1], f: Expr[G1] => Expr[G2]): Expr[G2] = expr match {
    case Star(left, right) => Star(mapUnfoldedStar(left, f), mapUnfoldedStar(right, f))(expr.o)
    case And(left, right) => And(mapUnfoldedStar(left, f), mapUnfoldedStar(right, f))(expr.o)
    case other => f(other)
  }

  def foldStar[G](exprs: Seq[Expr[G]])(implicit o: Origin): Expr[G] =
    exprs.reduceOption(Star(_, _)).getOrElse(tt)

  def foldStar[G](predicate: AccountedPredicate[G])(implicit o: Origin): Expr[G] = predicate match {
    case UnitAccountedPredicate(pred) => pred
    case SplitAccountedPredicate(left, right) => Star(foldStar(left), foldStar(right))
  }

  def foldOr[G](exprs: Seq[Expr[G]])(implicit o: Origin): Expr[G] =
    exprs.reduceOption(Or(_, _)).getOrElse(ff)
}
