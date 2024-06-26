package vct.col.util

import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.ast.expr.apply.FunctionInvocationImpl
import vct.col.origin._
import vct.col.ref.{DirectRef, Ref}
import vct.col.rewrite.Rewritten
import vct.result.VerificationError.{Unreachable, UserError}

/** Collection of general AST building utilities. This is meant to organically
  * grow, so add helpers as you see fit.
  */
object AstBuildHelpers {
  val ZERO: BigInt = BigInt(0)
  val ONE: BigInt = BigInt(1)

  case class NumericDividingError(left: Expr[_], right: Expr[_])
      extends UserError {
    override def text: String =
      f"Expected types to numeric, but got: ${left.t} and ${right.t} for $left and $right"
    override def code: String = "numericDividingError"
  }

  /** <strong>IMPORTANT</strong>: operators that end with a colon (`:`) are
    * <strong>right-associative</strong>. For example:
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
    * Also take into account how scala defines <a
    * href="https://docs.scala-lang.org/tour/operators.html#precedence">operator
    * precedence</a> when using or defining operators here. The precedence
    * generally does <strong>not</strong> match e.g. the syntax of PVL.
    */
  implicit class ExprBuildHelpers[G](left: Expr[G]) {
    def +(right: Expr[G])(implicit origin: Origin): Plus[G] = Plus(left, right)
    def -(right: Expr[G])(implicit origin: Origin): Minus[G] =
      Minus(left, right)
    def *(right: Expr[G])(implicit origin: Origin): Mult[G] = Mult(left, right)
    def /(
        right: Expr[G]
    )(implicit origin: Origin, blame: Blame[DivByZero]): DividingExpr[G] = {
      (left.t, right.t) match {
        case (_: IntType[G], _: IntType[G]) => FloorDiv(left, right)(blame)
        case (_: FloatType[G], _: FloatType[G]) => FloatDiv(left, right)(blame)
        case (_: TRational[G], _: TRational[G]) => RatDiv(left, right)(blame)
        case (_: TRational[G], _: IntType[G]) => RatDiv(left, right)(blame)
        case _ => throw NumericDividingError(left, right)
      }
    }
    def /:/(
        right: Expr[G]
    )(implicit origin: Origin, blame: Blame[DivByZero]): RatDiv[G] =
      RatDiv(left, right)(blame)
    def %(
        right: Expr[G]
    )(implicit origin: Origin, blame: Blame[DivByZero]): Mod[G] =
      Mod(left, right)(blame)

    def ===(right: Expr[G])(implicit origin: Origin): Eq[G] = Eq(left, right)
    def !==(right: Expr[G])(implicit origin: Origin): Neq[G] = Neq(left, right)
    def <(right: Expr[G])(implicit origin: Origin): Less[G] = Less(left, right)
    def >(right: Expr[G])(implicit origin: Origin): Greater[G] =
      Greater(left, right)
    def <=(right: Expr[G])(implicit origin: Origin): LessEq[G] =
      LessEq(left, right)
    def >=(right: Expr[G])(implicit origin: Origin): GreaterEq[G] =
      GreaterEq(left, right)

    def unary_!(implicit origin: Origin): Not[G] = Not(left)
    def &&(right: Expr[G])(implicit origin: Origin): And[G] = And(left, right)
    def ||(right: Expr[G])(implicit origin: Origin): Or[G] = Or(left, right)
    def &*(right: Expr[G])(implicit origin: Origin): Star[G] = Star(left, right)

    def ==>(right: Expr[G])(implicit origin: Origin): Implies[G] =
      Implies(left, right)

    def ~>(field: SilverField[G])(blame: Blame[InsufficientPermission])(
        implicit origin: Origin
    ): SilverDeref[G] = SilverDeref[G](left, field.ref)(blame)

    def @@(index: Expr[G])(blame: Blame[SeqBoundFailure])(
        implicit origin: Origin
    ): SeqSubscript[G] = SeqSubscript(left, index)(blame)

    def accounted(implicit o: Origin): UnitAccountedPredicate[G] =
      UnitAccountedPredicate(left)
  }

  implicit class AccountedBuildHelpers[G](left: AccountedPredicate[G]) {
    def &*(right: Expr[G])(implicit o: Origin): SplitAccountedPredicate[G] =
      SplitAccountedPredicate(left, right.accounted)
    def &*(right: AccountedPredicate[G])(
        implicit o: Origin
    ): SplitAccountedPredicate[G] = SplitAccountedPredicate(left, right)
  }

  implicit class VarBuildHelpers[G](left: Variable[G]) {
    def get(implicit origin: Origin): Local[G] = Local(new DirectRef(left))
    def <~(right: Expr[G])(implicit origin: Origin): SilverLocalAssign[G] =
      SilverLocalAssign(new DirectRef(left), right)
  }

  implicit class FieldBuildHelpers[G](left: SilverDeref[G]) {
    def <~(right: Expr[G])(
        blame: Blame[AssignFailed]
    )(implicit origin: Origin): SilverFieldAssign[G] =
      SilverFieldAssign(left.obj, left.field, right)(blame)
  }

  implicit class ApplicableBuildHelpers[Pre, Post](applicable: Applicable[Pre])(
      implicit rewriter: AbstractRewriter[Pre, Post]
  ) {
    def rewrite(
        args: => Seq[Variable[Post]] = rewriter.variables
          .dispatch(applicable.args)
    ): Applicable[Post] =
      applicable match {
        case inlineable: InlineableApplicable[Pre] =>
          new InlineableApplicableBuildHelpers(inlineable).rewrite(args = args)
        case function: ADTFunction[Pre] => function.rewrite(args = args)
        case process: ModelProcess[Pre] => process.rewrite(args = args)
        case action: ModelAction[Pre] => action.rewrite(args = args)
        case llvm: LlvmFunctionDefinition[Pre] => llvm.rewrite(args = args)
        case prover: ProverFunction[Pre] => prover.rewrite(args = args)
      }
  }

  implicit class InlineableApplicableBuildHelpers[Pre, Post](
      inlineable: InlineableApplicable[Pre]
  )(implicit rewriter: AbstractRewriter[Pre, Post]) {
    def rewrite(
        args: => Seq[Variable[Post]] = rewriter.variables
          .dispatch(inlineable.args),
        doInline: => Boolean = inlineable.doInline,
    ): InlineableApplicable[Post] =
      inlineable match {
        case pred: AbstractPredicate[Pre] =>
          new PredicateBuildHelpers(pred)
            .rewrite(args = args, doInline = doInline)
        case contracted: ContractApplicable[Pre] =>
          new ContractApplicableBuildHelpers(contracted)
            .rewrite(args = args, doInline = doInline)
      }
  }

  implicit class ContractApplicableBuildHelpers[Pre, Post](
      contracted: ContractApplicable[Pre]
  )(implicit rewriter: AbstractRewriter[Pre, Post]) {
    def rewrite(
        args: => Seq[Variable[Post]] = rewriter.variables
          .dispatch(contracted.args),
        returnType: => Type[Post] = rewriter.dispatch(contracted.returnType),
        typeArgs: => Seq[Variable[Post]] = rewriter.variables
          .dispatch(contracted.typeArgs),
        contract: => ApplicableContract[Post] = rewriter
          .dispatch(contracted.contract),
        doInline: => Boolean = contracted.doInline,
    ): ContractApplicable[Post] =
      contracted match {
        case function: Function[Pre] =>
          function.rewrite(
            args = args,
            returnType = returnType,
            typeArgs = typeArgs,
            doInline = Some(doInline),
            contract = contract,
          )
        case function: InstanceFunction[Pre] =>
          function.rewrite(
            args = args,
            returnType = returnType,
            typeArgs = typeArgs,
            doInline = Some(doInline),
            contract = contract,
          )
        case function: InstanceOperatorFunction[Pre] =>
          function.rewrite(
            args = args,
            returnType = returnType,
            doInline = Some(doInline),
            contract = contract,
          )
        case function: LlvmSpecFunction[Pre] =>
          function.rewrite(
            args = args,
            returnType = returnType,
            typeArgs = typeArgs,
            doInline = Some(doInline),
            contract = contract,
          )
        case method: AbstractMethod[Pre] =>
          new MethodBuildHelpers(method).rewrite(
            args = args,
            returnType = returnType,
            typeArgs = typeArgs,
            doInline = doInline,
            contract = contract,
          )
      }
  }

  implicit class MethodBuildHelpers[Pre, Post](method: AbstractMethod[Pre])(
      implicit rewriter: AbstractRewriter[Pre, Post]
  ) {
    def rewrite(
        args: => Seq[Variable[Post]] = rewriter.variables.dispatch(method.args),
        returnType: => Type[Post] = rewriter.dispatch(method.returnType),
        outArgs: => Seq[Variable[Post]] = rewriter.variables
          .dispatch(method.outArgs),
        body: => Option[Statement[Post]] = method.body.map(rewriter.dispatch),
        contract: => ApplicableContract[Post] = rewriter
          .dispatch(method.contract),
        typeArgs: => Seq[Variable[Post]] = rewriter.variables
          .dispatch(method.typeArgs),
        doInline: => Boolean = method.doInline,
        pure: => Boolean = method.pure,
        blame: Blame[CallableFailure] = method.blame,
    ): AbstractMethod[Post] =
      method match {
        case procedure: Procedure[Pre] =>
          procedure.rewrite(
            args = args,
            returnType = returnType,
            body = body,
            doInline = Some(doInline),
            contract = contract,
            typeArgs = typeArgs,
            outArgs = outArgs,
            pure = Some(pure),
            blame = blame,
          )
        case method: InstanceMethod[Pre] =>
          method.rewrite(
            args = args,
            returnType = returnType,
            body = body,
            doInline = Some(doInline),
            contract = contract,
            typeArgs = typeArgs,
            outArgs = outArgs,
            pure = Some(pure),
            blame = blame,
          )
        case method: InstanceOperatorMethod[Pre] =>
          method.rewrite(
            returnType = returnType,
            operator = rewriter.dispatch(method.operator),
            args = args,
            body = body,
            contract = contract,
            doInline = Some(doInline),
            pure = Some(pure),
            blame = blame,
          )
        case cons: Constructor[Pre] =>
          cons.rewrite(
            args = args,
            outArgs = outArgs,
            typeArgs = typeArgs,
            body = body,
            contract = contract,
            doInline = Some(doInline),
            blame = blame,
          )
      }
  }

  implicit class FunctionBuildHelpers[Pre, Post](
      function: AbstractFunction[Pre]
  )(implicit rewriter: AbstractRewriter[Pre, Post]) {
    def rewrite(
        args: => Seq[Variable[Post]] = rewriter.variables
          .dispatch(function.args),
        returnType: => Type[Post] = rewriter.dispatch(function.returnType),
        body: => Option[Expr[Post]] = function.body.map(rewriter.dispatch),
        contract: => ApplicableContract[Post] = rewriter
          .dispatch(function.contract),
        typeArgs: => Seq[Variable[Post]] = rewriter.variables
          .dispatch(function.typeArgs),
        doInline: => Boolean = function.doInline,
        threadLocal: => Boolean = function.threadLocal,
        blame: => Blame[ContractedFailure] = function.blame,
    ): ContractApplicable[Post] =
      function match {
        case function: Function[Pre] =>
          function.rewrite(
            args = args,
            returnType = returnType,
            body = body,
            doInline = Some(doInline),
            threadLocal = Some(threadLocal),
            contract = contract,
            typeArgs = typeArgs,
            blame = blame,
          )
        case function: InstanceFunction[Pre] =>
          function.rewrite(
            args = args,
            returnType = returnType,
            body = body,
            doInline = Some(doInline),
            threadLocal = Some(threadLocal),
            contract = contract,
            typeArgs = typeArgs,
            blame = blame,
          )
        case function: InstanceOperatorFunction[Pre] =>
          function.rewrite(
            returnType = returnType,
            args = args,
            body = body,
            contract = contract,
            doInline = Some(doInline),
            threadLocal = Some(threadLocal),
            blame = blame,
          )
        case function: LlvmSpecFunction[Pre] =>
          function.rewrite(
            returnType = returnType,
            args = args,
            body = body,
            contract = contract,
            doInline = Some(doInline),
            threadLocal = Some(threadLocal),
            blame = blame,
          )
      }
  }

  implicit class PredicateBuildHelpers[Pre, Post](
      predicate: AbstractPredicate[Pre]
  )(implicit rewriter: AbstractRewriter[Pre, Post]) {
    def rewrite(
        args: => Seq[Variable[Post]] = rewriter.variables
          .dispatch(predicate.args),
        doInline: => Boolean = predicate.doInline,
        threadLocal: => Boolean = predicate.threadLocal,
    ): AbstractPredicate[Post] =
      predicate match {
        case predicate: Predicate[Pre] =>
          predicate.rewrite(
            args = args,
            doInline = Some(doInline),
            threadLocal = Some(threadLocal),
          )
        case predicate: InstancePredicate[Pre] =>
          predicate.rewrite(
            args = args,
            doInline = Some(doInline),
            threadLocal = Some(threadLocal),
          )
      }
  }

  implicit class ApplyBuildHelpers[Pre, Post](apply: Apply[Pre])(
      implicit rewriter: AbstractRewriter[Pre, Post]
  ) {
    def rewrite(
        args: => Seq[Expr[Post]] = apply.args.map(rewriter.dispatch)
    ): Apply[Post] =
      apply match {
        case inv: ADTFunctionInvocation[Pre] => inv.rewrite(args = args)
        case inv: ProverFunctionInvocation[Pre] => inv.rewrite(args = args)
        case inv: LlvmFunctionInvocation[Pre] => inv.rewrite(args = args)
        case apply: ApplyAnyPredicate[Pre] =>
          new ApplyAnyPredicateBuildHelpers(apply).rewrite(args = args)
        case inv: Invocation[Pre] =>
          new InvocationBuildHelpers(inv).rewrite(args = args)
      }
  }

  implicit class ApplyAnyPredicateBuildHelpers[Pre, Post](
      apply: ApplyAnyPredicate[Pre]
  )(implicit rewriter: AbstractRewriter[Pre, Post]) {
    def rewrite(
        args: => Seq[Expr[Post]] = apply.args.map(rewriter.dispatch),
        perm: => Expr[Post] = rewriter.dispatch(apply.perm),
    ): ApplyAnyPredicate[Post] =
      apply match {
        case inv: PredicateApply[Pre] => inv.rewrite(args = args, perm = perm)
        case inv: InstancePredicateApply[Pre] =>
          inv.rewrite(args = args, perm = perm)
        case inv: CoalesceInstancePredicateApply[Pre] =>
          inv.rewrite(args = args, perm = perm)
      }
  }

  implicit class InvocationBuildHelpers[Pre, Post](apply: Invocation[Pre])(
      implicit rewriter: AbstractRewriter[Pre, Post]
  ) {
    def rewrite(
        args: => Seq[Expr[Post]] = apply.args.map(rewriter.dispatch),
        givenMap: => Seq[(Ref[Post, Variable[Post]], Expr[Post])] = apply
          .givenMap.map { case (Ref(v), e) =>
            (rewriter.succ(v), rewriter.dispatch(e))
          },
        yields: Seq[(Expr[Post], Ref[Post, Variable[Post]])] = apply.yields
          .map { case (a, b) => (rewriter.dispatch(a), rewriter.succ(b.decl)) },
    ): Invocation[Post] =
      apply match {
        case apply: AnyFunctionInvocation[Pre] =>
          new ApplyAnyFunctionBuildHelpers(apply)
            .rewrite(args = args, givenMap = givenMap, yields = yields)
        case apply: AnyMethodInvocation[Pre] =>
          new ApplyAnyMethodBuildHelpers(apply)
            .rewrite(args = args, givenMap = givenMap, yields = yields)
      }
  }

  implicit class ApplyAnyFunctionBuildHelpers[Pre, Post](
      apply: AnyFunctionInvocation[Pre]
  )(implicit rewriter: AbstractRewriter[Pre, Post]) {
    def rewrite(
        args: => Seq[Expr[Post]] = apply.args.map(rewriter.dispatch),
        typeArgs: => Seq[Type[Post]] = apply.typeArgs.map(rewriter.dispatch),
        givenMap: => Seq[(Ref[Post, Variable[Post]], Expr[Post])] = apply
          .givenMap.map { case (Ref(v), e) =>
            (rewriter.succ(v), rewriter.dispatch(e))
          },
        yields: Seq[(Expr[Post], Ref[Post, Variable[Post]])] = apply.yields
          .map { case (a, b) => (rewriter.dispatch(a), rewriter.succ(b.decl)) },
    ): AnyFunctionInvocation[Post] =
      apply match {
        case inv: FunctionInvocation[Pre] =>
          inv.rewrite(
            args = args,
            typeArgs = typeArgs,
            givenMap = givenMap,
            yields = yields,
          )
        case inv: InstanceFunctionInvocation[Pre] =>
          inv.rewrite(
            args = args,
            typeArgs = typeArgs,
            givenMap = givenMap,
            yields = yields,
          )
      }
  }

  implicit class ApplyAnyMethodBuildHelpers[Pre, Post](
      apply: AnyMethodInvocation[Pre]
  )(implicit rewriter: AbstractRewriter[Pre, Post]) {
    def rewrite(
        args: => Seq[Expr[Post]] = apply.args.map(rewriter.dispatch),
        outArgs: => Seq[Expr[Post]] = apply.outArgs.map(rewriter.dispatch),
        typeArgs: => Seq[Type[Post]] = apply.typeArgs.map(rewriter.dispatch),
        givenMap: => Seq[(Ref[Post, Variable[Post]], Expr[Post])] = apply
          .givenMap.map { case (Ref(v), e) =>
            (rewriter.succ(v), rewriter.dispatch(e))
          },
        yields: => Seq[(Expr[Post], Ref[Post, Variable[Post]])] = apply.yields
          .map { case (a, b) => (rewriter.dispatch(a), rewriter.succ(b.decl)) },
    ): AnyMethodInvocation[Post] =
      apply match {
        case inv: ProcedureInvocation[Pre] =>
          inv.rewrite(
            args = args,
            outArgs = outArgs,
            typeArgs = typeArgs,
            givenMap = givenMap,
            yields = yields,
          )
        case inv: MethodInvocation[Pre] =>
          inv.rewrite(
            args = args,
            outArgs = outArgs,
            typeArgs = typeArgs,
            givenMap = givenMap,
            yields = yields,
          )
        case inv: ConstructorInvocation[Pre] =>
          inv.rewrite(
            args = args,
            outArgs = outArgs,
            typeArgs = typeArgs,
            givenMap = givenMap,
            yields = yields,
          )
      }
  }

  implicit class InvocationStatementBuildHelpers[Pre, Post](
      apply: InvocationStatement[Pre]
  )(implicit rewriter: AbstractRewriter[Pre, Post]) {
    def rewrite(
        args: => Seq[Expr[Post]] = apply.args.map(rewriter.dispatch),
        outArgs: => Seq[Expr[Post]] = apply.outArgs.map(rewriter.dispatch),
        typeArgs: => Seq[Type[Post]] = apply.typeArgs.map(rewriter.dispatch),
        givenMap: => Seq[(Ref[Post, Variable[Post]], Expr[Post])] = apply
          .givenMap.map { case (Ref(v), e) =>
            (rewriter.succ(v), rewriter.dispatch(e))
          },
        yields: => Seq[(Expr[Post], Ref[Post, Variable[Post]])] = apply.yields
          .map { case (a, b) => (rewriter.dispatch(a), rewriter.succ(b.decl)) },
    ): InvocationStatement[Post] =
      apply match {
        case inv: InvokeProcedure[Pre] =>
          inv.rewrite(
            args = args,
            outArgs = outArgs,
            typeArgs = typeArgs,
            givenMap = givenMap,
            yields = yields,
          )
        case inv: InvokeMethod[Pre] =>
          inv.rewrite(
            args = args,
            outArgs = outArgs,
            typeArgs = typeArgs,
            givenMap = givenMap,
            yields = yields,
          )
        case inv: InvokeConstructor[Pre] =>
          inv.rewrite(
            args = args,
            outArgs = outArgs,
            typeArgs = typeArgs,
            givenMap = givenMap,
            yields = yields,
          )
      }
  }

  private def constOrigin(value: scala.Any): Origin =
    Origin(Seq(LabelContext(s"constant ${value}")))

  def tt[G]: BooleanValue[G] = BooleanValue(true)(constOrigin(true))
  def ff[G]: BooleanValue[G] = BooleanValue(false)(constOrigin(false))

  def const[G](i: Int)(implicit o: Origin): IntegerValue[G] = IntegerValue(i)

  def const[G](i: BigInt)(implicit o: Origin): IntegerValue[G] = IntegerValue(i)

  def c_const[G](i: Int)(implicit o: Origin): CIntegerValue[G] =
    CIntegerValue(i)

  def c_const[G](i: BigInt)(implicit o: Origin): CIntegerValue[G] =
    CIntegerValue(i)

  def contract[G](
      blame: Blame[NontrivialUnsatisfiable],
      requires: AccountedPredicate[G] =
        UnitAccountedPredicate(tt[G])(constOrigin(true)),
      ensures: AccountedPredicate[G] =
        UnitAccountedPredicate(tt[G])(constOrigin(true)),
      contextEverywhere: Expr[G] = tt[G],
      signals: Seq[SignalsClause[G]] = Nil,
      givenArgs: Seq[Variable[G]] = Nil,
      yieldsArgs: Seq[Variable[G]] = Nil,
      decreases: Option[DecreasesClause[G]] = None,
  )(implicit o: Origin): ApplicableContract[G] =
    ApplicableContract(
      requires,
      ensures,
      contextEverywhere,
      signals,
      givenArgs,
      yieldsArgs,
      decreases,
    )(blame)

  def loopInvariant[G](
      blame: Blame[LoopInvariantFailure],
      invariant: Expr[G] = null,
      decreases: Option[DecreasesClause[G]] = None,
  )(implicit o: Origin): LoopContract[G] =
    LoopInvariant(invariant, decreases)(blame)

  def withResult[G, T <: ContractApplicable[G]](
      builder: Result[G] => T
  )(implicit o: Origin): T = {
    val box = SuccessionMap[Unit, ContractApplicable[G]]()
    val result = Result[G](box.ref(()))
    val applicable = builder(result)
    box(()) = applicable
    applicable
  }

  def procedure[G](
      blame: Blame[CallableFailure],
      contractBlame: Blame[NontrivialUnsatisfiable],
      returnType: Type[G] = TVoid[G](),
      args: Seq[Variable[G]] = Nil,
      outArgs: Seq[Variable[G]] = Nil,
      typeArgs: Seq[Variable[G]] = Nil,
      body: Option[Statement[G]] = None,
      requires: AccountedPredicate[G] =
        UnitAccountedPredicate(tt[G])(constOrigin(true)),
      ensures: AccountedPredicate[G] =
        UnitAccountedPredicate(tt[G])(constOrigin(true)),
      contextEverywhere: Expr[G] = tt[G],
      signals: Seq[SignalsClause[G]] = Nil,
      givenArgs: Seq[Variable[G]] = Nil,
      yieldsArgs: Seq[Variable[G]] = Nil,
      decreases: Option[DecreasesClause[G]] = None,
      doInline: Boolean = false,
      pure: Boolean = false,
  )(implicit o: Origin): Procedure[G] =
    new Procedure(
      returnType,
      args,
      outArgs,
      typeArgs,
      body,
      ApplicableContract(
        requires,
        ensures,
        contextEverywhere,
        signals,
        givenArgs,
        yieldsArgs,
        decreases,
      )(contractBlame),
      doInline,
      pure,
    )(blame)

  def function[G](
      blame: Blame[ContractedFailure],
      contractBlame: Blame[NontrivialUnsatisfiable],
      returnType: Type[G] = TVoid(),
      args: Seq[Variable[G]] = Nil,
      typeArgs: Seq[Variable[G]] = Nil,
      body: Option[Expr[G]] = None,
      requires: AccountedPredicate[G] =
        UnitAccountedPredicate(tt[G])(constOrigin(true)),
      ensures: AccountedPredicate[G] =
        UnitAccountedPredicate(tt[G])(constOrigin(true)),
      contextEverywhere: Expr[G] = tt[G],
      signals: Seq[SignalsClause[G]] = Nil,
      givenArgs: Seq[Variable[G]] = Nil,
      yieldsArgs: Seq[Variable[G]] = Nil,
      decreases: Option[DecreasesClause[G]] = Some(
        DecreasesClauseNoRecursion[G]()(constOrigin("decreases"))
      ),
      doInline: Boolean = false,
  )(implicit o: Origin): Function[G] =
    new Function(
      returnType,
      args,
      typeArgs,
      body,
      ApplicableContract(
        requires,
        ensures,
        contextEverywhere,
        signals,
        givenArgs,
        yieldsArgs,
        decreases,
      )(contractBlame),
      doInline,
    )(blame)

  def functionInvocation[G](
      blame: Blame[InvocationFailure],
      ref: Ref[G, Function[G]],
      args: Seq[Expr[G]] = Nil,
      typeArgs: Seq[Type[G]] = Nil,
      givenMap: Seq[(Ref[G, Variable[G]], Expr[G])] = Nil,
      yields: Seq[(Expr[G], Ref[G, Variable[G]])] = Nil,
  )(implicit o: Origin): FunctionInvocation[G] =
    FunctionInvocation(ref, args, typeArgs, givenMap, yields)(blame)

  def methodInvocation[G](
      blame: Blame[InstanceInvocationFailure],
      obj: Expr[G],
      ref: Ref[G, InstanceMethod[G]],
      args: Seq[Expr[G]] = Nil,
      outArgs: Seq[Expr[G]] = Nil,
      typeArgs: Seq[Type[G]] = Nil,
      givenMap: Seq[(Ref[G, Variable[G]], Expr[G])] = Nil,
      yields: Seq[(Expr[G], Ref[G, Variable[G]])] = Nil,
  )(implicit o: Origin): MethodInvocation[G] =
    MethodInvocation(obj, ref, args, outArgs, typeArgs, givenMap, yields)(blame)

  def procedureInvocation[G](
      blame: Blame[InvocationFailure],
      ref: Ref[G, Procedure[G]],
      args: Seq[Expr[G]] = Nil,
      outArgs: Seq[Expr[G]] = Nil,
      typeArgs: Seq[Type[G]] = Nil,
      givenMap: Seq[(Ref[G, Variable[G]], Expr[G])] = Nil,
      yields: Seq[(Expr[G], Ref[G, Variable[G]])] = Nil,
  )(implicit o: Origin): ProcedureInvocation[G] =
    ProcedureInvocation(ref, args, outArgs, typeArgs, givenMap, yields)(blame)

  def constructorInvocation[G](
      blame: Blame[InvocationFailure],
      ref: Ref[G, Constructor[G]],
      classTypeArgs: Seq[Type[G]] = Nil,
      args: Seq[Expr[G]] = Nil,
      outArgs: Seq[Expr[G]] = Nil,
      typeArgs: Seq[Type[G]] = Nil,
      givenMap: Seq[(Ref[G, Variable[G]], Expr[G])] = Nil,
      yields: Seq[(Expr[G], Ref[G, Variable[G]])] = Nil,
  )(implicit o: Origin): ConstructorInvocation[G] =
    ConstructorInvocation(
      ref,
      classTypeArgs,
      args,
      outArgs,
      typeArgs,
      givenMap,
      yields,
    )(blame)

  private def GeneratedQuantifier: Origin =
    Origin(Seq(PreferredName(Seq("i")), LabelContext("generated quantifier")))

  def starall[G](
      blame: Blame[ReceiverNotInjective],
      t: Type[G],
      body: Local[G] => Expr[G],
      triggers: Local[G] => Seq[Seq[Expr[G]]] =
        (_: Local[G]) => Seq.empty[Seq[Expr[G]]],
  ): Starall[G] = {
    implicit val o: Origin = GeneratedQuantifier
    val i_var = new Variable[G](t)
    val i = Local[G](i_var.ref)
    Starall(bindings = Seq(i_var), triggers = triggers(i), body = body(i))(
      blame
    )
  }

  def forall[G](
      t: Type[G],
      body: Local[G] => Expr[G],
      triggers: Local[G] => Seq[Seq[Expr[G]]] =
        (_: Local[G]) => Seq.empty[Seq[Expr[G]]],
  ): Forall[G] = {
    implicit val o: Origin = GeneratedQuantifier
    val i_var = new Variable[G](t)
    val i = Local[G](i_var.ref)
    Forall(bindings = Seq(i_var), triggers = triggers(i), body = body(i))
  }

  def foralls[G](
      ts: Seq[Type[G]],
      body: Seq[Local[G]] => Expr[G],
      triggers: Seq[Local[G]] => Seq[Seq[Expr[G]]] =
        (_: Seq[Local[G]]) => Seq.empty[Seq[Expr[G]]],
  ): Forall[G] = {
    implicit val o: Origin = GeneratedQuantifier
    val i_vars: Seq[Variable[G]] = ts.map(new Variable[G](_))
    val is: Seq[Local[G]] = i_vars.map((x: Variable[G]) => Local[G](x.ref))
    Forall(bindings = i_vars, triggers = triggers(is), body = body(is))
  }

  private def GeneratedLet: Origin =
    Origin(Seq(PreferredName(Seq("x")), LabelContext("generated let")))

  def let[G](t: Type[G], x: Expr[G], body: Local[G] => Expr[G]): Let[G] = {
    implicit val o: Origin = GeneratedQuantifier
    val x_var: Variable[G] = new Variable[G](t)
    val x_local: Local[G] = Local(x_var.ref)
    Let(x_var, x, body(x_local))
  }

  def assignLocal[G](local: Local[G], value: Expr[G])(
      implicit o: Origin
  ): Assign[G] = Assign(local, value)(AssignLocalOk)

  def assignField[G](
      obj: Expr[G],
      field: Ref[G, InstanceField[G]],
      value: Expr[G],
      blame: Blame[AssignFailed],
  )(implicit o: Origin): Assign[G] =
    Assign(Deref(obj, field)(DerefAssignTarget), value)(blame)

  def fieldPerm[G](
      obj: Expr[G],
      field: Ref[G, InstanceField[G]],
      amount: Expr[G],
  )(implicit o: Origin): Perm[G] = Perm(FieldLocation(obj, field), amount)

  def value[G](obj: Expr[G], ref: Ref[G, InstanceField[G]])(
      implicit o: Origin
  ): Value[G] = Value(FieldLocation(obj, ref))

  def arrayPerm[G](
      arr: Expr[G],
      index: Expr[G],
      amount: Expr[G],
      arrayLocationError: Blame[ArrayLocationError],
  )(implicit o: Origin): Perm[G] =
    Perm(ArrayLocation(arr, index)(arrayLocationError), amount)

  def foldAnd[G](exprs: Iterable[Expr[G]])(implicit o: Origin): Expr[G] =
    exprs.reduceOption(And(_, _)).getOrElse(tt)

  def implies[G](conds: Seq[Expr[G]], body: Expr[G])(
      implicit o: Origin
  ): Expr[G] =
    conds match {
      case Nil => body
      case conds => Implies(foldAnd(conds), body)
    }

  def unfoldPredicate[G](p: AccountedPredicate[G]): Seq[Expr[G]] =
    p match {
      case UnitAccountedPredicate(pred) => Seq(pred)
      case SplitAccountedPredicate(left, right) =>
        unfoldPredicate(left) ++ unfoldPredicate(right)
    }

  def mapPredicate[G1, G2](
      p: AccountedPredicate[G1],
      f: Expr[G1] => Expr[G2],
  ): AccountedPredicate[G2] =
    p match {
      case UnitAccountedPredicate(pred) => UnitAccountedPredicate(f(pred))(p.o)
      case SplitAccountedPredicate(left, right) =>
        SplitAccountedPredicate(mapPredicate(left, f), mapPredicate(right, f))(
          p.o
        )
    }

  implicit class AccountedRewriteHelpers[Pre, Post](
      f: Expr[Pre] => Expr[Post]
  ) {
    def accounted: AccountedPredicate[Pre] => AccountedPredicate[Post] =
      p => mapPredicate(p, f)
  }

  def unfoldImplies[G](expr: Expr[G]): (Seq[Expr[G]], Expr[G]) =
    expr match {
      case Implies(left, right) =>
        val (antecedent, consequent) = AstBuildHelpers.unfoldImplies(right)
        (unfoldStar(left) ++ antecedent, consequent)
      case other => (Nil, other)
    }

  def unfoldStar[G](expr: Expr[G]): Seq[Expr[G]] =
    expr match {
      case Star(left, right) =>
        AstBuildHelpers.unfoldStar(left) ++ AstBuildHelpers.unfoldStar(right)
      case And(left, right) =>
        AstBuildHelpers.unfoldStar(left) ++ AstBuildHelpers.unfoldStar(right)
      case BooleanValue(true) => Nil
      case other => Seq(other)
    }

  // For if you want to be fussy about origins
  def mapUnfoldedStar[G1, G2](
      expr: Expr[G1],
      f: Expr[G1] => Expr[G2],
  ): Expr[G2] =
    expr match {
      case Star(left, right) =>
        Star(mapUnfoldedStar(left, f), mapUnfoldedStar(right, f))(expr.o)
      case And(left, right) =>
        And(mapUnfoldedStar(left, f), mapUnfoldedStar(right, f))(expr.o)
      case other => f(other)
    }

  def foldStar[G](exprs: Seq[Expr[G]])(implicit o: Origin): Expr[G] =
    exprs.reduceOption(Star(_, _)).getOrElse(tt)

  def foldStar[G](
      predicate: AccountedPredicate[G]
  )(implicit o: Origin): Expr[G] =
    predicate match {
      case UnitAccountedPredicate(pred) => pred
      case SplitAccountedPredicate(left, right) =>
        Star(foldStar(left), foldStar(right))
    }

  def foldPredicate[G](
      exprs: Seq[Expr[G]]
  )(implicit o: Origin): AccountedPredicate[G] =
    exprs match {
      case x :: Seq() => UnitAccountedPredicate(x)
      case x :: xs =>
        SplitAccountedPredicate(UnitAccountedPredicate(x), foldPredicate(xs))
    }

  def foldOr[G](exprs: Seq[Expr[G]])(implicit o: Origin): Expr[G] =
    exprs.reduceOption(Or(_, _)).getOrElse(ff)

  def foldAnd[G](exprs: Seq[Expr[G]])(implicit o: Origin): Expr[G] =
    exprs.reduceOption(And(_, _)).getOrElse(tt)

  def foldAny[G](t: Type[_])(exprs: Seq[Expr[G]])(implicit o: Origin): Expr[G] =
    t match {
      case TBool() => foldAnd(exprs)
      case TResource() => foldStar(exprs)
      case _ => ???
    }

  def loop[G](
      cond: Expr[G],
      body: Statement[G],
      init: Statement[G] = null,
      update: Statement[G] = null,
      contract: LoopContract[G] = null,
  )(implicit o: Origin): Loop[G] =
    Loop(
      init = Option(init).getOrElse(Block(Seq())),
      contract = Option(contract)
        .getOrElse(loopInvariant(blame = PanicBlame("Trivial contract"))),
      cond = cond,
      body = body,
      update = Option(update).getOrElse(Block(Seq())),
    )

  def instanceField[G](t: Type[G], isFinal: Boolean = false)(
      implicit o: Origin
  ): InstanceField[G] =
    new InstanceField(
      t,
      if (isFinal)
        Seq(Final())
      else
        Seq(),
    )

  def skip[G](implicit o: Origin): Block[G] = Block(Seq())
}
