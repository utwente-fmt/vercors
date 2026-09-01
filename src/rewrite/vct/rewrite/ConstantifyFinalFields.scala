package vct.col.rewrite

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.col.origin.{
  AbstractApplicable,
  Blame,
  ClassDerefError,
  ClassNull,
  InvocationFailure,
  Origin,
  PanicBlame,
  TrueSatisfiable,
}
import vct.col.ref.Ref
import vct.col.util.SuccessionMap
import vct.result.VerificationError.UserError

case object ConstantifyFinalFields extends RewriterBuilder {
  override def key: String = "constantFinalFields"
  override def desc: String =
    "Encode final fields with functions, so that they are not on the heap."

  private case class FinalFieldPerm(loc: FieldLocation[_]) extends UserError {
    override def code: String = "finalFieldPerm"
    override def text: String =
      loc.o.messageInContext(
        "Specifying permission over final fields is not allowed, since they are treated as constants."
      )
  }

  private case class FieldClassNull(
      blame: Blame[ClassDerefError],
      deref: HeapDeref[_],
  ) extends Blame[InvocationFailure] {
    override def blame(error: InvocationFailure): Unit =
      blame.blame(ClassNull(deref))
  }
}

case class ConstantifyFinalFields[Pre <: Generation]() extends Rewriter[Pre] {
  import vct.col.rewrite.ConstantifyFinalFields._

  val currentClass: ScopedStack[Class[Pre]] = ScopedStack()
  var finalValueMap: Map[Declaration[Pre], Expr[Pre]] = Map()
  val fieldFunction: SuccessionMap[InstanceField[Pre], Function[Post]] =
    SuccessionMap()
  val substituteThisObject: ScopedStack[Expr[Post]] = ScopedStack()

  def isFinal(field: InstanceField[Pre]): Boolean =
    field.flags.collectFirst { case _: Final[Pre] => () }.isDefined

  // This function is deliberately unclearly called isAllowedValue to avoid making the impression that we are implementing
  // java constexprs or something similar.
  // Below just happens to be the subset needed to encode string literals.
  def isAllowedValue(e: Expr[Pre]): Boolean =
    e match {
      case ThisObject(_) => true
      case IntegerValue(_) => true
      case LiteralSeq(_, vals) => vals.forall(isAllowedValue)
      case FunctionInvocation(func, args, _, givenMap, _, _) =>
        func.decl.contract.decreases.isDefined &&
        func.decl.contract.contextEverywhere.t.equals(TBool[Pre]()) &&
        unfoldPredicate(func.decl.contract.requires)
          .forall(_.t == TBool[Pre]()) && args.forall(isAllowedValue) &&
        givenMap.forall { case (_, e) => isAllowedValue(e) }
      case InstanceFunctionInvocation(obj, func, args, _, givenMap, Seq()) =>
        func.decl.contract.decreases.isDefined &&
        func.decl.contract.contextEverywhere.t == TBool[Pre]() &&
        unfoldPredicate(func.decl.contract.requires)
          .forall(_.t == TBool[Pre]()) && isAllowedValue(obj) &&
        args.forall(isAllowedValue) && givenMap.forall { case (_, e) =>
          isAllowedValue(e)
        }
      case _ => false
    }

  override def dispatch(decl: Program[Pre]): Program[Post] = {
    finalValueMap =
      decl.collect {
        // Note that we don't check the value of deref here, so if isClosedConstant is extended without care, this
        // might produce unsoundness in the future. E.g. if variables are present in the init value, this approach fails
        case Assign(Deref(_, Ref(field)), value)
            if isFinal(field) && isAllowedValue(value) =>
          (field, value)
      }.toMap

    super.dispatch(decl)
  }

  override def dispatch(decl: Declaration[Pre]): Unit =
    decl match {
      case cls: Class[Pre] => currentClass.having(cls) { super.dispatch(cls) }
      case field: InstanceField[Pre] if isFinal(field) =>
        implicit val o: Origin = field.o
        val `this` =
          new Variable(dispatch(
            currentClass.top.classType(currentClass.top.typeArgs.map {
              v: Variable[Pre] => TVar(v.ref)
            })
          ))
        fieldFunction(field) = globalDeclarations
          .declare(withResult((result: Result[Post]) =>
            function[Post](
              blame = AbstractApplicable,
              contractBlame = TrueSatisfiable,
              returnType = dispatch(field.t),
              args = Seq(`this`),
              requires = UnitAccountedPredicate(`this`.get !== Null()),
              ensures = UnitAccountedPredicate(finalValueMap.get(field) match {
                case Some(value) =>
                  result === substituteThisObject.having(`this`.get) {
                    super.dispatch(value)
                  }
                case None => tt[Post]
              }),
            )
          ))
      case other => super.dispatch(other)
    }

  override def dispatch(e: Expr[Pre]): Expr[Post] =
    e match {
      case ThisObject(_) if substituteThisObject.nonEmpty =>
        substituteThisObject.top
      case d @ Deref(obj, Ref(field)) if isFinal(field) =>
        implicit val o: Origin = e.o
        functionInvocation[Post](
          FieldClassNull(d.blame, d),
          fieldFunction.ref(field),
          Seq(dispatch(obj)),
        )
      case _ => super.dispatch(e)
    }

  override def dispatch(location: Location[Pre]): Location[Post] =
    location match {
      case loc @ FieldLocation(_, Ref(field)) if isFinal(field) =>
        throw FinalFieldPerm(loc)
      case _ => super.dispatch(location)
    }

  def makeInhale(
      obj: Expr[Pre],
      field: InstanceField[Pre],
      value: Expr[Pre],
      deref: Deref[Pre],
  )(implicit o: Origin): Statement[Post] =
    Assume(
      functionInvocation[Post](
        FieldClassNull(deref.blame, deref),
        fieldFunction.ref(field),
        Seq(dispatch(obj)),
      ) === dispatch(value)
    )

  override def dispatch(stat: Statement[Pre]): Statement[Post] =
    stat match {
      case Assign(Deref(obj, Ref(field)), value)
          if isFinal(field) && finalValueMap.contains(field) =>
        Block(Nil)(stat.o)
      case Eval(PreAssignExpression(Deref(obj, Ref(field)), value))
          if isFinal(field) && finalValueMap.contains(field) =>
        Block(Nil)(stat.o)
      case Assign(d @ Deref(obj, Ref(field)), value) if isFinal(field) =>
        makeInhale(obj, field, value, d)(stat.o)
      case Eval(PreAssignExpression(d @ Deref(obj, Ref(field)), value))
          if isFinal(field) =>
        makeInhale(obj, field, value, d)(stat.o)
      case _ => super.dispatch(stat)
    }
}
