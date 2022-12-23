package vct.col.rewrite

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.col.ast.RewriteHelpers._
import vct.col.origin.{AbstractApplicable, Origin, PanicBlame, TrueSatisfiable}
import vct.col.ref.Ref
import vct.col.rewrite.ConstantifyFinalFields.FinalFieldPerm
import vct.col.util.SuccessionMap
import vct.result.VerificationError.UserError

case object ConstantifyFinalFields extends RewriterBuilder {
  override def key: String = "constantFinalFields"
  override def desc: String = "Encode final fields with functions, so that they are not on the heap."

  case class FinalFieldPerm(loc: FieldLocation[_]) extends UserError {
    override def code: String = "finalFieldPerm"
    override def text: String =
      loc.o.messageInContext("Specifying permission over final fields is not allowed, since they are treated as constants.")
  }
}

case class ConstantifyFinalFields[Pre <: Generation]() extends Rewriter[Pre] {
  val currentClass: ScopedStack[Class[Pre]] = ScopedStack()
  val fieldFunction: SuccessionMap[InstanceField[Pre], Function[Post]] = SuccessionMap()

  def isFinal(field: InstanceField[Pre]): Boolean =
    field.flags.collectFirst { case _: Final[Pre] => () }.isDefined

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case cls: Class[Pre] =>
      currentClass.having(cls) { rewriteDefault(cls) }
    case field: InstanceField[Pre] =>
      implicit val o: Origin = field.o
      if(isFinal(field)) {
        fieldFunction(field) = globalDeclarations.declare(function[Post](
          blame = AbstractApplicable,
          contractBlame = TrueSatisfiable,
          returnType = dispatch(field.t),
          args = Seq(new Variable[Post](TClass(succ(currentClass.top)))),
        ))
      } else {
        rewriteDefault(field)
      }
    case other => rewriteDefault(other)
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case Deref(obj, Ref(field)) =>
      implicit val o: Origin = e.o
      if(isFinal(field)) FunctionInvocation[Post](fieldFunction.ref(field), Seq(dispatch(obj)), Nil, Nil, Nil)(PanicBlame("requires nothing"))
      else rewriteDefault(e)
    case other => rewriteDefault(other)
  }

  override def dispatch(location: Location[Pre]): Location[Post] = location match {
    case loc @ FieldLocation(_, Ref(field)) if isFinal(field) =>
      throw FinalFieldPerm(loc)
    case other => rewriteDefault(other)
  }

  def makeInhale(obj: Expr[Pre], field: InstanceField[Pre], value: Expr[Pre])(implicit o: Origin): Statement[Post] =
    Assume(FunctionInvocation[Post](fieldFunction.ref(field), Seq(dispatch(obj)), Nil, Nil, Nil)(PanicBlame("requires nothing")) === dispatch(value))

  override def dispatch(stat: Statement[Pre]): Statement[Post] = stat match {
    case Assign(Deref(obj, Ref(field)), value) if isFinal(field) => makeInhale(obj, field, value)(stat.o)
    case Eval(PreAssignExpression(Deref(obj, Ref(field)), value)) if isFinal(field) => makeInhale(obj, field, value)(stat.o)
    case other => rewriteDefault(other)
  }
}
