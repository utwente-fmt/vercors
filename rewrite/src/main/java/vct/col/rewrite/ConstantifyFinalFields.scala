package vct.col.rewrite

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.col.ast.RewriteHelpers._
import vct.col.origin.{AbstractApplicable, Origin, PanicBlame, TrueSatisfiable}
import vct.col.ref.Ref
import vct.col.util.SuccessionMap

case object ConstantifyFinalFields extends RewriterBuilder {
  override def key: String = "constantFinalFields"
  override def desc: String = "Encode final fields with functions, so that they are not on the heap."
}

case class ConstantifyFinalFields[Pre <: Generation]() extends Rewriter[Pre] {
  val currentClass: ScopedStack[Class[Pre]] = ScopedStack()
  var finalValueMap: Map[Declaration[Pre], Expr[Pre]] = Map()
  val fieldFunction: SuccessionMap[InstanceField[Pre], Function[Post]] = SuccessionMap()

  def isFinal(field: InstanceField[Pre]): Boolean =
    field.flags.collectFirst { case _: Final[Pre] => () }.isDefined

  // This function is deliberately unclearly called isAllowedValue to avoid making the impression that we are implementing
  // java constexprs or something similar.
  // Below just happens to be the subset needed to encode string literals.
  def isAllowedValue(e: Expr[Pre]): Boolean = e match {
    case IntegerValue(_) => true
    case LiteralSeq(_, vals) => vals.forall(isAllowedValue)
    case FunctionInvocation(_, args, _, givenMap, _) => args.forall(isAllowedValue) && givenMap.forall { case (_, e) => isAllowedValue(e) }
  }

  override def dispatch(decl: Program[Pre]): Program[Post] = {
    finalValueMap = decl.transSubnodes.collect({
      // Note that we don't check the value of deref here, so if isClosedConstant is extended without care, this
      // might produce unsoundness in the future. E.g. if variables are present in the init value, this approach fails
      case Assign(Deref(_, Ref(field)), value) if isFinal(field) && isAllowedValue(value) =>
        (field, value)
    }).toMap

    super.dispatch(decl)
  }

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case cls: Class[Pre] =>
      currentClass.having(cls) { rewriteDefault(cls) }
    case field: InstanceField[Pre] =>
      implicit val o: Origin = field.o
      if(isFinal(field)) {
        fieldFunction(field) = globalDeclarations.declare(
          withResult((result: Result[Post]) => function[Post](
            blame = AbstractApplicable,
            contractBlame = TrueSatisfiable,
            returnType = dispatch(field.t),
            args = Seq(new Variable[Post](TClass(succ(currentClass.top)))),
            ensures = UnitAccountedPredicate(finalValueMap.get(field) match {
              case Some(value) => result === rewriteDefault(value)
              case None => tt[Post]
            })
          ))
        )
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

  override def dispatch(stat: Statement[Pre]): Statement[Post] = stat match {
    case Assign(Deref(obj, Ref(field)), value) =>
      implicit val o: Origin = stat.o
      if(isFinal(field)) {
        if (finalValueMap.contains(field)) {
          Block(Nil)
        } else {
          Inhale(FunctionInvocation[Post](fieldFunction.ref(field), Seq(dispatch(obj)), Nil, Nil, Nil)(PanicBlame("requires nothing")) === dispatch(value))
        }
      } else {
        rewriteDefault(stat)
      }
    case other => rewriteDefault(other)
  }
}
