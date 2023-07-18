package vct.rewrite

import vct.col.ast._
import vct.col.origin.{AbstractApplicable, Origin, PanicBlame, TrueSatisfiable}
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.AstBuildHelpers.{function, functionInvocation}
import vct.col.util.SuccessionMap

case object HeapVariableToRef extends RewriterBuilder {
  override def key: String = "heapVarToRef"
  override def desc: String = "Translate global heap variables to the field of a constant Ref"

  case object GlobalsOrigin extends Origin {
    override def preferredName: String = "globals"
    override def context: String = "At: [globals]"
    override def inlineContext: String = "[globals]"
    override def shortPosition: String = "generated"
  }
}

case class HeapVariableToRef[Pre <: Generation]() extends Rewriter[Pre] {
  import HeapVariableToRef._

  lazy val globalsFunction: Function[Post] = {
    implicit val o: Origin = GlobalsOrigin
    globalDeclarations.declare(function[Post](AbstractApplicable, TrueSatisfiable, returnType = TRef()))
  }

  def globals(implicit o: Origin): Expr[Post] =
    functionInvocation[Post](PanicBlame("Precondition is `true`"), globalsFunction.ref)

  val heapVariableField: SuccessionMap[HeapVariable[Pre], SilverField[Post]] = SuccessionMap()

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case v: HeapVariable[Pre] =>
      heapVariableField(v) = globalDeclarations.declare(new SilverField(dispatch(v.t))(v.o))
    case other => rewriteDefault(other)
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case hv @ DerefHeapVariable(Ref(v)) =>
      SilverDeref[Post](globals(e.o), heapVariableField.ref(v))(hv.blame)(e.o)
    case other => rewriteDefault(other)
  }

  override def dispatch(location: Location[Pre]): Location[Post] = location match {
    case HeapVariableLocation(Ref(v)) =>
      SilverFieldLocation[Post](globals(location.o), heapVariableField.ref(v))(location.o)
    case other => rewriteDefault(other)
  }
}
