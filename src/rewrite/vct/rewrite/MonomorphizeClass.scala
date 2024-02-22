package vct.rewrite

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.ref.{LazyRef, Ref}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.AstBuildHelpers.ContractApplicableBuildHelpers
import vct.col.util.{Substitute, SuccessionMap}

import scala.collection.mutable

case object MonomorphizeClass extends RewriterBuilder {
  override def key: String = "monomorphizeClass"
  override def desc: String = "Monomorphize generic classes"
}

case class MonomorphizeClass[Pre <: Generation]() extends Rewriter[Pre] {
  val currentSubstitutions: ScopedStack[Map[Variable[Pre], Type[Pre]]] = ScopedStack()

  type Key = (Class[Pre], Seq[Type[Pre]])
  case class InstantiationContext(cls: Class[Pre],
                                  typeValues: Seq[Type[Pre]],
                                  removeBodies: Boolean,
                                  substitutions: Map[TVar[Pre], Type[Pre]]
                                  ) {
    def key = (cls, typeValues)
    def substitute = Substitute(Map.empty[Expr[Pre], Expr[Pre]], typeSubs = substitutions)
  }
  val ctx: ScopedStack[InstantiationContext] = ScopedStack()

  // key: generically instantiated type in pre
  val genericSucc: SuccessionMap[(Key, Declaration[Pre]), Declaration[Post]] = SuccessionMap()

  def instantiate(cls: Class[Pre], typeValues: Seq[Type[Pre]]): Unit = {
    val key = (cls, typeValues)
    genericSucc.get((key, cls)) match {
      case Some(ref) => // Done
      case None =>
        val newCtx = InstantiationContext(
          cls,
          typeValues,
          removeBodies = false,
          substitutions = cls.typeArgs.map { v: Variable[Pre] => TVar(v.ref[Variable[Pre]]) }.zip(typeValues).toMap
        )
        genericSucc((key, cls)) = ctx.having(newCtx) {
          globalDeclarations.scope {
            classDeclarations.scope {
              variables.scope {
                allScopes.anyDeclare(allScopes.anySucceedOnly(cls, cls.rewrite(typeArgs = Seq())))
              }
            }
          }
        }
    }
  }


  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case cls: Class[Pre] if cls.typeArgs.nonEmpty =>
      cls.typeArgs.foreach(_.drop())
      instantiate(cls, cls.typeArgs.map(v => v.t.asInstanceOf[TType[Pre]].t))
    case m: InstanceMethod[Pre] if ctx.nonEmpty =>
      val `m'` = classDeclarations.collect(super.dispatch(m))._1.head
      genericSucc((ctx.top.key, m)) = `m'`
    case other =>
      allScopes.anySucceed(other, other.rewriteDefault())
  }

  override def dispatch(t: Type[Pre]): Type[Post] = t match {
    case TClass(Ref(cls), typeArgs) if typeArgs.nonEmpty =>
      val typeValues = typeArgs.map(ctx.top.substitute.dispatch)
      instantiate(cls, typeValues)
      TClass[Post](genericSucc.ref(((cls, typeValues), cls)).asInstanceOf, Seq())
    case TVar(Ref(v)) =>
      ??? // Don't think this should occur anymore?
//      currentSubstitutions.top(v)
    case _ => t.rewriteDefault()
  }

  def evalType(t: Type[Pre]): Type[Pre] = ctx.top.substitute.dispatch(t)

  def evalTypes(ts: Seq[Type[Pre]]): Seq[Type[Pre]] = ts.map(evalType)

  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = e match {
    case inv: MethodInvocation[Pre] =>
      inv.obj.t match {
        case TClass(Ref(cls), typeArgs) if typeArgs.nonEmpty =>
          val typeValues = evalTypes(typeArgs)
          instantiate(cls, typeValues)
          inv.rewrite(
            ref = genericSucc.ref(((cls, typeValues), inv.ref.decl)).asInstanceOf
          )
        case _ => inv.rewriteDefault()
      }
    case inv: ConstructorInvocation[Pre] => ???
    case inv: InstanceFunctionInvocation[Pre] => ???
    case inv: InstancePredicateApply[Pre] => ???
    case other => other.rewriteDefault()
  }
}
