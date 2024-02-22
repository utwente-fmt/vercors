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
                                  keepBodies: Boolean,
                                  substitutions: Map[TVar[Pre], Type[Pre]]
                                  ) {
    def key = (cls, typeValues)
    def substitute = Substitute(Map.empty[Expr[Pre], Expr[Pre]], typeSubs = substitutions)

    def evalType(t: Type[Pre]): Type[Pre] = substitute.dispatch(t)
    def evalTypes(ts: Seq[Type[Pre]]): Seq[Type[Pre]] = ts.map(evalType)
  }
  val ctx: ScopedStack[InstantiationContext] = ScopedStack()

  def keepBodies: Boolean = ctx.topOption.map { ctx => ctx.keepBodies }.getOrElse(true)

  // key: generically instantiated type in pre
  val genericSucc: SuccessionMap[(Key, Declaration[Pre]), Declaration[Post]] = SuccessionMap()

  def instantiate(cls: Class[Pre], typeValues: Seq[Type[Pre]], keepBodies: Boolean): Unit = {
    val key = (cls, typeValues)
    genericSucc.get((key, cls)) match {
      case Some(_) => // Done
      case None =>
        val newCtx = InstantiationContext(
          cls,
          typeValues,
          keepBodies = keepBodies,
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
      instantiate(cls, cls.typeArgs.map(v => v.t.asInstanceOf[TType[Pre]].t), true)
    case method: InstanceMethod[Pre] if ctx.nonEmpty =>
      val newMethod: InstanceMethod[Post] =
        method.rewrite(body = if(keepBodies) method.body.map(dispatch) else None)
      genericSucc((ctx.top.key, method)) = newMethod
      classDeclarations.declare(newMethod)
      classDeclarations.succeedOnly(method, newMethod)
    case cons: Constructor[Pre] if ctx.nonEmpty =>
      val newCons = cons.rewrite(body = if(keepBodies) cons.body.map(dispatch) else None)
      genericSucc((ctx.top.key, cons)) = newCons
      classDeclarations.declare(newCons)
      classDeclarations.succeedOnly(cons, newCons)
    case other =>
      allScopes.anySucceed(other, other.rewriteDefault())
  }

  override def dispatch(t: Type[Pre]): Type[Post] = (t, ctx.topOption) match {
    case (TClass(Ref(cls), typeArgs), ctx) if typeArgs.nonEmpty =>
      val typeValues = ctx match {
        case Some(ctx) => typeArgs.map(ctx.substitute.dispatch)
        case None => typeArgs
      }
      instantiate(cls, typeValues, false)
      TClass[Post](genericSucc.ref[Post, Class[Post]](((cls, typeValues), cls)), Seq())
    case (tvar @ TVar(_), Some(ctx)) =>
      dispatch(ctx.substitutions(tvar))
    case _ => t.rewriteDefault()
  }

  override def dispatch(stmt: Statement[Pre]): Statement[Post] = stmt match {
    case inv: InvokeConstructor[Pre] if inv.classTypeArgs.nonEmpty =>
      val cls = inv.ref.decl.cls.decl
      val typeValues = ctx.topOption.map(_.evalTypes(inv.classTypeArgs)).getOrElse(inv.classTypeArgs)
      instantiate(cls, typeValues, false)
      inv.rewrite(
        ref = genericSucc.ref[Post, Constructor[Post]](((cls, typeValues), inv.ref.decl))
      )
    case inv: InvokeMethod[Pre]  =>
      inv.obj.t match {
        case TClass(Ref(cls), typeArgs) if typeArgs.nonEmpty =>
          val typeValues = ctx.topOption.map(_.evalTypes(typeArgs)).getOrElse(typeArgs)
          instantiate(cls, typeValues, false)
          inv.rewrite(
            ref = genericSucc.ref[Post, InstanceMethod[Post]](((cls, typeValues), inv.ref.decl))
          )
        case _ => inv.rewriteDefault()
      }
    case other => other.rewriteDefault()
  }
}
