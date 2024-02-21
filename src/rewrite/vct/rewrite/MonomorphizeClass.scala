package vct.rewrite

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.ref.{LazyRef, Ref}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.AstBuildHelpers.ContractApplicableBuildHelpers

import scala.collection.mutable

case object MonomorphizeClass extends RewriterBuilder {
  override def key: String = "monomorphizeClasses"
  override def desc: String = "Monomorphize generic classes"
}

case class MonomorphizeClass[Pre <: Generation]() extends Rewriter[Pre] {
  val currentSubstitutions: ScopedStack[Map[Variable[Pre], Type[Post]]] = ScopedStack()

  val monomorphizedRef: mutable.Map[(Class[Pre], Seq[Type[Post]]), Ref[Post, Class[Post]]] = mutable.Map()
  val monomorphizedImpl: mutable.Map[(Class[Pre], Seq[Type[Post]]), Class[Post]] = mutable.Map()

  def getOrBuild(cls: Class[Pre], typeValues: Seq[Type[Post]]): Ref[Post, Class[Post]] =
    monomorphizedRef.get((cls, typeValues)) match {
      case Some(ref) => ref
      case None =>
        monomorphizedRef((cls, typeValues)) = new LazyRef(monomorphizedImpl((cls, typeValues)))
        monomorphizedImpl((cls, typeValues)) = currentSubstitutions.having(cls.typeArgs.zip(typeValues).toMap) {
          globalDeclarations.scope {
            classDeclarations.scope {
              variables.scope {
                allScopes.anyDeclare(allScopes.anySucceedOnly(cls, cls.rewrite(typeArgs = Seq())))
              }
            }
          }
        }
        monomorphizedRef((cls, typeValues))
    }


  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case cls: Class[Pre] if cls.typeArgs.nonEmpty => ???
//    case app: ContractApplicable[Pre] if app.typeArgs.nonEmpty =>
//      app.typeArgs.foreach(_.drop())
//      getOrBuild(app, app.typeArgs.map(v => dispatch(v.t.asInstanceOf[TType[Pre]].t)))
    case other => other.rewriteDefault()
  }

  /*
  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = e match {
    case inv: Invocation[Pre] if inv.ref.decl.typeArgs.nonEmpty =>
      val typeValues = inv.typeArgs.map(dispatch)
      val ref = getOrBuild(inv.ref.decl, typeValues)

      inv match {
        case inv: ProcedureInvocation[Pre] => inv.rewrite(ref = ref.asInstanceOf[Ref[Post, Procedure[Post]]], typeArgs = Nil)
        case inv: MethodInvocation[Pre] => inv.rewrite(ref = ref.asInstanceOf[Ref[Post, InstanceMethod[Post]]], typeArgs = Nil)
        case inv: FunctionInvocation[Pre] => inv.rewrite(ref = ref.asInstanceOf[Ref[Post, Function[Post]]], typeArgs = Nil)
        case inv: InstanceFunctionInvocation[Pre] => inv.rewrite(ref = ref.asInstanceOf[Ref[Post, InstanceFunction[Post]]], typeArgs = Nil)
      }
    case other => rewriteDefault(other)
  }
   */

  override def dispatch(t: Type[Pre]): Type[Rewritten[Pre]] = t match {
    case TVar(Ref(v)) =>
      currentSubstitutions.top(v)
    case other => other.rewriteDefault()
  }
}
