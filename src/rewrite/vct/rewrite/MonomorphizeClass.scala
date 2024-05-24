package vct.rewrite

import com.typesafe.scalalogging.LazyLogging
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

case class MonomorphizeClass[Pre <: Generation]() extends Rewriter[Pre] with LazyLogging {
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

  val knownInstantiations: mutable.LinkedHashSet[Key] = mutable.LinkedHashSet()
  // key: generically instantiated type in pre
  val genericSucc: SuccessionMap[(Key, Declaration[Pre]), Declaration[Post]] = SuccessionMap()

  def instantiate(cls: Class[Pre], typeValues: Seq[Type[Pre]], keepBodies: Boolean): Unit = {
    /* Known limitation: the knownInstantations set does not take into account how a class was instantiated.
    A class can be instantiated both abstractly (without method bodies) and concretely (with method bodies)
    for the same sequence of type arguments, maybe. If that's the case, the knownInstantiations should take the
    "mode" of instantiation into account. But as this difference is at the moment also not encoded in genericSucc,
    it's not taken into account for knowInstantiations either
     */
    val key = (cls, typeValues)
    if (knownInstantiations.contains(key)) {
      logger.debug(s"Class ${cls.o.getPreferredNameOrElse().ucamel} with type args $typeValues is already instantiated, so skipping instantiation")
      return
    }
    val mode = if (keepBodies) { "concretely" } else { "abstractly" }
    logger.debug(s"Instantiating class ${cls.o.getPreferredNameOrElse().ucamel} $mode, args: $typeValues")
    val newCtx = InstantiationContext(
      cls,
      typeValues,
      keepBodies = keepBodies,
      substitutions = cls.typeArgs.map { v: Variable[Pre] => TVar(v.ref[Variable[Pre]]) }.zip(typeValues).toMap
    )
    knownInstantiations.add(key)
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
    case field: InstanceField[Pre] if ctx.nonEmpty =>
      val newField = field.rewrite()
      genericSucc((ctx.top.key, field)) = newField
      classDeclarations.declare(newField)
      classDeclarations.succeedOnly(field, newField)
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

  override def dispatch(loc: Location[Pre]): Location[Post] = loc match {
    case loc @ FieldLocation(obj, Ref(field)) =>
      obj.t match {
        case TClass(Ref(cls), typeArgs) if typeArgs.nonEmpty =>
          val typeArgs1 = ctx.topOption.map(_.evalTypes(typeArgs)).getOrElse(typeArgs)
          instantiate(cls, typeArgs1, false)
          loc.rewrite(
            field = genericSucc.ref[Post, InstanceField[Post]](((cls, typeArgs1), field))
          )
        case _ => loc.rewriteDefault()
      }
    case _ => loc.rewriteDefault()
  }

  override def dispatch(expr: Expr[Pre]): Expr[Post] = expr match {
    case deref @ Deref(obj, Ref(field)) =>
      obj.t match {
        case TClass(Ref(cls), typeArgs) if typeArgs.nonEmpty =>
          val typeArgs1 = ctx.topOption.map(_.evalTypes(typeArgs)).getOrElse(typeArgs)
          instantiate(cls, typeArgs1, false)
          deref.rewrite(
            ref = genericSucc.ref[Post, InstanceField[Post]](((cls, typeArgs1), field))
          )
        case _ => deref.rewriteDefault()
      }
    case _ => expr.rewriteDefault()
  }
}
