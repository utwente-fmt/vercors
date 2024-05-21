package vct.col.resolve.ctx

import vct.col.ast._
import vct.col.check.CheckContext
import vct.col.origin.DiagnosticOrigin
import vct.col.resolve.Resolve.{SpecContractParser, SpecExprParser}
import vct.col.util.SuccessionMap

import scala.collection.immutable.{HashMap, ListMap}
import scala.collection.mutable

case class ReferenceResolutionContext[G]
(
  javaParser: SpecExprParser,
  llvmSpecParser: SpecContractParser,
  stack: Seq[Seq[Referrable[G]]] = Nil,
  topLevelJavaDeref: Option[JavaDeref[G]] = None,
  externallyLoadedElements: mutable.ArrayBuffer[GlobalDeclaration[G]] = mutable.ArrayBuffer[GlobalDeclaration[G]](),
  checkContext: CheckContext[G] = CheckContext[G](),
  currentJavaNamespace: Option[JavaNamespace[G]] = None,
  currentJavaClass: Option[JavaClassOrInterface[G]] = None,
  currentThis: Option[ThisTarget[G]] = None,
  currentResult: Option[ResultTarget[G]] = None,
  currentInitializerType: Option[Type[G]] = None,
  inStaticJavaContext: Boolean = false,
  inGpuKernel: Boolean = false,
  javaBipStatePredicates: ListMap[Expr[G], JavaAnnotation[G]] = ListMap[Expr[G], JavaAnnotation[G]](),
  javaBipGuards: ListMap[Expr[G], JavaMethod[G]] = ListMap[Expr[G], JavaMethod[G]](),
  // When true and resolving a local, guard names should also be considered
  javaBipGuardsEnabled: Boolean = false,
  typeEnv: Map[Variable[G], Type[G]] = Map.empty[Variable[G], Type[G]],
) {
  def asTypeResolutionContext: TypeResolutionContext[G] =
    TypeResolutionContext(stack, currentJavaNamespace, None, Nil, inGpuKernel, externallyLoadedElements)

  def declare(decls: Seq[Declaration[G]]): ReferenceResolutionContext[G] =
    copy(stack = decls.flatMap(Referrable.from) +: stack)

  def withCheckContext(ctx: CheckContext[G]): ReferenceResolutionContext[G] = {
    val filter = ctx.undeclared.flatten.flatMap(Referrable.from).toSet
    copy(
      checkContext = ctx,
      stack = stack.map(_.filter(!filter.contains(_)))
    )
  }

  /* State predicates names are saved as expr's here because at the time this method is called, fields are not yet resolved.
     E.g. if in the code it says "@Guard(name = INIT)", where INIT is some static field, in the ast INIT is a LocalVariable.
     It is only after recursively visiting this annotation that INIT is resolved to a static field. So we save the expr
     here so we can later use the resolved value, instead of directly using an unresolved local.
  */
  def declareJavaBipStatePredicates(ps: Seq[(Expr[G], JavaAnnotation[G])]) =
    copy(javaBipStatePredicates = javaBipStatePredicates ++ ps.toMap)

  /* Guard names are explicitly saved as expr's instead of String. See comment at declareJavaBipStatePredicates. */
  def declareJavaBipGuards(gs: Seq[(Expr[G], JavaMethod[G])]): ReferenceResolutionContext[G] =
    copy(javaBipGuards = javaBipGuards ++ gs.toMap)

  def enableJavaBipGuards(): ReferenceResolutionContext[G] = copy(javaBipGuardsEnabled = true)

  def currentPkg: Option[JavaName[G]] = currentJavaNamespace.flatMap(_.pkg)
  def currentFqn: Option[JavaName[G]] = currentPkg.map(pkg => JavaName(pkg.names ++ currentJavaClass.map(cls => Seq(cls.name)).getOrElse(Seq()))(DiagnosticOrigin))

  def appendTypeEnv(typeEnv: Map[Variable[G], Type[G]]): ReferenceResolutionContext[G] =
    copy(typeEnv = this.typeEnv ++ typeEnv)
}
