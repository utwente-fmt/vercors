package vct.col.resolve.ctx

import vct.col.ast._
import vct.col.check.CheckContext
import vct.col.origin.DiagnosticOrigin

import scala.collection.mutable

case class ReferenceResolutionContext[G]
(
  program: Program[G],
  stack: Seq[Seq[Referrable[G]]] = Nil,
  externallyLoadedElements: mutable.ArrayBuffer[GlobalDeclaration[G]] = mutable.ArrayBuffer[GlobalDeclaration[G]](),
  checkContext: CheckContext[G] = CheckContext[G](),
  currentJavaNamespace: Option[JavaNamespace[G]] = None,
  currentJavaClass: Option[JavaClassOrInterface[G]] = None,
  currentThis: Option[ThisTarget[G]] = None,
  currentResult: Option[ResultTarget[G]] = None,
  currentInitializerType: Option[Type[G]] = None,
) {
  def asTypeResolutionContext: TypeResolutionContext[G] =
    TypeResolutionContext(stack, currentJavaNamespace, None, Nil, externallyLoadedElements)

  def declare(decls: Seq[Declaration[G]]): ReferenceResolutionContext[G] =
    copy(stack = decls.flatMap(Referrable.from) +: stack)

  def currentPkg: Option[JavaName[G]] = currentJavaNamespace.flatMap(_.pkg)
  def currentFqn: Option[JavaName[G]] = currentPkg.map(pkg => JavaName(pkg.names ++ currentJavaClass.map(cls => Seq(cls.name)).getOrElse(Seq()))(DiagnosticOrigin))
}
