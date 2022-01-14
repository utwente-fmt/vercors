package vct.col.resolve

import vct.col.ast.{Declaration, GlobalDeclaration, JavaClassOrInterface, JavaNamespace}
import vct.col.check.CheckContext

import scala.collection.mutable

case class ReferenceResolutionContext[G]
                                     (stack: Seq[Seq[Referrable[G]]] = Nil,
                                      externallyLoadedElements: mutable.ArrayBuffer[GlobalDeclaration[G]] = mutable.ArrayBuffer[GlobalDeclaration[G]](),
                                      checkContext: CheckContext[G] = CheckContext[G](),
                                      currentJavaNamespace: Option[JavaNamespace[G]] = None,
                                      currentJavaClass: Option[JavaClassOrInterface[G]] = None,
                                      currentThis: Option[ThisTarget[G]] = None,
                                      currentResult: Option[ResultTarget[G]] = None,
                                     ) {
  def replace(stack: Seq[Seq[Referrable[G]]] = stack,
              externallyLoadedElements: mutable.ArrayBuffer[GlobalDeclaration[G]] = externallyLoadedElements,
              checkContext: CheckContext[G] = checkContext,
              currentJavaNamespace: Option[JavaNamespace[G]] = currentJavaNamespace,
              currentJavaClass: Option[JavaClassOrInterface[G]] = currentJavaClass,
              currentThis: Option[ThisTarget[G]] = currentThis,
              currentResult: Option[ResultTarget[G]] = currentResult,
             ): ReferenceResolutionContext[G] =
    ReferenceResolutionContext(stack, externallyLoadedElements, checkContext, currentJavaNamespace, currentJavaClass, currentThis, currentResult)

  def asTypeResolutionContext: TypeResolutionContext[G] =
    TypeResolutionContext(stack, currentJavaNamespace, None, externallyLoadedElements)

  def declare(decls: Seq[Declaration[G]]): ReferenceResolutionContext[G] =
    replace(stack=decls.flatMap(Referrable.from) +: stack)
}
