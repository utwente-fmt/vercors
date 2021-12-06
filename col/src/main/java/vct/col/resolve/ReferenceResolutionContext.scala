package vct.col.resolve

import vct.col.ast.{Declaration, GlobalDeclaration, JavaClassOrInterface, JavaNamespace}
import vct.col.check.CheckContext

import scala.collection.mutable

case class ReferenceResolutionContext(stack: Seq[Seq[Referrable]] = Nil,
                                      externallyLoadedElements: mutable.ArrayBuffer[GlobalDeclaration] = mutable.ArrayBuffer(),
                                      checkContext: CheckContext = CheckContext(),
                                      currentJavaNamespace: Option[JavaNamespace] = None,
                                      currentJavaClass: Option[JavaClassOrInterface] = None,
                                      currentThis: Option[ThisTarget] = None,
                                      currentResult: Option[ResultTarget] = None,
                                     ) {
  def replace(stack: Seq[Seq[Referrable]] = stack,
              externallyLoadedElements: mutable.ArrayBuffer[GlobalDeclaration] = externallyLoadedElements,
              checkContext: CheckContext = checkContext,
              currentJavaNamespace: Option[JavaNamespace] = currentJavaNamespace,
              currentJavaClass: Option[JavaClassOrInterface] = currentJavaClass,
              currentThis: Option[ThisTarget] = currentThis,
              currentResult: Option[ResultTarget] = currentResult,
             ): ReferenceResolutionContext =
    ReferenceResolutionContext(stack, externallyLoadedElements, checkContext, currentJavaNamespace, currentJavaClass, currentThis, currentResult)

  def asTypeResolutionContext: TypeResolutionContext =
    TypeResolutionContext(stack, currentJavaNamespace, externallyLoadedElements)

  def declare(decls: Seq[Declaration]): ReferenceResolutionContext =
    replace(stack=decls.flatMap(Referrable.from) +: stack)
}
