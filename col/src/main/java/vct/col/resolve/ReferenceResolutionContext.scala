package vct.col.resolve

import vct.col.ast.{Declaration, GlobalDeclaration, JavaClassOrInterface, JavaNamespace, Type}
import vct.col.check.CheckContext

import scala.collection.mutable

case class ReferenceResolutionContext(stack: Seq[Seq[Referrable]] = Nil,
                                      externallyLoadedElements: mutable.ArrayBuffer[GlobalDeclaration] = mutable.ArrayBuffer(),
                                      checkContext: CheckContext = CheckContext(),
                                      currentJavaNamespace: Option[JavaNamespace] = None,
                                      currentJavaClass: Option[JavaClassOrInterface] = None,
                                      currentThisType: Option[Type] = None,
                                      currentReturnType: Option[Type] = None,
                                     ) {
  def replace(stack: Seq[Seq[Referrable]] = stack,
              externallyLoadedElements: mutable.ArrayBuffer[GlobalDeclaration] = externallyLoadedElements,
              checkContext: CheckContext = checkContext,
              currentJavaNamespace: Option[JavaNamespace] = currentJavaNamespace,
              currentJavaClass: Option[JavaClassOrInterface] = currentJavaClass,
              currentThisType: Option[Type] = currentThisType,
              currentReturnType: Option[Type] = currentReturnType,
             ): ReferenceResolutionContext =
    ReferenceResolutionContext(stack, externallyLoadedElements, checkContext, currentJavaNamespace, currentJavaClass, currentThisType, currentReturnType)

  def asTypeResolutionContext: TypeResolutionContext =
    TypeResolutionContext(stack, currentJavaNamespace, externallyLoadedElements)

  def declare(decls: Seq[Declaration]): ReferenceResolutionContext =
    replace(stack=decls.flatMap(Referrable.from) +: stack)
}
