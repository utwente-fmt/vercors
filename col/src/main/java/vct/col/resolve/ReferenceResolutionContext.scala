package vct.col.resolve

import vct.col.ast.{CheckContext, Declaration, JavaClassOrInterface, JavaNamespace, Type}

import scala.collection.mutable

case class ReferenceResolutionContext(stack: Seq[Seq[Referrable]] = Nil,
                                      externallyLoadedClasses: mutable.ArrayBuffer[JavaNamespace] = mutable.ArrayBuffer(),
                                      checkContext: CheckContext = CheckContext(),
                                      currentJavaNamespace: Option[JavaNamespace] = None,
                                      currentJavaClass: Option[JavaClassOrInterface] = None,
                                      currentThisType: Option[Type] = None,
                                      currentReturnType: Option[Type] = None,
                                     ) {
  def replace(stack: Seq[Seq[Referrable]] = stack,
              externallyLoadedClasses: mutable.ArrayBuffer[JavaNamespace] = externallyLoadedClasses,
              checkContext: CheckContext = checkContext,
              currentJavaNamespace: Option[JavaNamespace] = currentJavaNamespace,
              currentJavaClass: Option[JavaClassOrInterface] = currentJavaClass,
              currentThisType: Option[Type] = currentThisType,
              currentReturnType: Option[Type] = currentReturnType,
             ): ReferenceResolutionContext =
    ReferenceResolutionContext(stack, externallyLoadedClasses, checkContext, currentJavaNamespace, currentJavaClass, currentThisType, currentReturnType)

  def asTypeResolutionContext: TypeResolutionContext =
    TypeResolutionContext(stack, currentJavaNamespace, externallyLoadedClasses)

  def declare(decls: Seq[Declaration]): ReferenceResolutionContext =
    replace(stack=decls.flatMap(Referrable.from) +: stack)
}
