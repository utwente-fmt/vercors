package vct.col.resolve

import vct.col.ast.{CheckContext, Declaration, JavaClassOrInterface, JavaNamespace, Type}

case class ReferenceResolutionContext(stack: Seq[Seq[Referrable]] = Nil,
                                      checkContext: CheckContext = CheckContext(),
                                      currentJavaNamespace: Option[JavaNamespace] = None,
                                      currentJavaClass: Option[JavaClassOrInterface] = None,
                                      currentReturnType: Option[Type] = None,
                                     ) {
  def replace(stack: Seq[Seq[Referrable]] = stack,
              checkContext: CheckContext = checkContext,
              currentJavaNamespace: Option[JavaNamespace] = currentJavaNamespace,
              currentJavaClass: Option[JavaClassOrInterface] = currentJavaClass,
              currentReturnType: Option[Type] = currentReturnType,
             ): ReferenceResolutionContext =
    ReferenceResolutionContext(stack, checkContext, currentJavaNamespace, currentJavaClass, currentReturnType)

  def declare(decls: Seq[Declaration]): ReferenceResolutionContext =
    replace(stack=decls.flatMap(Referrable.from) +: stack)
}
