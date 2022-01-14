package vct.col.resolve

import hre.util.ScopedStack
import vct.col.ast.{GlobalDeclaration, JavaClass, JavaNamespace}

import scala.collection.mutable

case class TypeResolutionContext[G]
                                (stack: Seq[Seq[Referrable[G]]] = Nil,
                                 namespace: Option[JavaNamespace[G]] = None,
                                 externalJavaLoader: Option[ExternalJavaLoader] = None,
                                 externallyLoadedElements: mutable.ArrayBuffer[GlobalDeclaration[G]] = mutable.ArrayBuffer[GlobalDeclaration[G]](),
                                ) {
  def replace(stack: Seq[Seq[Referrable[G]]] = stack,
              namespace: Option[JavaNamespace[G]] = namespace): TypeResolutionContext[G] = {
    TypeResolutionContext(stack, namespace, externalJavaLoader, externallyLoadedElements)
  }
}
