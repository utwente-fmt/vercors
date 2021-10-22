package vct.col.resolve

import hre.util.ScopedStack
import vct.col.ast.{GlobalDeclaration, JavaClass, JavaNamespace}

import scala.collection.mutable

case class TypeResolutionContext(stack: Seq[Seq[Referrable]] = Nil,
                                 namespace: Option[JavaNamespace] = None,
                                 externallyLoadedElements: mutable.ArrayBuffer[GlobalDeclaration] = mutable.ArrayBuffer(),
                                ) {
  def replace(stack: Seq[Seq[Referrable]] = stack,
              namespace: Option[JavaNamespace] = namespace): TypeResolutionContext = {
    TypeResolutionContext(stack, namespace, externallyLoadedElements)
  }
}
