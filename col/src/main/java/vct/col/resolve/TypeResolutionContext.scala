package vct.col.resolve

import vct.col.ast.JavaNamespace

import scala.collection.mutable

case class TypeResolutionContext(stack: Seq[Seq[Referrable]] = Nil,
                                 namespace: Option[JavaNamespace] = None,
                                 externallyLoadedClasses: mutable.ArrayBuffer[JavaNamespace] = mutable.ArrayBuffer()) {
  def replace(stack: Seq[Seq[Referrable]] = stack,
              namespace: Option[JavaNamespace] = namespace): TypeResolutionContext = {
    TypeResolutionContext(stack, namespace, externallyLoadedClasses)
  }
}
