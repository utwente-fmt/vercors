package vct.col.resolve

import vct.col.ast.JavaNamespace

case class TypeResolutionContext(stack: Seq[Seq[Referrable]] = Nil,
                                 namespace: Option[JavaNamespace] = None) {
  def replace(stack: Seq[Seq[Referrable]] = stack,
              namespace: Option[JavaNamespace] = namespace): TypeResolutionContext = {
    TypeResolutionContext(stack, namespace)
  }
}
