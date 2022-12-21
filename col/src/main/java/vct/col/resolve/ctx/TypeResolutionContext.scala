package vct.col.resolve.ctx

import vct.col.ast.{GlobalDeclaration, JavaNamespace}
import vct.col.resolve.ExternalJavaLoader
import vct.col.resolve.ResolveTypes.JavaClassPathEntry

import scala.collection.mutable

case class TypeResolutionContext[G]
(
  stack: Seq[Seq[Referrable[G]]] = Nil,
  namespace: Option[JavaNamespace[G]] = None,
  externalJavaLoader: Option[ExternalJavaLoader] = None,
  javaClassPath: Seq[JavaClassPathEntry] = Nil,
  externallyLoadedElements: mutable.ArrayBuffer[GlobalDeclaration[G]] = mutable.ArrayBuffer[GlobalDeclaration[G]](),
)
