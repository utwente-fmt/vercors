package vct.col.resolve

import vct.col.ast.{Declaration, GlobalDeclaration, JavaBipStatePredicate, JavaClassOrInterface, JavaMethod, JavaName, JavaNamespace, Type}
import vct.col.check.CheckContext
import vct.col.origin.DiagnosticOrigin
import vct.col.resolve.Resolve.SpecExprParser
import vct.col.util.SuccessionMap

import scala.collection.immutable.{HashMap, ListMap}
import scala.collection.mutable

case class ReferenceResolutionContext[G]
(
  javaParser: SpecExprParser,
  stack: Seq[Seq[Referrable[G]]] = Nil,
  externallyLoadedElements: mutable.ArrayBuffer[GlobalDeclaration[G]] = mutable.ArrayBuffer[GlobalDeclaration[G]](),
  checkContext: CheckContext[G] = CheckContext[G](),
  currentJavaNamespace: Option[JavaNamespace[G]] = None,
  currentJavaClass: Option[JavaClassOrInterface[G]] = None,
  currentThis: Option[ThisTarget[G]] = None,
  currentResult: Option[ResultTarget[G]] = None,
  currentInitializerType: Option[Type[G]] = None,
  providedJavaBipStatePredicates: Set[String] = Set(),
  javaBipStatePredicates: SuccessionMap[String, JavaBipStatePredicate[G]] = SuccessionMap[String, JavaBipStatePredicate[G]](),
  javaBipGuards: Map[String, JavaMethod[G]] = new HashMap[String, JavaMethod[G]]()
) {
  def asTypeResolutionContext: TypeResolutionContext[G] =
    TypeResolutionContext(stack, currentJavaNamespace, None, externallyLoadedElements)

  def declare(decls: Seq[Declaration[G]]): ReferenceResolutionContext[G] =
    copy(stack = decls.flatMap(Referrable.from) +: stack)

  def provideJavaBipStatePredicates(ns: Set[String]) =
    copy(providedJavaBipStatePredicates = ns ++ providedJavaBipStatePredicates)

  def declareJavaBipGuards(gs: Map[String, JavaMethod[G]]): ReferenceResolutionContext[G] =
    copy(javaBipGuards = gs ++ javaBipGuards)

  def currentPkg: Option[JavaName[G]] = currentJavaNamespace.flatMap(_.pkg)
  def currentFqn: Option[JavaName[G]] = currentPkg.map(pkg => JavaName(pkg.names ++ currentJavaClass.map(cls => Seq(cls.name)).getOrElse(Seq()))(DiagnosticOrigin))
}
