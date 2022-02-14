package vct.col.newrewrite.util

import vct.col.ast.{AbstractRewriter, AccountedPredicate, ApplicableContract, CDeclarationSpecifier, CDeclarator, CInit, CPointer, CTypeQualifier, CatchClause, Coercion, Declaration, Expr, FieldFlag, IterVariable, JavaImport, JavaModifier, JavaName, LoopContract, ParRegion, Program, SignalsClause, SilverPredicateAccess, Statement, Type}
import vct.col.ref.{LazyRef, Ref}
import vct.col.rewrite.NonLatchingRewriter

import scala.reflect.ClassTag

case class FreshSuccessionScope[Pre, Post](outer: AbstractRewriter[Pre, Post]) extends AbstractRewriter[Pre, Post] {
  override def lookupSuccessor(decl: Declaration[Pre]): Option[Declaration[Post]] =
    super.lookupSuccessor(decl).orElse(outer.lookupSuccessor(decl))

  override def dispatch(decl: Declaration[Pre]): Unit = outer.dispatch(decl)
  override def dispatch(node: CInit[Pre]): CInit[Post] = outer.dispatch(node)
  override def dispatch(node: Coercion[Pre]): Coercion[Post] = outer.dispatch(node)
  override def dispatch(node: CDeclarationSpecifier[Pre]): CDeclarationSpecifier[Post] = outer.dispatch(node)
  override def dispatch(node: SilverPredicateAccess[Pre]): SilverPredicateAccess[Post] = outer.dispatch(node)
  override def dispatch(node: LoopContract[Pre]): LoopContract[Post] = outer.dispatch(node)
  override def dispatch(node: Statement[Pre]): Statement[Post] = outer.dispatch(node)
  override def dispatch(node: SignalsClause[Pre]): SignalsClause[Post] = outer.dispatch(node)
  override def dispatch(node: CTypeQualifier[Pre]): CTypeQualifier[Post] = outer.dispatch(node)
  override def dispatch(node: CDeclarator[Pre]): CDeclarator[Post] = outer.dispatch(node)
  override def dispatch(node: Expr[Pre]): Expr[Post] = outer.dispatch(node)
  override def dispatch(node: Type[Pre]): Type[Post] = outer.dispatch(node)
  override def dispatch(node: IterVariable[Pre]): IterVariable[Post] = outer.dispatch(node)
  override def dispatch(node: JavaModifier[Pre]): JavaModifier[Post] = outer.dispatch(node)
  override def dispatch(node: CPointer[Pre]): CPointer[Post] = outer.dispatch(node)
  override def dispatch(node: CatchClause[Pre]): CatchClause[Post] = outer.dispatch(node)
  override def dispatch(node: AccountedPredicate[Pre]): AccountedPredicate[Post] = outer.dispatch(node)
  override def dispatch(node: ParRegion[Pre]): ParRegion[Post] = outer.dispatch(node)
  override def dispatch(node: Program[Pre]): Program[Post] = outer.dispatch(node)
  override def dispatch(node: JavaName[Pre]): JavaName[Post] = outer.dispatch(node)
  override def dispatch(node: JavaImport[Pre]): JavaImport[Post] = outer.dispatch(node)
  override def dispatch(node: ApplicableContract[Pre]): ApplicableContract[Post] = outer.dispatch(node)
  override def dispatch(node: FieldFlag[Pre]): FieldFlag[Post] = outer.dispatch(node)
}
