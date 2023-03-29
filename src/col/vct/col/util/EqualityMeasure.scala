package vct.col.util

import vct.col.ast.Declaration

case class EqualityMeasure[Pre, Post, PreDecl <: Declaration[Pre], PostDecl <: Declaration[Post]](scopes: FrozenScopes[Pre, Post, PreDecl, PostDecl], decl: PreDecl) {
  override def equals(obj: Any): Boolean = obj match {
    case other: EqualityMeasure[Pre, Post, PreDecl, PostDecl] =>
      decl == other.decl &&
        // Assumption: the pre-declaration must occur in one of the maps for both frozenscopes, we don't care about
        // the equality value if they don't end up there (since that's a bug).
        // We can ignore identical maps in the tails, since whether the declaration is in there does not matter
        // Non-identical maps in the init must not contain the pre-declaration, since the successor will not be visible
        // to the other frozen scope stack, and hence the post-declaration cannot be equal.
        scopes.scopes.reverse.zip(other.scopes.scopes.reverse).dropWhile { case (l, r) => l == r }.forall {
          case (l, r) => !l.contains(decl) && !r.contains(decl)
        }
    case _ => false
  }

  override def hashCode(): Int = decl.hashCode()
}