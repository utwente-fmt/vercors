package vct.col.rewrite

import vct.col.ast._

class Rewriter[Pre <: Generation] extends NonLatchingRewriter[Pre, Rewritten[Pre]] {
  type Post = Rewritten[Pre]
}
