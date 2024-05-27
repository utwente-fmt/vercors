package vct.parsers.debug

import org.antlr.v4.runtime.atn.{PredictionContext, PredictionContextCache}

import scala.collection.mutable

class ProfilingPredictionContextCache extends PredictionContextCache {
  val hits: mutable.Map[PredictionContext, Int] = mutable.Map()
  val canonicalizations: mutable.Map[PredictionContext, Int] = mutable.Map()

  override def add(ctx: PredictionContext): PredictionContext = {
    if (ctx eq PredictionContext.EMPTY)
      PredictionContext.EMPTY
    else {
      val existing = cache.get(ctx)
      if (existing ne null) {
        canonicalizations(ctx) += 1
        return existing
      } else {
        hits(ctx) = 0
        canonicalizations(ctx) = 0
        cache.put(ctx, ctx)
        ctx
      }
    }
  }

  override def get(ctx: PredictionContext): PredictionContext = {
    val res = super.get(ctx)
    if (res ne null) { hits(ctx) += 1 }
    res
  }
}
