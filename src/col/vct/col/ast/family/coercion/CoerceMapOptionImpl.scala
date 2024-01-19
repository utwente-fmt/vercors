package vct.col.ast.family.coercion

import vct.col.ast.{CoerceMapOption, TOption}
import vct.col.ast.ops.CoerceMapOptionOps

trait CoerceMapOptionImpl[G] extends CoerceMapOptionOps[G] { this: CoerceMapOption[G] => 
  override def target: TOption[G] = TOption(targetOptionElement)
}
