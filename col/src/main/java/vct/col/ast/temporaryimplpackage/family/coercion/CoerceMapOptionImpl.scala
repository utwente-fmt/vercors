package vct.col.ast.temporaryimplpackage.family.coercion

import vct.col.ast.{CoerceMapOption, TOption}

trait CoerceMapOptionImpl[G] { this: CoerceMapOption[G] => 
  override def target: TOption[G] = TOption(targetOptionElement)
}
