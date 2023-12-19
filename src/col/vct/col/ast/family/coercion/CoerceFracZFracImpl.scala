package vct.col.ast.family.coercion

import vct.col.ast.{CoerceFracZFrac, TZFraction}

trait CoerceFracZFracImpl[G] { this: CoerceFracZFrac[G] => 
  override def target: TZFraction[G] = TZFraction()
}
