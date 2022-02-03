package vct.col.ast.temporaryimplpackage.family.coercion

import vct.col.ast.{CoerceFracZFrac, TZFraction}

trait CoerceFracZFracImpl[G] { this: CoerceFracZFrac[G] => 
  override def target: TZFraction[G] = TZFraction()
}
