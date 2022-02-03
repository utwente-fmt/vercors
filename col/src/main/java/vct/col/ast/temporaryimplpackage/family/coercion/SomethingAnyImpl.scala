package vct.col.ast.temporaryimplpackage.family.coercion

import vct.col.ast.{SomethingAny, TAny, Type}

trait SomethingAnyImpl[G] { this: SomethingAny[G] => 
  override def target: Type[G] = TAny()
}
