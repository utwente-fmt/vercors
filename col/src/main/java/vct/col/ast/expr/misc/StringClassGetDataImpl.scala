package vct.col.ast.expr.misc

import vct.col.ast.{StringClassData, TString, Type}

trait StringClassGetDataImpl[G] { this: StringClassData[G] =>
  def t: Type[G] = TString();
}
