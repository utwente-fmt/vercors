package vct.col.ast.expr.misc

import vct.col.ast.{StringClassGetData, TString, Type}

trait StringClassGetDataImpl[G] { this: StringClassGetData[G] =>
  def t: Type[G] = TString();
}
