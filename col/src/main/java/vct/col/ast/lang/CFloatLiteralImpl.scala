package vct.col.ast.lang

import vct.col.ast.{CDouble, CFloat, CFloatLiteral, CLong, CPrimitiveType, Type}

trait CFloatLiteralImpl[G] { this: CFloatLiteral[G] =>
  assert(floatType match {
    case Seq(CFloat()) => true
    case Seq(CDouble()) => true
    case Seq(CLong(), CDouble()) => true
  })
  def t: Type[G] = CPrimitiveType[G](floatType)
}
