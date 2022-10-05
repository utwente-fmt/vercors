package vct.col.ast.lang

import vct.col.ast.{ClassDeclaration, JavaEnum, JavaEnumConstant, Type, Variable}
import vct.col.resolve.ctx.{RefJavaEnumConstant, Referrable}
import vct.col.resolve.lang.Java

trait JavaEnumImpl[G] { this: JavaEnum[G] =>
  override def typeParams: Seq[Variable[G]] = Nil
  override def decls: Seq[ClassDeclaration[G]] = constants ++ classDecls
  override def supports: Seq[Type[G]] = Seq(ext)

  def getConstant(name: String): Option[RefJavaEnumConstant[G]] = constants.collectFirst({
    case c: JavaEnumConstant[G] if c.name == name => RefJavaEnumConstant(c)
  })
}
