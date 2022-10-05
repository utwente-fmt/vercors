package vct.col.ast.lang

import vct.col.ast.{ClassDeclaration, JavaEnum, Type, Variable}
import vct.col.resolve.lang.Java

trait JavaEnumImpl[G] { this: JavaEnum[G] =>
  override def typeParams: Seq[Variable[G]] = Nil
  override def decls: Seq[ClassDeclaration[G]] = constants ++ classDecls
  override def supports: Seq[Type[G]] = Seq(ext)
}
