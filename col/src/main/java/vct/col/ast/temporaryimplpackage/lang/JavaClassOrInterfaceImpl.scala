package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{ClassDeclaration, Declaration, JavaClassOrInterface, JavaModifier, JavaTClass, Type, Variable}
import vct.col.ref.Ref
import vct.result.VerificationResult.Unreachable

trait JavaClassOrInterfaceImpl { this: JavaClassOrInterface =>
  def name: String
  def modifiers: Seq[JavaModifier]
  def typeParams: Seq[Variable]
  def decls: Seq[ClassDeclaration]
  def supports: Seq[Type]

  def transSupportArrows(seen: Set[JavaClassOrInterface]): Seq[(JavaClassOrInterface, JavaClassOrInterface)] = {
    if(seen.contains(this)) {
      throw Unreachable("Yes, you got me, cyclical inheritance is not supported!")
    }

    val ts = supports.flatMap {
      case JavaTClass(Ref(cls), _) => Seq(cls)
      case _ => Nil
    }

    ts.map((this, _)) ++ ts.flatMap(_.transSupportArrows(Set(this) ++ seen))
  }

  override def declarations: Seq[Declaration] = typeParams ++ decls
}