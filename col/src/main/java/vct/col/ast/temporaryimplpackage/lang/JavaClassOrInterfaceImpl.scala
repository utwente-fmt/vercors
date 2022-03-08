package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{ClassDeclaration, Declaration, JavaAnnotationMethod, JavaClassOrInterface, JavaMethod, JavaModifier, JavaTClass, Type, Variable}
import vct.col.ref.Ref
import vct.result.VerificationResult.Unreachable

import scala.reflect.ClassTag

trait JavaClassOrInterfaceImpl[G] { this: JavaClassOrInterface[G] =>
  def name: String
  def modifiers: Seq[JavaModifier[G]]
  def typeParams: Seq[Variable[G]]
  def decls: Seq[ClassDeclaration[G]]
  def supports: Seq[Type[G]]

  def transSupportArrows(seen: Set[JavaClassOrInterface[G]]): Seq[(JavaClassOrInterface[G], JavaClassOrInterface[G])] = {
    if(seen.contains(this)) {
      throw Unreachable("Yes, you got me, cyclical inheritance is not supported!")
    }

    val ts = supports.flatMap {
      case JavaTClass(Ref(cls), _) => Seq(cls)
      case _ => Nil
    }

    ts.map((this, _)) ++ ts.flatMap(_.transSupportArrows(Set(this) ++ seen))
  }

  override def declarations: Seq[Declaration[G]] = typeParams ++ decls

  def findMethodByName[Decl <: ClassDeclaration[G]](name: String)(implicit tag: ClassTag[Decl]): Seq[Decl] = {
    def matches(decl: ClassDeclaration[G]): Boolean = decl match {
      case decl: JavaMethod[G] if decl.name == name => true
      case decl: JavaAnnotationMethod[G] if decl.name == name => true
      case _ => false
    }

    decls.collect {
      case decl: Decl if matches(decl) => decl
    }
  }
}