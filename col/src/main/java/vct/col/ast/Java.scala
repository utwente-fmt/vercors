package vct.col.ast

import hre.util.FuncTools

case class JavaName(names: Seq[String])(implicit val o: Origin)
  extends NodeFamily with NoCheck {
  var ref: Option[JavaClass] = None
}
case class JavaImport(isStatic: Boolean, name: JavaName, star: Boolean)(implicit val o: Origin)
  extends NodeFamily with NoCheck

sealed trait JavaModifier extends NodeFamily with NoCheck
case class JavaPublic()(implicit val o: Origin) extends JavaModifier
case class JavaProtected()(implicit val o: Origin) extends JavaModifier
case class JavaPrivate()(implicit val o: Origin) extends JavaModifier
case class JavaStatic()(implicit val o: Origin) extends JavaModifier
case class JavaAbstract()(implicit val o: Origin) extends JavaModifier
case class JavaFinal()(implicit val o: Origin) extends JavaModifier
case class JavaStrictFP()(implicit val o: Origin) extends JavaModifier
case class JavaNative()(implicit val o: Origin) extends JavaModifier
case class JavaSynchronized()(implicit val o: Origin) extends JavaModifier
case class JavaTransient()(implicit val o: Origin) extends JavaModifier
case class JavaVolatile()(implicit val o: Origin) extends JavaModifier

case class JavaPure()(implicit val o: Origin) extends JavaModifier
case class JavaInline()(implicit val o: Origin) extends JavaModifier

sealed trait JavaGlobalDeclaration extends ExtraGlobalDeclaration
case class JavaNamespace(pkg: Option[JavaName], imports: Seq[JavaImport], declarations: Seq[GlobalDeclaration])(implicit val o: Origin)
  extends JavaGlobalDeclaration with NoCheck with Declarator

sealed trait JavaClassOrInterface
case class JavaClass(name: String, modifiers: Seq[JavaModifier], typeParams: Seq[Variable],
                     ext: Type, imp: Seq[Type],
                     decls: Seq[ClassDeclaration])
                    (implicit val o: Origin)
  extends JavaGlobalDeclaration with NoCheck with JavaClassOrInterface with Declarator {
  def superTypeOf(other: JavaClass): Boolean =
    other == this ||
      superTypeOf(other.ext) ||
      other.imp.exists(superTypeOf)

  def superTypeOf(other: Type): Boolean = other match {
    case otherClassType @ JavaTClass(_) => superTypeOf(otherClassType.ref.get)
    case _ => false
  }

  override def declarations: Seq[Declaration] = typeParams ++ decls
}
case class JavaInterface(name: String, modifiers: Seq[JavaModifier], typeParams: Seq[Variable],
                         ext: Seq[Type], decls: Seq[ClassDeclaration])
                        (implicit val o: Origin)
  extends JavaGlobalDeclaration with NoCheck with JavaClassOrInterface with Declarator {
  override def declarations: Seq[Declaration] = typeParams ++ decls
}

sealed trait JavaClassDeclaration extends ExtraClassDeclaration
case class JavaSharedInitialization(isStatic: Boolean, initialization: Statement)(implicit val o: Origin)
  extends JavaClassDeclaration with NoCheck
case class JavaFields(modifiers: Seq[JavaModifier], t: Type, decls: Seq[(String, Int, Option[Expr])])
                     (implicit val o: Origin)
  extends JavaClassDeclaration with NoCheck
case class JavaConstructor(modifiers: Seq[JavaModifier], name: String,
                           parameters: Seq[Variable], typeParameters: Seq[Variable],
                           signals: Seq[JavaName], body: Statement, contract: ApplicableContract)
                          (implicit val o: Origin)
  extends JavaClassDeclaration with NoCheck with Declarator {
  override def declarations: Seq[Declaration] = parameters ++ typeParameters
}
case class JavaMethod(modifiers: Seq[JavaModifier], returnType: Type, dims: Int, name: String,
                      parameters: Seq[Variable], typeParameters: Seq[Variable],
                      signals: Seq[JavaName], body: Option[Statement], contract: ApplicableContract)
                     (val blame: PostconditionBlame)(implicit val o: Origin)
  extends JavaClassDeclaration with NoCheck with Declarator {
  override def declarations: Seq[Declaration] = parameters ++ typeParameters
}

case class JavaSpecDeclaration(isStatic: Boolean, declaration: ClassDeclaration)(implicit val o: Origin)
  extends JavaClassDeclaration with NoCheck

sealed trait JavaStatement extends ExtraStatement
case class JavaLocalDeclaration(modifiers: Seq[JavaModifier], t: Type, decls: Seq[(String, Int, Option[Expr])])
                               (implicit val o: Origin)
  extends JavaStatement with NoCheck

sealed trait JavaType extends ExtraType
case class JavaTUnion(names: Seq[JavaName])(implicit val o: Origin) extends JavaType {
  override def superTypeOfImpl(other: Type): Boolean =
    names.exists(_.ref.get.superTypeOf(other))
}

case class JavaTClass(names: Seq[(String, Option[Seq[Type]])])(implicit val o: Origin) extends JavaType {
  var ref: Option[JavaClass] = None

  override def superTypeOfImpl(other: Type): Boolean =
    ref.get.superTypeOf(other)
}

case class JavaTClassValue(ref: Ref[JavaClass])(implicit val o: Origin) extends JavaType {
  override protected def superTypeOfImpl(other: Type): Boolean = false
}

case class JavaTNamespace(names: Seq[String])(implicit val o: Origin) extends JavaType {
  override protected def superTypeOfImpl(other: Type): Boolean = false
}

sealed trait JavaExpr extends ExtraExpr
case class JavaLocal(name: String)(implicit val o: Origin) extends JavaExpr with NoCheck {
  var ref: Option[JavaRef] = None
  override def t: Type = ref.get.t
}

case class JavaDeref(obj: Expr, field: String)(implicit val o: Origin) extends JavaExpr with NoCheck {
  var ref: Option[JavaDerefRef] = None
  override def t: Type = ref.get match {
    case JavaRefField(fields, declIndex) =>
      FuncTools.repeat(TArray(_), fields.decls(declIndex)._2, fields.t)
    case JavaDerefRefNamespace(names) =>
      JavaTNamespace(names)
    case JavaDerefRefClass(cls) =>
      JavaTClassValue(new DirectRef(cls))
  }
}

case class JavaLiteralArray(exprs: Seq[Expr])(implicit val o: Origin) extends JavaExpr with NoCheck {
  var typeContext: Option[Type] = None
  override def t: Type = typeContext.get
}

case class JavaInvocation(obj: Option[Expr], typeParams: Seq[Type], method: String, arguments: Seq[Expr])
                         (val blame: PreconditionBlame)(implicit val o: Origin) extends JavaExpr with NoCheck {
  var ref: Option[JavaMethod] = None
  override def t: Type = ref.get.returnType
}

case class JavaNewClass(args: Seq[Expr], typeArgs: Seq[Type], name: Type)
                       (val blame: PreconditionBlame)(implicit val o: Origin) extends JavaExpr with NoCheck {
  override def t: Type = name
}
case class JavaNewLiteralArray(baseType: Type, dims: Int, initializer: Expr)(implicit val o: Origin)
  extends JavaExpr with NoCheck {
  override def t: Type = FuncTools.repeat(TArray(_), dims, baseType)
}

case class JavaNewDefaultArray(baseType: Type, specifiedDims: Seq[Expr], moreDims: Int)(implicit val o: Origin)
  extends JavaExpr with NoCheck {
  override def t: Type = (0 until (specifiedDims.size + moreDims)).foldLeft(baseType)((t, _) => TArray(t))
}
