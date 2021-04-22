package vct.col.ast

case class JavaName(names: Seq[String])(implicit val o: Origin)
  extends NodeFamily with NoCheck
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

sealed trait JavaGlobalDeclaration extends ExtraGlobalDeclaration
case class JavaNamespace(pkg: Option[JavaName], imports: Seq[JavaImport], decls: Seq[GlobalDeclaration])(implicit val o: Origin)
  extends JavaGlobalDeclaration with NoCheck

case class JavaClass(name: String, modifiers: Seq[JavaModifier], typeParams: Seq[Variable],
                     ext: Type, imp: Seq[Type],
                     decls: Seq[ClassDeclaration])
                    (implicit val o: Origin)
  extends JavaGlobalDeclaration with NoCheck
case class JavaInterface(name: String, modifiers: Seq[JavaModifier], typeParams: Seq[Variable],
                         ext: Seq[Type], decls: Seq[ClassDeclaration])
                        (implicit val o: Origin)
  extends JavaGlobalDeclaration with NoCheck

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
  extends JavaClassDeclaration with NoCheck
case class JavaMethod(modifiers: Seq[JavaModifier], returnType: Type, dims: Int, name: String,
                      parameters: Seq[Variable], typeParameters: Seq[Variable],
                      signals: Seq[JavaName], body: Option[Statement], contract: ApplicableContract)
                     (implicit val o: Origin)
  extends JavaClassDeclaration with NoCheck

sealed trait JavaStatement extends ExtraStatement
case class JavaLocalDeclaration(modifiers: Seq[JavaModifier], t: Type, decls: Seq[(String, Int, Option[Expr])])
                               (implicit val o: Origin)
  extends JavaStatement with NoCheck

sealed trait JavaType extends ExtraType
case class JavaTUnion(names: Seq[JavaName])(implicit val o: Origin) extends JavaType {
  override def superTypeOf(other: Type): Boolean = ???
}
case class JavaTArray(element: Type, dimensions: Int)(implicit val o: Origin) extends JavaType {
  override def superTypeOf(other: Type): Boolean = ???
}
case class JavaTClass(names: Seq[(String, Option[Seq[Type]])])(implicit val o: Origin) extends JavaType {
  override def superTypeOf(other: Type): Boolean = ???
}

sealed trait JavaExpr extends ExtraExpr {
  override def t: Type = TSkip()
}

case class JavaLiteralArray(exprs: Seq[Expr])(implicit val o: Origin) extends JavaExpr with NoCheck
case class JavaInvocation(obj: Option[Expr], typeParams: Seq[Type], method: String, arguments: Seq[Expr])
                         (val blame: PreconditionBlame)(implicit val o: Origin) extends JavaExpr with NoCheck
case class JavaNewClass(args: Seq[Expr], typeArgs: Seq[Type], name: Type)
                       (val blame: PreconditionBlame)(implicit val o: Origin) extends JavaExpr with NoCheck
case class JavaNewLiteralArray(baseType: Type, dims: Int, initializer: Expr)(implicit val o: Origin)
  extends JavaExpr with NoCheck
case class JavaNewDefaultArray(baseType: Type, specifiedDims: Seq[Expr], moreDims: Int)(implicit val o: Origin)
  extends JavaExpr with NoCheck