package vct.col.ast

import hre.util.FuncTools
import vct.col.resolve.{BuiltinField, BuiltinInstanceMethod, JavaDerefTarget, JavaInvocationTarget, JavaNameTarget, JavaTypeNameTarget, RefADTFunction, RefAxiomaticDataType, RefClass, RefFunction, RefInstanceFunction, RefInstanceMethod, RefInstancePredicate, RefJavaClass, RefJavaField, RefJavaLocalDeclaration, RefJavaMethod, RefModel, RefModelAction, RefModelField, RefModelProcess, RefPredicate, RefProcedure, RefUnloadedJavaNamespace, RefVariable, SpecDerefTarget, SpecInvocationTarget, SpecNameTarget, SpecTypeNameTarget}

case class JavaName(names: Seq[String])(implicit val o: Origin)
  extends NodeFamily with NoCheck {
  var ref: Option[JavaTypeNameTarget] = None
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
class JavaNamespace(val pkg: Option[JavaName], val imports: Seq[JavaImport], val declarations: Seq[GlobalDeclaration])(implicit val o: Origin)
  extends JavaGlobalDeclaration with NoCheck with Declarator

sealed abstract class JavaClassOrInterface extends JavaGlobalDeclaration with Declarator {
  def name: String
  def modifiers: Seq[JavaModifier]
  def typeParams: Seq[Variable]
  def decls: Seq[ClassDeclaration]
  def supports: Seq[Type]
  def superTypeOf(other: JavaClassOrInterface): Boolean =
    other == this || other.supports.exists(superTypeOf)

  def superTypeOf(other: Type): Boolean = other match {
    case otherClassType @ JavaTClass(_) => superTypeOf(otherClassType.ref.get match {
      case RefJavaClass(decl) => decl
      case _ => return false
    })
    case _ => false
  }

  override def declarations: Seq[Declaration] = typeParams ++ decls
}

class JavaClass(val name: String, val modifiers: Seq[JavaModifier], val typeParams: Seq[Variable],
                val ext: Type, val imp: Seq[Type],
                val decls: Seq[ClassDeclaration])
               (implicit val o: Origin)
  extends JavaClassOrInterface with NoCheck {
  override def supports: Seq[Type] = ext +: imp
}
class JavaInterface(val name: String, val modifiers: Seq[JavaModifier], val typeParams: Seq[Variable],
                    val ext: Seq[Type], val decls: Seq[ClassDeclaration])
                   (implicit val o: Origin)
  extends JavaClassOrInterface with NoCheck {
  override def supports: Seq[Type] = ext
}

sealed trait JavaClassDeclaration extends ExtraClassDeclaration
class JavaSharedInitialization(val isStatic: Boolean, val initialization: Statement)(implicit val o: Origin)
  extends JavaClassDeclaration with NoCheck
class JavaFields(val modifiers: Seq[JavaModifier], val t: Type, val decls: Seq[(String, Int, Option[Expr])])
                (implicit val o: Origin)
  extends JavaClassDeclaration with NoCheck
class JavaConstructor(val modifiers: Seq[JavaModifier], val name: String,
                      val parameters: Seq[Variable], val typeParameters: Seq[Variable],
                      val signals: Seq[JavaName], val body: Statement, val contract: ApplicableContract)
                     (implicit val o: Origin)
  extends JavaClassDeclaration with NoCheck with Declarator {
  override def declarations: Seq[Declaration] = parameters ++ typeParameters ++ contract.givenArgs ++ contract.yieldsArgs
}
class JavaMethod(val modifiers: Seq[JavaModifier], val returnType: Type, val dims: Int, val name: String,
                 val parameters: Seq[Variable], val typeParameters: Seq[Variable],
                 val signals: Seq[JavaName], val body: Option[Statement], val contract: ApplicableContract)
                (val blame: Blame[PostconditionFailed])(implicit val o: Origin)
  extends JavaClassDeclaration with NoCheck with Declarator {
  override def declarations: Seq[Declaration] = parameters ++ typeParameters ++ contract.givenArgs ++ contract.yieldsArgs
}

class JavaLocalDeclaration(val modifiers: Seq[JavaModifier], val t: Type, val decls: Seq[(String, Int, Option[Expr])])
                          (implicit val o: Origin)
  extends ExtraDeclarationKind with NoCheck {
  override def declareDefault(scope: ScopeContext): Unit = scope.javaLocalScopes.top += this
}

sealed trait JavaStatement extends ExtraStatement
case class JavaLocalDeclarationStatement(decl: JavaLocalDeclaration)
                                        (implicit val o: Origin)
  extends JavaStatement with NoCheck

sealed trait JavaType extends ExtraType
case class JavaTUnion(types: Seq[Type])(implicit val o: Origin = DiagnosticOrigin) extends JavaType {
  override def mimics: Type =
    if(types.size == 1) types.head.mimics
    else JavaTUnion(types.map(_.mimics))

  override def subTypeOfImpl(other: Type): Boolean =
    types.forall(other.superTypeOf)

  override def superTypeOfImpl(other: Type): Boolean =
    types.exists(_.superTypeOf(other))
}

case class JavaTClass(names: Seq[(String, Option[Seq[Type]])])(implicit val o: Origin) extends JavaType {
  var ref: Option[JavaTypeNameTarget] = None

  override def mimics: Type = ref.get match {
    case RefAxiomaticDataType(decl) => TAxiomatic(decl.ref, Nil)
    case RefModel(decl) => TModel(decl.ref)
    case RefJavaClass(_) => this
  }

  override def superTypeOfImpl(other: Type): Boolean =
    ref.get.asInstanceOf[RefJavaClass].decl.superTypeOf(other)
}

sealed trait JavaExpr extends ExtraExpr
case class JavaLocal(name: String)(implicit val o: Origin) extends JavaExpr with NoCheck {
  var ref: Option[JavaNameTarget] = None
  override def t: Type = ref.get match {
    case ref: RefAxiomaticDataType => TNotAValue(ref)
    case RefVariable(decl) => decl.t
    case ref: RefUnloadedJavaNamespace => TNotAValue(ref)
    case ref: RefJavaClass => TNotAValue(ref)
    case RefJavaField(decls, idx) => FuncTools.repeat(TArray(_), decls.decls(idx)._2, decls.t)
    case RefJavaLocalDeclaration(decls, idx) => FuncTools.repeat(TArray(_), decls.decls(idx)._2, decls.t)
    case RefModelField(field) => field.t
  }
}

case class JavaDeref(obj: Expr, field: String)(implicit val o: Origin) extends JavaExpr with NoCheck {
  var ref: Option[JavaDerefTarget] = None
  override def t: Type = ref.get match {
    case RefModelField(decl) => decl.t
    case ref: RefUnloadedJavaNamespace => TNotAValue(ref)
    case ref: RefJavaClass => TNotAValue(ref)
    case ref: RefAxiomaticDataType => TNotAValue(ref)
    case RefJavaField(decls, idx) => FuncTools.repeat(TArray(_), decls.decls(idx)._2, decls.t)
    case BuiltinField(f) => f(obj).t
  }
}

case class JavaLiteralArray(exprs: Seq[Expr])(implicit val o: Origin) extends JavaExpr with NoCheck {
  var typeContext: Option[Type] = None
  override def t: Type = typeContext.get
}

case class JavaInvocation(obj: Option[Expr], typeParams: Seq[Type], method: String, arguments: Seq[Expr], givenArgs: Seq[(String, Expr)], yields: Seq[(Expr, String)])
                         (val blame: Blame[PreconditionFailed])(implicit val o: Origin) extends JavaExpr with NoCheck {
  var ref: Option[JavaInvocationTarget] = None
  override def t: Type = ref.get match {
    case RefFunction(decl) => decl.returnType
    case RefProcedure(decl) => decl.returnType
    case RefPredicate(decl) => decl.returnType
    case RefInstanceFunction(decl) => decl.returnType
    case RefInstanceMethod(decl) => decl.returnType
    case RefInstancePredicate(decl) => decl.returnType
    case RefADTFunction(decl) => decl.returnType
    case RefModelProcess(decl) => decl.returnType
    case RefModelAction(decl) => decl.returnType
    case RefJavaMethod(decl) => decl.returnType
    case BuiltinInstanceMethod(f) => f(obj.get)(arguments).t
  }
}

case class JavaNewClass(args: Seq[Expr], typeArgs: Seq[Type], name: Type)
                       (val blame: Blame[PreconditionFailed])(implicit val o: Origin) extends JavaExpr with NoCheck {
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
