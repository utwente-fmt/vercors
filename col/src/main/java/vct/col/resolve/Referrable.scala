package vct.col.resolve

import vct.col.ast._
import vct.col.origin.SourceNameOrigin

case object Referrable {
  def from(decl: Declaration): Seq[Referrable] = Seq(decl match {
    case decl: CParam => RefCParam(decl)
    case decl: CFunctionDefinition => RefCFunctionDefinition(decl)
    case decl: CGlobalDeclaration => return decl.decl.inits.indices.map(RefCGlobalDeclaration(decl, _))
    case decl: JavaNamespace => RefJavaNamespace(decl)
    case decl: JavaClass => RefJavaClass(decl)
    case decl: JavaInterface => RefJavaClass(decl)
    case decl: SilverField => RefSilverField(decl)
    case decl: SimplificationRule => RefSimplificationRule(decl)
    case decl: AxiomaticDataType => RefAxiomaticDataType(decl)
    case decl: Function => RefFunction(decl)
    case decl: Procedure => RefProcedure(decl)
    case decl: Predicate => RefPredicate(decl)
    case decl: Class => RefClass(decl)
    case decl: Model => RefModel(decl)
    case decl: JavaSharedInitialization => RefJavaSharedInitialization(decl)
    case decl: JavaFields => return decl.decls.indices.map(RefJavaField(decl, _))
    case decl: JavaConstructor => RefJavaConstructor(decl)
    case decl: JavaMethod => RefJavaMethod(decl)
    case decl: InstanceFunction => RefInstanceFunction(decl)
    case decl: InstanceMethod => RefInstanceMethod(decl)
    case decl: InstancePredicate => RefInstancePredicate(decl)
    case decl: Field => RefField(decl)
    case decl: Variable => RefVariable(decl)
    case decl: LabelDecl => RefLabelDecl(decl)
    case decl: ParBlockDecl => RefParBlockDecl(decl)
    case decl: ParInvariantDecl => RefParInvariantDecl(decl)
    case decl: ADTAxiom => RefADTAxiom(decl)
    case decl: ADTFunction => RefADTFunction(decl)
    case decl: ModelField => RefModelField(decl)
    case decl: ModelProcess => RefModelProcess(decl)
    case decl: ModelAction => RefModelAction(decl)
    case decl: CDeclaration => return decl.inits.indices.map(RefCDeclaration(decl, _))
    case decl: JavaLocalDeclaration => return decl.decls.indices.map(RefJavaLocalDeclaration(decl, _))
    case decl: PVLConstructor => RefPVLConstructor(decl)
  })

  def originName(decl: Declaration): String = decl.o match {
    case SourceNameOrigin(name, _) => name
    case _ => throw NameLost(decl.o)
  }

  def originNameOrEmpty(decl: Declaration): String = decl.o match {
    case SourceNameOrigin(name, _) => name
    case _ => ""
  }
}

sealed trait Referrable {
  def name: String = this match {
    case RefCParam(decl) => C.nameFromDeclarator(decl.declarator)
    case RefCFunctionDefinition(decl) => C.nameFromDeclarator(decl.declarator)
    case RefCGlobalDeclaration(decls, initIdx) => C.nameFromDeclarator(decls.decl.inits(initIdx).decl)
    case RefCDeclaration(decls, initIdx) => C.nameFromDeclarator(decls.inits(initIdx).decl)
    case RefJavaNamespace(_) => ""
    case RefUnloadedJavaNamespace(_) => ""
    case RefJavaClass(decl) => decl.name
    case RefSilverField(decl) => Referrable.originName(decl)
    case RefSimplificationRule(decl) => Referrable.originName(decl)
    case RefAxiomaticDataType(decl) => Referrable.originName(decl)
    case RefFunction(decl) => Referrable.originName(decl)
    case RefProcedure(decl) => Referrable.originName(decl)
    case RefPredicate(decl) => Referrable.originName(decl)
    case RefClass(decl) => Referrable.originName(decl)
    case RefModel(decl) => Referrable.originName(decl)
    case RefJavaSharedInitialization(decl) => ""
    case RefJavaField(decls, idx) => decls.decls(idx)._1
    case RefJavaLocalDeclaration(decls, idx) => decls.decls(idx)._1
    case RefJavaConstructor(decl) => decl.name
    case RefJavaMethod(decl) => decl.name
    case RefInstanceFunction(decl) => Referrable.originName(decl)
    case RefInstanceMethod(decl) => Referrable.originName(decl)
    case RefInstancePredicate(decl) => Referrable.originName(decl)
    case RefField(decl) => Referrable.originName(decl)
    case RefVariable(decl) => Referrable.originName(decl)
    case RefLabelDecl(decl) => Referrable.originName(decl)
    case RefParBlockDecl(decl) => Referrable.originNameOrEmpty(decl)
    case RefParInvariantDecl(decl) => Referrable.originNameOrEmpty(decl)
    case RefADTAxiom(decl) => Referrable.originName(decl)
    case RefADTFunction(decl) => Referrable.originName(decl)
    case RefModelField(decl) => Referrable.originName(decl)
    case RefModelProcess(decl) => Referrable.originName(decl)
    case RefModelAction(decl) => Referrable.originName(decl)
    case BuiltinField(_) => ""
    case BuiltinInstanceMethod(_) => ""
    case RefPVLConstructor(decl) => ""
  }
}
sealed trait JavaTypeNameTarget extends Referrable with JavaDerefTarget
sealed trait CTypeNameTarget extends Referrable
sealed trait PVLTypeNameTarget extends Referrable
sealed trait SpecTypeNameTarget extends JavaTypeNameTarget with CTypeNameTarget with PVLTypeNameTarget

sealed trait JavaNameTarget extends Referrable
sealed trait CNameTarget extends Referrable
sealed trait PVLNameTarget extends Referrable
sealed trait SpecNameTarget extends CNameTarget with JavaNameTarget with PVLNameTarget

sealed trait CDerefTarget extends Referrable
sealed trait JavaDerefTarget extends Referrable
sealed trait PVLDerefTarget extends Referrable
sealed trait SpecDerefTarget extends CDerefTarget with JavaDerefTarget with PVLDerefTarget

sealed trait JavaInvocationTarget extends Referrable
sealed trait CInvocationTarget extends Referrable
sealed trait PVLInvocationTarget extends Referrable
sealed trait SpecInvocationTarget
  extends JavaInvocationTarget
    with CDerefTarget with CInvocationTarget
    with PVLInvocationTarget

sealed trait ThisTarget extends Referrable

sealed trait ResultTarget extends Referrable

case class RefCParam(decl: CParam) extends Referrable with CNameTarget
case class RefCFunctionDefinition(decl: CFunctionDefinition) extends Referrable with CNameTarget with CInvocationTarget with ResultTarget
case class RefCGlobalDeclaration(decls: CGlobalDeclaration, initIdx: Int) extends Referrable with CNameTarget with CInvocationTarget with ResultTarget
case class RefCDeclaration(decls: CDeclaration, initIdx: Int) extends Referrable with CNameTarget with CInvocationTarget
case class RefJavaNamespace(decl: JavaNamespace) extends Referrable
case class RefUnloadedJavaNamespace(names: Seq[String]) extends Referrable with JavaNameTarget with JavaDerefTarget
case class RefJavaClass(decl: JavaClassOrInterface) extends Referrable with JavaTypeNameTarget with JavaNameTarget with JavaDerefTarget with ThisTarget
case class RefSilverField(decl: SilverField) extends Referrable
case class RefSimplificationRule(decl: SimplificationRule) extends Referrable
case class RefAxiomaticDataType(decl: AxiomaticDataType) extends Referrable with SpecTypeNameTarget with SpecNameTarget with JavaDerefTarget
case class RefFunction(decl: Function) extends Referrable with SpecInvocationTarget with ResultTarget
case class RefProcedure(decl: Procedure) extends Referrable with SpecInvocationTarget with ResultTarget
case class RefPredicate(decl: Predicate) extends Referrable with SpecInvocationTarget
case class RefClass(decl: Class) extends Referrable with PVLTypeNameTarget with PVLNameTarget with ThisTarget
case class RefModel(decl: Model) extends Referrable with SpecTypeNameTarget with ThisTarget
case class RefJavaSharedInitialization(decl: JavaSharedInitialization) extends Referrable
case class RefJavaField(decls: JavaFields, idx: Int) extends Referrable with JavaNameTarget with JavaDerefTarget
case class RefJavaLocalDeclaration(decls: JavaLocalDeclaration, idx: Int) extends Referrable with JavaNameTarget
case class RefJavaConstructor(decl: JavaConstructor) extends Referrable
case class RefJavaMethod(decl: JavaMethod) extends Referrable with JavaInvocationTarget with ResultTarget
case class RefInstanceFunction(decl: InstanceFunction) extends Referrable with SpecInvocationTarget with ResultTarget
case class RefInstanceMethod(decl: InstanceMethod) extends Referrable with SpecInvocationTarget with ResultTarget
case class RefInstancePredicate(decl: InstancePredicate) extends Referrable with SpecInvocationTarget
case class RefField(decl: Field) extends Referrable with PVLNameTarget with PVLDerefTarget
case class RefVariable(decl: Variable) extends Referrable with SpecNameTarget with SpecTypeNameTarget
case class RefLabelDecl(decl: LabelDecl) extends Referrable
case class RefParBlockDecl(decl: ParBlockDecl) extends Referrable
case class RefParInvariantDecl(decl: ParInvariantDecl) extends Referrable
case class RefADTAxiom(decl: ADTAxiom) extends Referrable
case class RefADTFunction(decl: ADTFunction) extends Referrable with SpecInvocationTarget
case class RefModelField(decl: ModelField) extends Referrable with SpecDerefTarget with SpecNameTarget
case class RefModelProcess(decl: ModelProcess) extends Referrable with SpecInvocationTarget
case class RefModelAction(decl: ModelAction) extends Referrable with SpecInvocationTarget
case class RefPVLConstructor(decl: PVLConstructor) extends Referrable

case class BuiltinField(f: Expr => Expr) extends Referrable with SpecDerefTarget
case class BuiltinInstanceMethod(f: Expr => Seq[Expr] => Expr) extends Referrable with SpecInvocationTarget