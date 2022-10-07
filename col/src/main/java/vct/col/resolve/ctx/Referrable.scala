package vct.col.resolve.ctx

import vct.col.ast._
import vct.col.origin.SourceNameOrigin
import vct.col.resolve.NameLost
import vct.col.resolve.lang.C

sealed trait Referrable[G] {
  def name: String = this match {
    case RefCTranslationUnit(_) => ""
    case RefCParam(decl) => C.nameFromDeclarator(decl.declarator)
    case RefCFunctionDefinition(decl) => C.nameFromDeclarator(decl.declarator)
    case RefCGlobalDeclaration(decls, initIdx) => C.nameFromDeclarator(decls.decl.inits(initIdx).decl)
    case RefCLocalDeclaration(decls, initIdx) => C.nameFromDeclarator(decls.decl.inits(initIdx).decl)
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
    case RefEnum(decl) => Referrable.originName(decl)
    case RefEnumConstant(_, decl) => Referrable.originName(decl)
    case RefJavaSharedInitialization(decl) => ""
    case RefJavaField(decls, idx) => decls.decls(idx).name
    case RefJavaLocalDeclaration(decls, idx) => decls.decls(idx).name
    case RefJavaConstructor(decl) => decl.name
    case RefJavaMethod(decl) => decl.name
    case RefJavaParam(decl) => decl.name
    case RefJavaAnnotationMethod(decl) => decl.name
    case RefInstanceFunction(decl) => Referrable.originName(decl)
    case RefInstanceMethod(decl) => Referrable.originName(decl)
    case RefInstancePredicate(decl) => Referrable.originName(decl)
    case RefField(decl) => Referrable.originName(decl)
    case RefVariable(decl) => Referrable.originName(decl)
    case RefLabelDecl(decl) => Referrable.originName(decl)
    case RefSendDecl(decl) => Referrable.originName(decl)
    case RefRunMethod(_) => ""
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
    case ImplicitDefaultJavaConstructor() => ""
    case ImplicitDefaultPVLConstructor() => ""
    case RefCudaThreadIdx() => "threadIdx"
    case RefCudaBlockDim() => "blockDim"
    case RefCudaBlockIdx() => "blockIdx"
    case RefCudaGridDim() => "gridDim"
    case RefCudaVecX(_) => "x"
    case RefCudaVecY(_) => "y"
    case RefCudaVecZ(_) => "z"
  }
}

case object Referrable {
  def from[G](decl: Declaration[G]): Seq[Referrable[G]] = Seq[Referrable[G]](decl match {
    case decl: CTranslationUnit[G] => RefCTranslationUnit(decl)
    case decl: CParam[G] => RefCParam(decl)
    case decl: CFunctionDefinition[G] => RefCFunctionDefinition(decl)
    case decl: CGlobalDeclaration[G] => return decl.decl.inits.indices.map(RefCGlobalDeclaration(decl, _))
    case decl: JavaNamespace[G] => RefJavaNamespace(decl)
    case decl: JavaClass[G] => RefJavaClass(decl)
    case decl: JavaInterface[G] => RefJavaClass(decl)
    case decl: JavaAnnotationInterface[G] => RefJavaClass(decl)
    case decl: SilverField[G] => RefSilverField(decl)
    case decl: SimplificationRule[G] => RefSimplificationRule(decl)
    case decl: AxiomaticDataType[G] => RefAxiomaticDataType(decl)
    case decl: Function[G] => RefFunction(decl)
    case decl: Procedure[G] => RefProcedure(decl)
    case decl: Predicate[G] => RefPredicate(decl)
    case decl: Class[G] => RefClass(decl)
    case decl: Model[G] => RefModel(decl)
    case decl: Enum[G] => RefEnum(decl)
    case decl: EnumConstant[G] => RefEnumConstant(None, decl)
    case decl: JavaSharedInitialization[G] => RefJavaSharedInitialization(decl)
    case decl: JavaFields[G] => return decl.decls.indices.map(RefJavaField(decl, _))
    case decl: JavaConstructor[G] => RefJavaConstructor(decl)
    case decl: JavaMethod[G] => RefJavaMethod(decl)
    case decl: JavaParam[G] => RefJavaParam(decl)
    case decl: JavaAnnotationMethod[G] => RefJavaAnnotationMethod(decl)
    case decl: InstanceFunction[G] => RefInstanceFunction(decl)
    case decl: InstanceMethod[G] => RefInstanceMethod(decl)
    case decl: InstancePredicate[G] => RefInstancePredicate(decl)
    case decl: InstanceField[G] => RefField(decl)
    case decl: Variable[G] => RefVariable(decl)
    case decl: LabelDecl[G] => RefLabelDecl(decl)
    case decl: SendDecl[G] => RefSendDecl(decl)
    case decl: RunMethod[G] => RefRunMethod(decl)
    case decl: ParBlockDecl[G] => RefParBlockDecl(decl)
    case decl: ParInvariantDecl[G] => RefParInvariantDecl(decl)
    case decl: ADTAxiom[G] => RefADTAxiom(decl)
    case decl: ADTFunction[G] => RefADTFunction(decl)
    case decl: ModelField[G] => RefModelField(decl)
    case decl: ModelProcess[G] => RefModelProcess(decl)
    case decl: ModelAction[G] => RefModelAction(decl)
    case decl: CLocalDeclaration[G] => return decl.decl.inits.indices.map(RefCLocalDeclaration(decl, _))
    case decl: JavaLocalDeclaration[G] => return decl.decls.indices.map(RefJavaLocalDeclaration(decl, _))
    case decl: PVLConstructor[G] => RefPVLConstructor(decl)
  })

  def originName(decl: Declaration[_]): String = decl.o match {
    case SourceNameOrigin(name, _) => name
    case _ => throw NameLost(decl.o)
  }

  def originNameOrEmpty(decl: Declaration[_]): String = decl.o match {
    case SourceNameOrigin(name, _) => name
    case _ => ""
  }
}

sealed trait JavaTypeNameTarget[G] extends Referrable[G] with JavaDerefTarget[G]
sealed trait CTypeNameTarget[G] extends Referrable[G]
sealed trait PVLTypeNameTarget[G] extends Referrable[G]
sealed trait SpecTypeNameTarget[G] extends JavaTypeNameTarget[G] with CTypeNameTarget[G] with PVLTypeNameTarget[G]

sealed trait JavaNameTarget[G] extends Referrable[G]
sealed trait CNameTarget[G] extends Referrable[G]
sealed trait PVLNameTarget[G] extends Referrable[G]
sealed trait SpecNameTarget[G] extends CNameTarget[G] with JavaNameTarget[G] with PVLNameTarget[G]

sealed trait CDerefTarget[G] extends Referrable[G]
sealed trait JavaDerefTarget[G] extends Referrable[G]
sealed trait PVLDerefTarget[G] extends Referrable[G]
sealed trait SpecDerefTarget[G] extends CDerefTarget[G] with JavaDerefTarget[G] with PVLDerefTarget[G]

sealed trait JavaInvocationTarget[G] extends Referrable[G]
sealed trait CInvocationTarget[G] extends Referrable[G]
sealed trait PVLInvocationTarget[G] extends Referrable[G]
sealed trait SpecInvocationTarget[G]
  extends JavaInvocationTarget[G]
    with CDerefTarget[G] with CInvocationTarget[G]
    with PVLInvocationTarget[G]

sealed trait ThisTarget[G] extends Referrable[G]

sealed trait ResultTarget[G] extends Referrable[G]

sealed trait JavaConstructorTarget[G] extends Referrable[G]
sealed trait PVLConstructorTarget[G] extends Referrable[G]

sealed trait JavaBipStatePredicateTarget[G] extends Referrable[G]

case class RefCTranslationUnit[G](decl: CTranslationUnit[G]) extends Referrable[G]
case class RefCParam[G](decl: CParam[G]) extends Referrable[G] with CNameTarget[G]
case class RefCFunctionDefinition[G](decl: CFunctionDefinition[G]) extends Referrable[G] with CNameTarget[G] with CInvocationTarget[G] with ResultTarget[G]
case class RefCGlobalDeclaration[G](decls: CGlobalDeclaration[G], initIdx: Int) extends Referrable[G] with CNameTarget[G] with CInvocationTarget[G] with ResultTarget[G]
case class RefCLocalDeclaration[G](decls: CLocalDeclaration[G], initIdx: Int) extends Referrable[G] with CNameTarget[G]
case class RefJavaNamespace[G](decl: JavaNamespace[G]) extends Referrable[G]
case class RefUnloadedJavaNamespace[G](names: Seq[String]) extends Referrable[G] with JavaNameTarget[G] with JavaDerefTarget[G]
case class RefJavaClass[G](decl: JavaClassOrInterface[G]) extends Referrable[G] with JavaTypeNameTarget[G] with JavaNameTarget[G] with JavaDerefTarget[G] with ThisTarget[G]
case class RefSilverField[G](decl: SilverField[G]) extends Referrable[G]
case class RefSimplificationRule[G](decl: SimplificationRule[G]) extends Referrable[G]
case class RefAxiomaticDataType[G](decl: AxiomaticDataType[G]) extends Referrable[G] with SpecTypeNameTarget[G] with SpecNameTarget[G] with JavaDerefTarget[G]
case class RefFunction[G](decl: Function[G]) extends Referrable[G] with SpecInvocationTarget[G] with ResultTarget[G]
case class RefProcedure[G](decl: Procedure[G]) extends Referrable[G] with SpecInvocationTarget[G] with ResultTarget[G]
case class RefPredicate[G](decl: Predicate[G]) extends Referrable[G] with SpecInvocationTarget[G]
case class RefClass[G](decl: Class[G]) extends Referrable[G] with PVLTypeNameTarget[G] with SpecNameTarget[G] with ThisTarget[G]
case class RefModel[G](decl: Model[G]) extends Referrable[G] with SpecTypeNameTarget[G] with ThisTarget[G] with PVLConstructorTarget[G] with JavaConstructorTarget[G]
case class RefEnum[G](decl: Enum[G]) extends Referrable[G] with SpecTypeNameTarget[G] with SpecNameTarget[G]
case class RefEnumConstant[G](enum: Option[Enum[G]], decl: EnumConstant[G]) extends Referrable[G] with SpecDerefTarget[G] with SpecNameTarget[G]
case class RefJavaSharedInitialization[G](decl: JavaSharedInitialization[G]) extends Referrable[G]
case class RefJavaField[G](decls: JavaFields[G], idx: Int) extends Referrable[G] with JavaNameTarget[G] with JavaDerefTarget[G]
case class RefJavaLocalDeclaration[G](decls: JavaLocalDeclaration[G], idx: Int) extends Referrable[G] with JavaNameTarget[G]
case class RefJavaConstructor[G](decl: JavaConstructor[G]) extends Referrable[G] with JavaConstructorTarget[G]
case class RefJavaMethod[G](decl: JavaMethod[G]) extends Referrable[G] with JavaInvocationTarget[G] with ResultTarget[G]
case class RefJavaParam[G](decl: JavaParam[G]) extends Referrable[G] with JavaNameTarget[G] with JavaDerefTarget[G]
case class RefJavaAnnotationMethod[G](decl: JavaAnnotationMethod[G]) extends Referrable[G] with JavaInvocationTarget[G] with ResultTarget[G]
case class RefInstanceFunction[G](decl: InstanceFunction[G]) extends Referrable[G] with SpecInvocationTarget[G] with ResultTarget[G]
case class RefInstanceMethod[G](decl: InstanceMethod[G]) extends Referrable[G] with SpecInvocationTarget[G] with ResultTarget[G]
case class RefInstancePredicate[G](decl: InstancePredicate[G]) extends Referrable[G] with SpecInvocationTarget[G]
case class RefField[G](decl: InstanceField[G]) extends Referrable[G] with PVLNameTarget[G] with PVLDerefTarget[G]
case class RefVariable[G](decl: Variable[G]) extends Referrable[G] with SpecNameTarget[G] with SpecTypeNameTarget[G]
case class RefLabelDecl[G](decl: LabelDecl[G]) extends Referrable[G]
case class RefSendDecl[G](decl: SendDecl[G]) extends Referrable[G]
case class RefRunMethod[G](decl: RunMethod[G]) extends Referrable[G]
case class RefParBlockDecl[G](decl: ParBlockDecl[G]) extends Referrable[G]
case class RefParInvariantDecl[G](decl: ParInvariantDecl[G]) extends Referrable[G]
case class RefADTAxiom[G](decl: ADTAxiom[G]) extends Referrable[G]
case class RefADTFunction[G](decl: ADTFunction[G]) extends Referrable[G] with SpecInvocationTarget[G]
case class RefModelField[G](decl: ModelField[G]) extends Referrable[G] with SpecDerefTarget[G] with SpecNameTarget[G]
case class RefModelProcess[G](decl: ModelProcess[G]) extends Referrable[G] with SpecInvocationTarget[G]
case class RefModelAction[G](decl: ModelAction[G]) extends Referrable[G] with SpecInvocationTarget[G]
case class RefPVLConstructor[G](decl: PVLConstructor[G]) extends Referrable[G] with PVLConstructorTarget[G]
case class RefJavaBipStatePredicate[G](decl: JavaAnnotation[G]) extends Referrable[G] with JavaBipStatePredicateTarget[G]
case class RefJavaBipGuard[G](decl: JavaMethod[G]) extends Referrable[G] with JavaNameTarget[G]

case class BuiltinField[G](f: Expr[G] => Expr[G]) extends Referrable[G] with SpecDerefTarget[G]
case class BuiltinInstanceMethod[G](f: Expr[G] => Seq[Expr[G]] => Expr[G]) extends Referrable[G] with SpecInvocationTarget[G]

case class PVLBuiltinInstanceMethod[G](f: Expr[G] => Seq[Expr[G]] => Expr[G]) extends Referrable[G] with PVLInvocationTarget[G]

case class ImplicitDefaultJavaConstructor[G]() extends Referrable[G] with JavaConstructorTarget[G]
case class ImplicitDefaultPVLConstructor[G]() extends Referrable[G] with PVLConstructorTarget[G]

case class ImplicitDefaultJavaBipStatePredicate[G](state: String) extends Referrable[G] with JavaBipStatePredicateTarget[G]

sealed trait RefCudaVec[G] extends Referrable[G] with CNameTarget[G]
case class RefCudaThreadIdx[G]() extends RefCudaVec[G]
case class RefCudaBlockDim[G]() extends RefCudaVec[G]
case class RefCudaBlockIdx[G]() extends RefCudaVec[G]
case class RefCudaGridDim[G]() extends RefCudaVec[G]

sealed trait RefCudaVecDim[G] extends Referrable[G] with CDerefTarget[G] {
  def vec: RefCudaVec[G]
}
case class RefCudaVecX[G](vec: RefCudaVec[G]) extends RefCudaVecDim[G]
case class RefCudaVecY[G](vec: RefCudaVec[G]) extends RefCudaVecDim[G]
case class RefCudaVecZ[G](vec: RefCudaVec[G]) extends RefCudaVecDim[G]
