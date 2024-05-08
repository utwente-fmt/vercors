package vct.col.resolve.ctx

import vct.col.ast._
import vct.col.origin.SourceName
import vct.col.resolve.NameLost
import vct.col.resolve.lang.{C, CPP}

/**
 * Collection of all things that can be cross-referenced in the AST. This includes all declarations, indices at
 * multi-declarations, references to implicitly defined constructs, built-in fields and methods, etc. Referrables are
 * briefly used while resolving the AST, and are almost immediately replaced by instances of [[vct.col.ref.Ref]]. See
 * also [[vct.col.resolve.ResolveTypes]] and [[vct.col.resolve.ResolveReferences]].
 * @tparam G The generation marker
 */
sealed trait Referrable[G] {
  def name: String = this match {
    case RefCTranslationUnit(_) => ""
    case RefCParam(decl) => C.nameFromDeclarator(decl.declarator)
    case RefCFunctionDefinition(decl) => C.nameFromDeclarator(decl.declarator)
    case RefCGlobalDeclaration(decls, initIdx) => C.nameFromDeclarator(decls.decl.inits(initIdx).decl)
    case RefCLocalDeclaration(decls, initIdx) => C.nameFromDeclarator(decls.decl.inits(initIdx).decl)
    case RefCPPTranslationUnit(_) => ""
    case RefCPPParam(decl) => CPP.nameFromDeclarator(decl.declarator)
    case RefCPPFunctionDefinition(decl) => CPP.nameFromDeclarator(decl.declarator)
    case RefCPPGlobalDeclaration(decls, initIdx) => CPP.nameFromDeclarator(decls.decl.inits(initIdx).decl)
    case RefCPPCustomType(typeName) => typeName
    case RefCPPLambdaDefinition(decl) => ""
    case RefCPPLocalDeclaration(decls, initIdx) => CPP.nameFromDeclarator(decls.decl.inits(initIdx).decl)
    case RefSYCLAccessMode(decl) => decl.name
    case RefSYCLConstructorDefinition(typ) => typ.namespacePath
    case RefJavaNamespace(_) => ""
    case RefUnloadedJavaNamespace(_) => ""
    case RefCStruct(decl: CGlobalDeclaration[_]) => decl.decl match {
      case CDeclaration(_, _, Seq(defn: CStructDeclaration[G]), Seq()) => defn.name.getOrElse("")
      case _ => ???
    }
    case RefCStruct(_) => ???
    case RefCStructField(decls, idx) => C.nameFromDeclarator(decls.decls(idx))
    case RefJavaClass(decl) => decl.name
    case RefSilverField(decl) => Referrable.originName(decl)
    case RefSimplificationRule(decl) => Referrable.originName(decl)
    case RefAxiomaticDataType(decl) => Referrable.originName(decl)
    case RefFunction(decl) => Referrable.originName(decl)
    case RefProcedure(decl) => Referrable.originName(decl)
    case RefVeSUVMainMethod(_) => ""
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
    case RefInstanceOperatorMethod(decl) => Referrable.originName(decl)
    case RefInstanceOperatorFunction(decl) => Referrable.originName(decl)
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
    case RefChoreography(decl) => Referrable.originName(decl)
    case RefEndpoint(decl) => Referrable.originName(decl)
    case RefProverType(decl) => Referrable.originName(decl)
    case RefProverFunction(decl) => Referrable.originName(decl)
    case RefJavaBipGuard(decl) => Referrable.originName(decl)
    case RefLlvmFunctionDefinition(decl) => Referrable.originName(decl)
    case RefLlvmGlobal(decl) => Referrable.originName(decl)
    case RefLlvmSpecFunction(decl) => Referrable.originName(decl)
    case RefBipComponent(decl) => Referrable.originName(decl)
    case RefBipGlue(decl) => ""
    case RefBipGuard(decl) => Referrable.originName(decl)
    case RefBipIncomingData(decl) => Referrable.originName(decl)
    case RefBipOutgoingData(decl) => Referrable.originName(decl)
    case RefBipPort(decl) => Referrable.originName(decl)
    case RefBipPortSynchronization(decl) => ""
    case RefBipStatePredicate(decl) => Referrable.originName(decl)
    case RefBipTransition(decl) => Referrable.originName(decl)
    case RefBipTransitionSynchronization(decl) => ""
    case RefBipConstructor(decl) => Referrable.originName(decl)
    case RefHeapVariable(decl) => Referrable.originName(decl)
    case RefPVLEndpoint(decl) => decl.name
    case RefPVLChoreography(decl) => decl.name
    case RefPVLChorRun(_) => ""

    case RefJavaBipGlueContainer() => ""
    case PVLBuiltinInstanceMethod(_) => ""
    case BuiltinField(_) => ""
    case BuiltinInstanceMethod(_) => ""
    case RefPVLConstructor(decl) => ""
    case ImplicitDefaultJavaConstructor(_) => ""
    case ImplicitDefaultPVLConstructor(_) => ""
    case RefCudaThreadIdx() => "threadIdx"
    case RefCudaBlockDim() => "blockDim"
    case RefCudaBlockIdx() => "blockIdx"
    case RefCudaGridDim() => "gridDim"
    case RefCudaVecX(_) => "x"
    case RefCudaVecY(_) => "y"
    case RefCudaVecZ(_) => "z"
    case RefJavaBipStatePredicate(name, _) => name
    case ImplicitDefaultJavaBipStatePredicate(name) => name
  }
}

case object Referrable {
  def from[G](decl: Declaration[G]): Seq[Referrable[G]] = Seq[Referrable[G]](decl match {
    case decl: CTranslationUnit[G] => RefCTranslationUnit(decl)
    case decl: CParam[G] => RefCParam(decl)
    case decl: CFunctionDefinition[G] => RefCFunctionDefinition(decl)
    case decl: CStructMemberDeclarator[G] => return decl.decls.indices.map(RefCStructField(decl, _))
    case decl: CGlobalDeclaration[G] => decl.decl match {
      case CDeclaration(_, _, Seq(_ :CStructDeclaration[G]), Seq()) => RefCStruct(decl)
      case _ => return decl.decl.inits.indices.map(RefCGlobalDeclaration(decl, _))
    }
    case decl: CPPTranslationUnit[G] => RefCPPTranslationUnit(decl)
    case decl: CPPParam[G] => RefCPPParam(decl)
    case decl: CPPFunctionDefinition[G] => RefCPPFunctionDefinition(decl)
    case decl: CPPGlobalDeclaration[G] => return decl.decl.inits.indices.map(RefCPPGlobalDeclaration(decl, _))
    case decl: JavaNamespace[G] => RefJavaNamespace(decl)
    case decl: JavaClass[G] => RefJavaClass(decl)
    case decl: JavaInterface[G] => RefJavaClass(decl)
    case decl: JavaAnnotationInterface[G] => RefJavaClass(decl)
    case decl: SilverField[G] => RefSilverField(decl)
    case decl: SimplificationRule[G] => RefSimplificationRule(decl)
    case decl: AxiomaticDataType[G] => RefAxiomaticDataType(decl)
    case decl: Function[G] => RefFunction(decl)
    case decl: Procedure[G] => RefProcedure(decl)
    case decl: VeSUVMainMethod[G] => RefVeSUVMainMethod(decl)
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
    case decl: InstanceOperatorMethod[G] => RefInstanceOperatorMethod(decl)
    case decl: InstanceOperatorFunction[G] => RefInstanceOperatorFunction(decl)
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
    case decl: CPPLocalDeclaration[G] => return decl.decl.inits.indices.map(RefCPPLocalDeclaration(decl, _))
    case decl: JavaLocalDeclaration[G] => return decl.decls.indices.map(RefJavaLocalDeclaration(decl, _))
    case decl: PVLConstructor[G] => RefPVLConstructor(decl)
    case decl: Choreography[G] => RefChoreography(decl)
    case decl: Endpoint[G] => RefEndpoint(decl)
    case decl: LlvmFunctionDefinition[G] => RefLlvmFunctionDefinition(decl)
    case decl: LlvmGlobal[G] => RefLlvmGlobal(decl)
    case decl: LlvmSpecFunction[G] => RefLlvmSpecFunction(decl)
    case decl: ProverType[G] => RefProverType(decl)
    case decl: ProverFunction[G] => RefProverFunction(decl)
    case decl: JavaBipGlueContainer[G] => RefJavaBipGlueContainer()
    case decl: BipComponent[G] => RefBipComponent(decl)
    case decl: BipGlue[G] => RefBipGlue(decl)
    case decl: BipGuard[G] => RefBipGuard(decl)
    case decl: BipIncomingData[G] => RefBipIncomingData(decl)
    case decl: BipOutgoingData[G] => RefBipOutgoingData(decl)
    case decl: BipPort[G] => RefBipPort(decl)
    case decl: BipPortSynchronization[G] => RefBipPortSynchronization(decl)
    case decl: BipStatePredicate[G] => RefBipStatePredicate(decl)
    case decl: BipTransition[G] => RefBipTransition(decl)
    case decl: BipTransitionSynchronization[G] => RefBipTransitionSynchronization(decl)
    case decl: BipConstructor[G] => RefBipConstructor(decl)
    case decl: HeapVariable[G] => RefHeapVariable(decl)
    case decl: PVLEndpoint[G] => RefPVLEndpoint(decl)
    case decl: PVLChoreography[G] => RefPVLChoreography(decl)
    case decl: PVLChorRun[G] => RefPVLChorRun(decl)
  })

  def originName(decl: Declaration[_]): String = decl.o.find[SourceName] match {
    case Some(SourceName(name)) => name
    case _ => throw NameLost(decl.o)
  }

  def originNameOrEmpty(decl: Declaration[_]): String = decl.o.find[SourceName] match {
    case Some(SourceName(name)) => name
    case _ => ""
  }
}

/*
The *Target traits enable, in combination with [[Referrable.name]], very lazy name resolution. If we e.g. encounter a
name in Java, we can just search for in-scope JavaNameTarget's with the correct _.name.
 */

sealed trait JavaTypeNameTarget[G] extends Referrable[G] with JavaDerefTarget[G]
sealed trait CTypeNameTarget[G] extends Referrable[G]
sealed trait CStructTarget[G] extends Referrable[G]
sealed trait CPPTypeNameTarget[G] extends Referrable[G]
sealed trait PVLTypeNameTarget[G] extends Referrable[G]
sealed trait SpecTypeNameTarget[G] extends JavaTypeNameTarget[G] with CTypeNameTarget[G] with CPPTypeNameTarget[G] with PVLTypeNameTarget[G]

sealed trait JavaNameTarget[G] extends Referrable[G]
sealed trait CNameTarget[G] extends Referrable[G]
sealed trait CPPNameTarget[G] extends Referrable[G]
sealed trait PVLNameTarget[G] extends Referrable[G]
sealed trait SpecNameTarget[G] extends CNameTarget[G] with CPPNameTarget[G] with JavaNameTarget[G] with PVLNameTarget[G]

sealed trait CDerefTarget[G] extends Referrable[G]
sealed trait CPPDerefTarget[G] extends Referrable[G]
sealed trait JavaDerefTarget[G] extends Referrable[G]
sealed trait PVLDerefTarget[G] extends Referrable[G]
sealed trait SpecDerefTarget[G] extends CDerefTarget[G] with CPPDerefTarget[G] with JavaDerefTarget[G] with PVLDerefTarget[G]

sealed trait JavaInvocationTarget[G] extends Referrable[G]
sealed trait CInvocationTarget[G] extends Referrable[G]
sealed trait CPPInvocationTarget[G] extends Referrable[G]
sealed trait PVLInvocationTarget[G] extends Referrable[G]
sealed trait LlvmInvocationTarget[G] extends Referrable[G]
sealed trait SpecInvocationTarget[G]
  extends JavaInvocationTarget[G]
    with CNameTarget[G]
    with CDerefTarget[G] with CInvocationTarget[G]
    with CPPNameTarget[G]
    with CPPDerefTarget[G] with CPPInvocationTarget[G]
    with PVLInvocationTarget[G]
    with LlvmInvocationTarget[G]

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
case class RefCPPTranslationUnit[G](decl: CPPTranslationUnit[G]) extends Referrable[G]
case class RefCPPParam[G](decl: CPPParam[G]) extends Referrable[G] with CPPNameTarget[G]
case class RefCPPFunctionDefinition[G](decl: CPPFunctionDefinition[G]) extends Referrable[G] with CPPNameTarget[G] with CPPInvocationTarget[G] with ResultTarget[G]
case class RefCPPLambdaDefinition[G](decl: CPPLambdaDefinition[G]) extends Referrable[G] with CPPInvocationTarget[G] with CPPTypeNameTarget[G]
case class RefCPPCustomType[G](typeName: String) extends Referrable[G] with CPPTypeNameTarget[G]
case class RefCPPGlobalDeclaration[G](decls: CPPGlobalDeclaration[G], initIdx: Int) extends Referrable[G] with CPPNameTarget[G] with CPPInvocationTarget[G] with CPPDerefTarget[G] with ResultTarget[G]
case class RefCPPLocalDeclaration[G](decls: CPPLocalDeclaration[G], initIdx: Int) extends Referrable[G] with CPPNameTarget[G]
case class RefSYCLAccessMode[G](decl: SYCLAccessMode[G]) extends Referrable[G] with CPPNameTarget[G]
case class RefSYCLConstructorDefinition[G](typ: SYCLTConstructableClass[G]) extends Referrable[G] with CPPNameTarget[G] with CPPInvocationTarget[G]
case class RefJavaNamespace[G](decl: JavaNamespace[G]) extends Referrable[G]
case class RefUnloadedJavaNamespace[G](names: Seq[String]) extends Referrable[G] with JavaNameTarget[G] with JavaDerefTarget[G]
case class RefCStruct[G](decl: CGlobalDeclaration[G]) extends Referrable[G] with CStructTarget[G] with CNameTarget[G] with CDerefTarget[G]
case class RefCStructField[G](decls: CStructMemberDeclarator[G], idx: Int) extends Referrable[G] with CNameTarget[G] with CDerefTarget[G]
case class RefJavaClass[G](decl: JavaClassOrInterface[G]) extends Referrable[G] with JavaTypeNameTarget[G] with JavaNameTarget[G] with JavaDerefTarget[G] with ThisTarget[G]
case class RefSilverField[G](decl: SilverField[G]) extends Referrable[G]
case class RefSimplificationRule[G](decl: SimplificationRule[G]) extends Referrable[G]
case class RefAxiomaticDataType[G](decl: AxiomaticDataType[G]) extends Referrable[G] with SpecTypeNameTarget[G] with SpecNameTarget[G] with JavaDerefTarget[G]
case class RefFunction[G](decl: Function[G]) extends Referrable[G] with SpecInvocationTarget[G] with ResultTarget[G]
case class RefProcedure[G](decl: Procedure[G]) extends Referrable[G] with SpecInvocationTarget[G] with ResultTarget[G]
case class RefVeSUVMainMethod[G](decl: VeSUVMainMethod[G]) extends Referrable[G] with SpecInvocationTarget[G] with ResultTarget[G]
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
case class RefJavaParam[G](decl: JavaParam[G]) extends Referrable[G] with JavaNameTarget[G]
case class RefJavaAnnotationMethod[G](decl: JavaAnnotationMethod[G]) extends Referrable[G] with JavaInvocationTarget[G] with ResultTarget[G]
case class RefInstanceFunction[G](decl: InstanceFunction[G]) extends Referrable[G] with SpecInvocationTarget[G] with ResultTarget[G]
case class RefInstanceMethod[G](decl: InstanceMethod[G]) extends Referrable[G] with SpecInvocationTarget[G] with ResultTarget[G]
case class RefInstanceOperatorMethod[G](decl: InstanceOperatorMethod[G]) extends Referrable[G] with ResultTarget[G]
case class RefInstanceOperatorFunction[G](decl: InstanceOperatorFunction[G]) extends Referrable[G] with ResultTarget[G]
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
case class RefJavaBipStatePredicate[G](state: String, decl: JavaAnnotation[G]) extends Referrable[G] with JavaBipStatePredicateTarget[G]
case class RefJavaBipGuard[G](decl: JavaMethod[G]) extends Referrable[G] with JavaNameTarget[G]
case class RefJavaBipGlueContainer[G]() extends Referrable[G] // Bip glue jobs are not actually referrable
case class RefLlvmFunctionDefinition[G](decl: LlvmFunctionDefinition[G]) extends Referrable[G] with LlvmInvocationTarget[G] with ResultTarget[G]
case class RefLlvmGlobal[G](decl: LlvmGlobal[G]) extends Referrable[G]
case class RefBipComponent[G](decl: BipComponent[G]) extends Referrable[G]
case class RefBipGlue[G](decl: BipGlue[G]) extends Referrable[G]
case class RefBipGuard[G](decl: BipGuard[G]) extends Referrable[G]
case class RefBipIncomingData[G](decl: BipIncomingData[G]) extends Referrable[G]
case class RefBipOutgoingData[G](decl: BipOutgoingData[G]) extends Referrable[G]
case class RefBipPort[G](decl: BipPort[G]) extends Referrable[G]
case class RefBipPortSynchronization[G](decl: BipPortSynchronization[G]) extends Referrable[G]
case class RefBipStatePredicate[G](decl: BipStatePredicate[G]) extends Referrable[G]
case class RefBipTransition[G](decl: BipTransition[G]) extends Referrable[G]
case class RefBipTransitionSynchronization[G](decl: BipTransitionSynchronization[G]) extends Referrable[G]
case class RefBipConstructor[G](decl: BipConstructor[G]) extends Referrable[G]
case class RefHeapVariable[G](decl: HeapVariable[G]) extends Referrable[G]
case class RefPVLEndpoint[G](decl: PVLEndpoint[G]) extends Referrable[G] with PVLNameTarget[G]
case class RefPVLChoreography[G](decl: PVLChoreography[G]) extends Referrable[G] with ThisTarget[G]
case class RefPVLChorRun[G](decl: PVLChorRun[G]) extends Referrable[G]

case class RefLlvmSpecFunction[G](decl: LlvmSpecFunction[G]) extends Referrable[G] with LlvmInvocationTarget[G] with ResultTarget[G]
case class RefChoreography[G](decl: Choreography[G]) extends Referrable[G] with ThisTarget[G]
case class RefEndpoint[G](decl: Endpoint[G]) extends Referrable[G]
case class RefProverType[G](decl: ProverType[G]) extends Referrable[G] with SpecTypeNameTarget[G]
case class RefProverFunction[G](decl: ProverFunction[G]) extends Referrable[G] with SpecInvocationTarget[G]

case class BuiltinField[G](f: Expr[G] => Expr[G]) extends Referrable[G] with SpecDerefTarget[G]
case class BuiltinInstanceMethod[G](f: Expr[G] => Seq[Expr[G]] => Expr[G]) extends Referrable[G] with SpecInvocationTarget[G]

case class PVLBuiltinInstanceMethod[G](f: Expr[G] => Seq[Expr[G]] => Expr[G]) extends Referrable[G] with PVLInvocationTarget[G]

case class ImplicitDefaultJavaConstructor[G](cls: JavaClass[G]) extends Referrable[G] with JavaConstructorTarget[G]
case class ImplicitDefaultPVLConstructor[G](cls: Class[G]) extends Referrable[G] with PVLConstructorTarget[G]

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
