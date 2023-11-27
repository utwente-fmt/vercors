package vct.col.resolve.lang

import hre.util.FuncTools
import vct.col.ast._
import vct.col.origin._
import vct.col.resolve._
import vct.col.resolve.ctx._
import vct.col.typerules.Types
import vct.result.VerificationError.UserError

import scala.collection.immutable.Seq

case object CPP {
  implicit private val o: Origin = DiagnosticOrigin

  private case class CPPTypeNotSupported(node: Option[Node[_]]) extends UserError {
    override def code: String = "cppTypeNotSupported"

    override def text: String = {
      (node match {
        case Some(node) => node.o.messageInContext(_)
        case None => (text: String) => text
      })("This type is not supported by VerCors.")
    }
  }

  val NUMBER_LIKE_PREFIXES: Seq[Seq[CPPDeclarationSpecifier[_]]] = Seq(
    Nil,
    Seq(CPPUnsigned()),
    Seq(CPPSigned()),
  )

  val NUMBER_LIKE_TYPES: Seq[Seq[CPPDeclarationSpecifier[_]]] = Seq(
    Seq(CPPInt()),
    Seq(CPPLong()),
    Seq(CPPLong(), CPPInt()),
    Seq(CPPLong(), CPPLong()),
    Seq(CPPLong(), CPPLong(), CPPInt()),
  )

  val NUMBER_LIKE_SPECIFIERS: Seq[Seq[CPPDeclarationSpecifier[_]]] =
    for (prefix <- NUMBER_LIKE_PREFIXES; t <- NUMBER_LIKE_TYPES)
      yield prefix ++ t

  case class DeclaratorInfo[G](params: Option[Seq[CPPParam[G]]], typeOrReturnType: Type[G] => Type[G], name: String)

  def getDeclaratorInfo[G](decl: CPPDeclarator[G], isParam: Boolean = false): DeclaratorInfo[G] = decl match {
    case CPPAddressingDeclarator(operators, inner) =>
      val innerInfo = getDeclaratorInfo(inner)
      if (isParam && operators.size == 1 && operators.head.isInstanceOf[CPPReference[G]]) {
        // Pass by reference parameter, so & can be ignored
        DeclaratorInfo(
          innerInfo.params,
          t => innerInfo.typeOrReturnType(t),
          innerInfo.name)
      } else if (operators.collectFirst({ case x: CPPReference[G] => x }).isDefined) {
        // Do not support multiple &, or & later in the sequence
        throw CPPTypeNotSupported(Some(decl))
      } else {
        DeclaratorInfo(
          innerInfo.params,
          t => innerInfo.typeOrReturnType(FuncTools.repeat[Type[G]](TPointer(_), operators.size, t)),
          innerInfo.name)
      }
    case array@CPPArrayDeclarator(inner, size) =>
      val innerInfo = getDeclaratorInfo(inner)
      DeclaratorInfo(innerInfo.params, t => innerInfo.typeOrReturnType(CPPTArray(size, t)(array.blame)), innerInfo.name)
    case CPPTypedFunctionDeclarator(params, _, inner) =>
      val innerInfo = getDeclaratorInfo(inner)
      DeclaratorInfo(params = Some(params), typeOrReturnType = t => t, innerInfo.name)
    case CPPLambdaDeclarator(params) =>
      DeclaratorInfo(params = Some(params), typeOrReturnType = t => t, "")
    case CPPName(name) =>
      DeclaratorInfo(params = None, typeOrReturnType = t => t, name)
  }

  def getBaseTypeFromSpecs[G](specs: Seq[CPPDeclarationSpecifier[G]], context: Option[Node[G]] = None): Type[G] =
    specs.collect { case spec: CPPTypeSpecifier[G] => spec } match {
      case Seq(CPPVoid()) => TVoid()
      case Seq(CPPChar()) => TChar()
      case t if CPP.NUMBER_LIKE_SPECIFIERS.contains(t) => TInt()
      case Seq(CPPSpecificationType(t@TFloat(_, _))) => t
      case Seq(CPPBool()) => TBool()
      case Seq(SYCLClassDefName("event", Seq())) => SYCLTEvent()
      case Seq(SYCLClassDefName("handler", Seq())) => SYCLTHandler()
      case Seq(SYCLClassDefName("queue", Seq())) => SYCLTQueue()
      case Seq(SYCLClassDefName(name, Seq(CPPExprOrTypeSpecifier(Some(IntegerValue(dim)), None)))) =>
        name match {
          case "item" => SYCLTItem(dim.intValue)
          case "nd_item" => SYCLTNDItem(dim.intValue)
          case "range" => SYCLTRange(dim.intValue)
          case "nd_range" => SYCLTNDRange(dim.intValue)
          case _ => throw CPPTypeNotSupported(context)
        }
      case Seq(SYCLClassDefName(name, Seq(CPPExprOrTypeSpecifier(None, Some(typ)), CPPExprOrTypeSpecifier(Some(IntegerValue(dim)), None)))) =>
        val baseType = getBaseTypeFromSpecs(Seq(typ))
        name match {
          case "buffer" => SYCLTBuffer(baseType, dim.intValue)
          case "accessor" => SYCLTAccessor(baseType, dim.intValue)
          case "local_accessor" => SYCLTLocalAccessor(baseType, dim.intValue)
          case _ => throw CPPTypeNotSupported(context)
        }
      case Seq(CPPTypedefName("VERCORS::LAMBDA", _)) => CPPTLambda()
      case Seq(CPPTypedefName("VERCORS::ARRAY", Seq(CPPExprOrTypeSpecifier(None, Some(typ)), CPPExprOrTypeSpecifier(Some(IntegerValue(dim)), None)))) =>
        FuncTools.repeat(TArray[G](_), dim.toInt, getBaseTypeFromSpecs(Seq(typ)))
      case Seq(defn@CPPTypedefName(_, _)) => Types.notAValue(defn.ref.get)
      case Seq(CPPSpecificationType(typ)) => typ
      case spec +: _ => throw CPPTypeNotSupported(context.orElse(Some(spec)))
      case _ => throw CPPTypeNotSupported(context)
    }

  def unwrappedType[G](t: Type[G]): Type[G] = t match {
    case CPPPrimitiveType(specs) => getBaseTypeFromSpecs(specs)
    case _ => t
  }

  def nameFromDeclarator(declarator: CPPDeclarator[_]): String =
    getDeclaratorInfo(declarator).name

  def typeOrReturnTypeFromDeclarator[G](specs: Seq[CPPDeclarationSpecifier[G]], decl: CPPDeclarator[G]): Type[G] = {
    getDeclaratorInfo(decl).typeOrReturnType(CPPPrimitiveType(specs)) match {
      case CPPPrimitiveType(returnedSpecs) => getBaseTypeFromSpecs(returnedSpecs)
      case x => x
    }

  }

  def paramsFromDeclarator[G](declarator: CPPDeclarator[G]): Seq[CPPParam[G]] =
    getDeclaratorInfo(declarator).params.get

  def findCPPTypeName[G](name: String, ctx: TypeResolutionContext[G]): Option[CPPTypeNameTarget[G]] = name match {
    case "VERCORS::LAMBDA" | "VERCORS::ARRAY" => Some(RefCPPCustomType(name))
    case _ => ctx.stack.flatten.collectFirst {
      case target: CPPTypeNameTarget[G] if target.name == name => target
    }
  }

  def findCPPName[G](name: String, ctx: ReferenceResolutionContext[G]): Seq[CPPNameTarget[G]] = {
    val targets = ctx.stack.flatten.collect {
      case target: CPPNameTarget[G] if target.name == name => target
    }
    if (targets.nonEmpty) return targets

    name match {
      case "sycl::read_write" => Seq(RefSYCLAccessMode(SYCLReadWriteAccess()))
      case "sycl::read_only" => Seq(RefSYCLAccessMode(SYCLReadOnlyAccess()))
      case "sycl::range" => Seq(RefSYCLConstructorDefinition(SYCLTRange()))
      case "sycl::nd_range" => Seq(RefSYCLConstructorDefinition(SYCLTNDRange()))
      case "sycl::buffer" => Seq(RefSYCLConstructorDefinition(SYCLTBuffer()))
      case "sycl::accessor" => Seq(RefSYCLConstructorDefinition(SYCLTAccessor()))
      case "sycl::local_accessor" => Seq(RefSYCLConstructorDefinition(SYCLTLocalAccessor()))
      case _ => Seq()
    }
  }

  def findDeref[G](obj: Expr[G], name: String, ctx: ReferenceResolutionContext[G], blame: Blame[BuiltinError]): Seq[CPPDerefTarget[G]] =
    CPP.unwrappedType(obj.t) match {
      case t: TNotAValue[G] => t.decl.get match {
        case RefAxiomaticDataType(decl) => decl.decls.flatMap(Referrable.from).collect {
          case ref: RefADTFunction[G] if ref.name == name => ref
        }
        case _ => Nil
      }
      case t: SYCLTClass[G] => findCPPName(t.namespacePath + "::" + name, ctx).collect({
        case decl: CPPDerefTarget[G] if decl.isInstanceOf[CPPInvocationTarget[G]] => decl
      })
      case _ => Spec.builtinField(obj, name, blame).toSeq
    }

  def findForwardDeclaration[G](declarator: CPPDeclarator[G], ctx: ReferenceResolutionContext[G]): Option[RefCPPGlobalDeclaration[G]] =
    ctx.stack.flatten.collectFirst {
      case target: RefCPPGlobalDeclaration[G] if target.name == nameFromDeclarator(declarator) => target
    }

  def findDefinition[G](declarator: CPPDeclarator[G], ctx: ReferenceResolutionContext[G]): Option[RefCPPFunctionDefinition[G]] =
    ctx.stack.flatten.collectFirst {
      case target: RefCPPFunctionDefinition[G] if target.name == nameFromDeclarator(declarator) => target
    }

  def getParamTypes[G](ref: CPPInvocationTarget[G]): Seq[Type[G]] = ref match {
    case globalDeclRef: RefCPPGlobalDeclaration[G] if globalDeclRef.decls.decl.inits.size == 1 =>
      paramsFromDeclarator(globalDeclRef.decls.decl.inits.head.decl).map(param => getBaseTypeFromSpecs(param.specifiers))
    case functionDeclRef: RefCPPFunctionDefinition[G] =>
      paramsFromDeclarator(functionDeclRef.decl.declarator).map(param => getBaseTypeFromSpecs(param.specifiers))
    case functionRef: RefFunction[G] => functionRef.decl.args.map(variable => variable.t)
    case procedureRef: RefProcedure[G] => procedureRef.decl.args.map(variable => variable.t)
    case predicateRef: RefPredicate[G] => predicateRef.decl.args.map(variable => variable.t)
    case instanceFunctionRef: RefInstanceFunction[G] => instanceFunctionRef.decl.args.map(variable => variable.t)
    case instanceMethodRef: RefInstanceMethod[G] => instanceMethodRef.decl.args.map(variable => variable.t)
    case instancePredicateRef: RefInstancePredicate[G] => instancePredicateRef.decl.args.map(variable => variable.t)
    case aDTFunctionRef: RefADTFunction[G] => aDTFunctionRef.decl.args.map(variable => variable.t)
    case modelProcessRef: RefModelProcess[G] => modelProcessRef.decl.args.map(variable => variable.t)
    case modelActionRef: RefModelAction[G] => modelActionRef.decl.args.map(variable => variable.t)
    case proverFunctionRef: RefProverFunction[G] => proverFunctionRef.decl.args.map(variable => variable.t)
    case _ => Seq()
  }

  def resolveInvocation[G](applicable: Expr[G], args: Seq[Expr[G]], ctx: ReferenceResolutionContext[G]): CPPInvocationTarget[G] =
    applicable.t match {
      case t: TNotAValue[G] => (t.decl.get, applicable) match {
        // Do not check arguments for BuiltinInstanceMethods as we do not know the argument types
        case (target: BuiltinInstanceMethod[G], _) => target
        // Resolve the SYCL constructor
        case (RefSYCLConstructorDefinition(typ), CPPLocal(_, genericArgs)) => typ.findConstructor(genericArgs, args) match {
          case Some(constructorRef) => constructorRef
          case None => throw NotApplicable(applicable)
        }
        // The previously found method is already the correct one
        case (target: CPPInvocationTarget[G], _) if Util.compatTypes(args, getParamTypes(target)) => target
        case (_, local@CPPLocal(name, genericArgs)) =>
          // Currently linked method does not have correct params
          // So find all declarations with correct name and see if there is
          // an alternative whose parameters do match the arguments
          val allDecls = findCPPName(name, ctx).collect({case decl: CPPInvocationTarget[G] => decl})
          val foundMatch: CPPInvocationTarget[G] = findAlternativeInvocationTarget(applicable, args, allDecls)
          local.ref = Some(foundMatch.asInstanceOf[CPPNameTarget[G]])
          t.decl = Some(foundMatch)
          foundMatch
        case (_, deref@CPPClassMethodOrFieldAccess(classInstance, name)) =>
          // Currently linked method does not have correct params
          // So find all declarations with correct name and see if there is
          // an alternative whose parameters do match the arguments
          val allDecls = findDeref(classInstance, name, ctx, deref.blame).collect({
            case target: CPPInvocationTarget[G] => target
          })
          val foundMatch: CPPInvocationTarget[G] = findAlternativeInvocationTarget(applicable, args, allDecls)
          deref.ref = Some(foundMatch.asInstanceOf[CPPDerefTarget[G]])
          t.decl = Some(foundMatch)
          foundMatch
        case _ => throw NotApplicable(applicable)
      }
      case _ => throw NotApplicable(applicable)
    }

  private def findAlternativeInvocationTarget[G](applicable: Expr[G], args: Seq[Expr[G]], decls: Seq[CPPInvocationTarget[G]]): CPPInvocationTarget[G] = {
    decls.find(
      value => Util.compatTypes(args, getParamTypes(value))
    ).getOrElse(throw NotApplicable(applicable))
  }
}