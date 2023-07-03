package vct.col.resolve.lang

import hre.util.FuncTools
import vct.col.ast._
import vct.col.origin._
import vct.col.resolve._
import vct.col.resolve.ctx._
import vct.col.typerules.Types
import vct.result.VerificationError.UserError

case object CPP {
  implicit private val o: Origin = DiagnosticOrigin

  case class CPPTypeNotSupported(node: Option[Node[_]]) extends UserError {
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

  def getDeclaratorInfo[G](decl: CPPDeclarator[G]): DeclaratorInfo[G] = decl match {
    case CPPPointerDeclarator(pointers, inner) =>
      val innerInfo = getDeclaratorInfo(inner)
      DeclaratorInfo(
        innerInfo.params,
        t => innerInfo.typeOrReturnType(FuncTools.repeat[Type[G]](TPointer(_), pointers.size, t)),
        innerInfo.name)
    case array @ CPPArrayDeclarator(inner, size) =>
      val innerInfo = getDeclaratorInfo(inner)
      DeclaratorInfo(innerInfo.params, t => innerInfo.typeOrReturnType(CPPTArray(size, t)(array.blame)), innerInfo.name)
    case CPPTypedFunctionDeclarator(params, _, inner) =>
      val innerInfo = getDeclaratorInfo(inner)
      DeclaratorInfo(params = Some(params), typeOrReturnType = (t => t), innerInfo.name)
    case CPPName(name) => DeclaratorInfo(params = None, typeOrReturnType = (t => t), name)
  }

  def getPrimitiveType[G](specs: Seq[CPPDeclarationSpecifier[G]], context: Option[Node[G]] = None): Type[G] =
    specs.collect { case spec: CPPTypeSpecifier[G] => spec } match {
      case Seq(CPPVoid()) => TVoid()
      case Seq(CPPChar()) => TChar()
      case t if CPP.NUMBER_LIKE_SPECIFIERS.contains(t) => TInt()
      case Seq(CPPSpecificationType(t@TFloat(_, _))) => t
      case Seq(CPPBool()) => TBool()
      case Seq(defn@CPPTypedefName(_)) => Types.notAValue(defn.ref.get)
      case Seq(CPPSpecificationType(typ)) => typ
      case spec +: _ => throw CPPTypeNotSupported(context.orElse(Some(spec)))
      case _ => throw CPPTypeNotSupported(context)
    }

  def nameFromDeclarator(declarator: CPPDeclarator[_]): String =
    getDeclaratorInfo(declarator).name

  def typeOrReturnTypeFromDeclaration[G](specs: Seq[CPPDeclarationSpecifier[G]], decl: CPPDeclarator[G]): Type[G] =
    getDeclaratorInfo(decl).typeOrReturnType(CPPPrimitiveType(specs))

  def paramsFromDeclarator[G](declarator: CPPDeclarator[G]): Seq[CPPParam[G]] =
    getDeclaratorInfo(declarator).params.get

  def findCPPTypeName[G](names: Seq[String], ctx: TypeResolutionContext[G]): Option[CPPTypeNameTarget[G]] =
    ctx.stack.flatten.collectFirst {
      case target: CPPTypeNameTarget[G] if target.name == names.mkString("::") => target
    }

  def findCPPName[G](name: Seq[String], ctx: ReferenceResolutionContext[G]): Option[CPPNameTarget[G]] = {
    if (name.length == 1) {
      ctx.stack.flatten.collectFirst {
        case target: CPPNameTarget[G] if target.name == name.head => target
      }
    } else {
      val ctxTarget: Option[RefCPPNamespaceDefinition[G]] = ctx.stack.flatten.collectFirst {
        case namespace: RefCPPNamespaceDefinition[G] if namespace.decl.name == name.head => namespace
      }

      ctxTarget match {
        case Some(ref) =>
          var curNameSeq = name.drop(1);
          var foundNamespace: Option[CPPNamespaceDefinition[G]] = Some(ref.decl);
          var returnVal: Option[CPPNameTarget[G]] = None;
          while (curNameSeq.nonEmpty) {
            if (foundNamespace.isEmpty) {
              return None
            }

            if (curNameSeq.length > 1) {
              // Look for nested namespaces
              foundNamespace = foundNamespace.get.declarations.collectFirst {
                case namespace: CPPNamespaceDefinition[G] if namespace.name == curNameSeq.head => namespace
              }
            } else {
              // Look for final nameTarget
              returnVal = foundNamespace.get.declarations.collectFirst {
                case funcDef: CPPFunctionDefinition[G] if getDeclaratorInfo(funcDef.declarator).name == curNameSeq.head  => RefCPPFunctionDefinition(funcDef)
                case globalDecl: CPPGlobalDeclaration[G] if getDeclaratorInfo(globalDecl.decl.inits.head.decl).name == curNameSeq.head => RefCPPGlobalDeclaration(globalDecl, 0)
              }
            }
            curNameSeq = curNameSeq.drop(1)
          }
          returnVal
        case None => None
      }
    }
  }

  def findForwardDeclaration[G](declarator: CPPDeclarator[G], ctx: ReferenceResolutionContext[G]): Option[RefCPPGlobalDeclaration[G]] =
    ctx.stack.flatten.collectFirst {
      case target: RefCPPGlobalDeclaration[G] if target.name == nameFromDeclarator(declarator) => target
    }

  def findDefinition[G](declarator: CPPDeclarator[G], ctx: ReferenceResolutionContext[G]): Option[RefCPPFunctionDefinition[G]] =
    ctx.stack.flatten.collectFirst {
      case target: RefCPPFunctionDefinition[G] if target.name == nameFromDeclarator(declarator) => target
    }

  def resolveInvocation[G](obj: Expr[G]): CPPInvocationTarget[G] =
    obj.t match {
      case t: TNotAValue[G] => t.decl.get match {
        case target: CPPInvocationTarget[G] => target
        case _ => throw NotApplicable(obj)
      }
      case _ => throw NotApplicable(obj)
    }
}
