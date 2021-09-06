package vct.col.ast

import hre.util.FuncTools
import vct.col.ast
import vct.result.VerificationResult
import vct.result.VerificationResult.ExcludedByCompilationError

import scala.annotation.tailrec
import scala.reflect.ClassTag

case class DynamicFunctionsUnsupported(node: Node) extends VerificationResult.UserError {
  override def text: String = node.o.messageInContext(
    "Functions as values are not supported: we can only resolve invocations of a single name in C.")
}

case class ResultOutsideMethod(res: AmbiguousResult) extends VerificationResult.UserError {
  override def text: String = res.o.messageInContext(
    "The special '\\result' expression is only allowed in the postcondition of a method.")
}

case class NameLost(decl: Declaration) extends VerificationResult.SystemError {
  override def text: String =
    decl.o.messageInContext("The name of this declaration was queried, but it's origin is no longer a SourceNameOrigin.")
}

case class NoSuchName(name: String, reference: Node) extends VerificationResult.UserError {
  override def text: String =
    reference.o.messageInContext(s"No declaration named $name was found.")
}

case object Referrable {
  def find[T](ctx: Seq[Seq[Referrable]], name: String, node: Node)(f: PartialFunction[Referrable, T]): T =
    ctx.flatten.collectFirst(f) match {
      case Some(value) => value
      case None => throw NoSuchName(name, node)
    }

  def findDecl[T <: Declaration](ctx: Seq[Seq[Referrable]], name: String, node: Node)(implicit tag: ClassTag[T]): T =
    find(ctx, name, node) {
      case ref @ RefDeclaration(x: T) if ref.name == name => x
    }

  def findLocal(ctx: Seq[Seq[Referrable]], name: String, node: Node): Variable = findDecl(ctx, name, node)
  def findApplicable(ctx: Seq[Seq[Referrable]], name: String, node: Node): Applicable = findDecl(ctx, name, node)
  def findClass(ctx: Seq[Seq[Referrable]], name: String, node: Node): Class = findDecl(ctx, name, node)
  def findLabel(ctx: Seq[Seq[Referrable]], name: String, node: Node): LabelDecl = findDecl(ctx, name, node)
  def findParInvariant(ctx: Seq[Seq[Referrable]], name: String, node: Node): ParInvariantDecl = findDecl(ctx, name, node)
  def findParBlock(ctx: Seq[Seq[Referrable]], name: String, node: Node): ParBlockDecl = findDecl(ctx, name, node)
  def findModel(ctx: Seq[Seq[Referrable]], name: String, node: Node): Model = findDecl(ctx, name, node)

  def findCRef(ctx: Seq[Seq[Referrable]], name: String, node: Node): CRef =
    find(ctx, name, node) {
      case ref: CRef if ref.name == name => ref
    }

  def findJavaRef(ctx: Seq[Seq[Referrable]], name: String, node: Node): JavaRef =
    find(ctx, name, node) {
      case ref: JavaRef if ref.name == name => ref
    }

  def findModelField(ctx: Seq[Seq[Referrable]], name: String, node: Node): ModelField =
    find(ctx, name, node) {
      case ref @ RefDeclaration(field: ModelField) if ref.name == name => field
    }
}

/**
  * This type is separate from Ref, which is designed to automatically track references across the AST
  * Referrable also encompasses anything that is referred to, but not in an automatic fashion: mostly via names.
  */
sealed trait Referrable {
  /**
    * Default type associated with the referrable in usual usage.
    * For variables and fields, this is simply its type. For applicables, it's the return type.
    */
  def t: Type
}

case object CRef {
  @tailrec
  def extractName(decl: CDeclarator): String = decl match {
    case CPointerDeclarator(_, inner) => extractName(inner)
    case CArrayDeclarator(_, _, inner) => extractName(inner)
    case CTypedFunctionDeclarator(_, _, inner) => extractName(inner)
    case CAnonymousFunctionDeclarator(_, inner) => extractName(inner)
    case CName(name) => name
  }
}

sealed trait CRef extends Referrable {
  def name: String
}

sealed trait JavaRef extends Referrable {
  def name: String
}

case class RefDeclaration(decl: Declaration) extends CRef with JavaRef {
  override def t: Type = decl match {
    case app: Applicable => app.returnType
    case f: Field => f.t
    case f: ModelField => f.t
    case v: Variable => v.t
  }

  override def name: String = decl match {
    case JavaClass(name, _, _, _, _, _) => name
    case JavaInterface(name, _, _, _, _) => name
    case JavaConstructor(_, name, _, _, _, _, _) => name
    case JavaMethod(_, _, _, name, _, _, _, _, _) => name
    case _: JavaNamespace => "+"

    case other => other.o match {
      case SourceNameOrigin(name, _) => name
      case _ => throw NameLost(decl)
    }
  }

  def expand: Seq[Referrable] = decl match {
    case defn: CFunctionDefinition =>
      Seq(CRefFunctionDefinition(defn))
    case CGlobalDeclaration(decl) =>
      CRefDeclaration.allOfDeclaration(decl)

    case fields @ JavaFields(_, _, decls) =>
      decls.indices.map(JavaRefField(fields, _))

    case _ => Seq(this)
  }
}

case class CRefFunctionDefinition(defn: CFunctionDefinition) extends CRef {
  override def t: Type = CPrimitiveType.ofDeclarator(defn.specs, defn.declarator)
  override def name: String = CRef.extractName(defn.declarator)
}
object CRefDeclaration {
  def allOfDeclaration(decl: CDeclaration): Seq[CRefDeclaration] =
    decl.inits.indices.map(CRefDeclaration(decl, _))
}
case class CRefDeclaration(decl: CDeclaration, initIndex: Int) extends CRef {
  override def t: Type = CPrimitiveType.ofDeclarator(decl.specs, decl.inits(initIndex).decl)
  override def name: String = CRef.extractName(decl.inits(initIndex).decl)
}

case class JavaRefField(fields: JavaFields, declIndex: Int) extends JavaRef with JavaDerefRef {
  override def t: Type = FuncTools.repeat(TArray(_), fields.decls(declIndex)._2, fields.t)
  override def name: String = fields.decls(declIndex)._1
}
case class JavaRefLocal(locals: JavaLocalDeclaration, declIndex: Int) extends JavaRef {
  override def t: Type = FuncTools.repeat(TArray(_), locals.decls(declIndex)._2, locals.t)
  override def name: String = locals.decls(declIndex)._1
}

case class CRefLabeledStatement(stat: CLabeledStatement) extends Referrable {
  override def t: Type = throw VerificationResult.Unreachable(
    "A labeled statement does not have a type. Perhaps we are missing a check for the kind of CRef contained in some node?")
}

sealed trait JavaDerefRef
case class JavaDerefRefNamespace(names: Seq[String]) extends JavaDerefRef
case class JavaDerefRefClass(cls: JavaClass) extends JavaDerefRef

object ResolveTypes {
  def getMatchingImports(ns: Option[JavaNamespace], names: Seq[String]): Seq[JavaImport] = {
    implicit val o: Origin = DiagnosticOrigin

    (ns match {
      case Some(ns) =>
        ns.imports.filter(!_.isStatic).filter(imp =>
          (imp.star && imp.name.names == names.init) ||
          (!imp.star && imp.name.names == names)
        ) :+ JavaImport(isStatic=false, ns.pkg.getOrElse(JavaName(Nil)), star=true)
      case None => Nil
    }) :+ JavaImport(isStatic=false, JavaName(Seq("java", "lang")), star=true)
  }

  def findJavaClassInContext(packages: Seq[Seq[String]], ctx: Seq[Seq[Referrable]], simpleName: String): Option[JavaClass] = {
    for(importPkg <- packages) {
      for(frame <- ctx) {
        for(ref <- frame) {
          ref match {
            case RefDeclaration(JavaNamespace(Some(pkg), _, decls)) if pkg.names == importPkg =>
              for(decl <- decls) {
                decl match {
                  case cls @ JavaClass(name, _, _, _, _, _) if name == simpleName =>
                    return Some(cls)
                }
              }
            case _ =>
          }
        }
      }
    }

    None
  }

  def findJavaClassByImport(ns: Option[JavaNamespace], names: Seq[String], ctx: Seq[Seq[Referrable]]): Option[JavaClass] = {
    val imports = getMatchingImports(ns, names)
    val packages = imports.map(imp =>
      if(imp.star) imp.name.names
      else imp.name.names.init)
    val simpleName = names.last

    findJavaClassInContext(packages, ctx, names.last)
    // TODO this is the place to load from library files and/or JVM runtime
  }

  def findJavaClass(ctx: Seq[Seq[Referrable]], ns: Option[JavaNamespace], name: String): Option[JavaClass] = {
    for(frame <- ctx) {
      for(ref <- frame) {
        ref match {
          case RefDeclaration(cls @ JavaClass(className, _, _, _, _, _))
            if className == name => return Some(cls)
        }
      }
    }

    findJavaClassByImport(ns, Seq(name), ctx)
  }

  def findFqJavaClass(ctx: Seq[Seq[Referrable]], ns: Option[JavaNamespace], names: Seq[String]): Option[JavaClass] = {
    findJavaClassByImport(ns, names, ctx)
  }

  def findCRef(name: String, ctx: Seq[Seq[Referrable]]): Option[CRef] = ctx.flatten.collectFirst {
    case ref: CRef if ref.name == name => ref
  }

  def resolve(node: Node, ctx: Seq[Seq[Referrable]], ns: Option[JavaNamespace]): Unit = node match {
    case cls @ JavaTClass(genericNames) =>
      val names = genericNames.map(_._1)
      if(names.size == 1) {
        cls.ref = Some(findJavaClass(ctx, ns, names.head).getOrElse(throw NoSuchName(names.head, cls)))
      } else {
        cls.ref = Some(findFqJavaClass(ctx, ns, names).getOrElse(throw NoSuchName(names.mkString("."), cls)))
      }
    case ref @ CTypedefName(name) =>
      ref.ref = Some(findCRef(name, ctx).getOrElse(throw NoSuchName(name, ref)))
    case newNs: JavaNamespace =>
      newNs.subnodes.foreach(resolve(_, ctx, Some(newNs)))
    case other => other.subnodes.foreach(resolve(_, ctx, ns))
  }
}

object ResolveReferences {
  def resolveAndCheck(node: Node,
                      ctx: Seq[Seq[Referrable]],
                      ns: Option[JavaNamespace],
                      returnType: Option[Type],
                      checkCtx: CheckContext): Seq[CheckError] = {
    val innerNs = node match {
      case newNs: JavaNamespace => Some(newNs)
      case _ => ns
    }
    val innerReturnType = node match {
      case app: ContractApplicable => Some(app.returnType)
      case JavaMethod(_, returnType, _, _, _, _, _, _, _) => Some(returnType)
      case _ => returnType
    }

    val innerCtx = enterContext(node) +: ctx
    val innerCheckCtx = node.enterCheckContext(checkCtx)
    val childErrors = node.subnodes.flatMap(resolveAndCheck(_, innerCtx, innerNs, innerReturnType, innerCheckCtx))

    if(childErrors.isEmpty) {
      resolveFlatly(node, ctx, ns, returnType)
      node.check(checkCtx)
    } else {
      childErrors
    }
  }

  def enterContext(node: Node): Seq[Referrable] = node match {
    case Scope(locals, body) =>
      def scanScope(node: Node): Seq[Referrable] = node match {
        case _: Scope => Nil
        case CDeclarationStatement(cDecl) =>
          CRefDeclaration.allOfDeclaration(cDecl)
        case decl @ JavaLocalDeclaration(_, _, decls) =>
          decls.indices.map(JavaRefLocal(decl, _))
        case LocalDecl(v) => Seq(RefDeclaration(v))
        case other =>
          other.subnodes.flatMap(scanScope)
      }
      locals.map(RefDeclaration) ++ scanScope(body)

    case app: Applicable =>
      app.body.map(_.transSubnodes.collect {
        case stat: CLabeledStatement => CRefLabeledStatement(stat)
        case label: Label => RefDeclaration(label.decl)
      }).getOrElse(Nil) ++ app.declarations.map(RefDeclaration).flatMap(_.expand)

    case declarator: Declarator => declarator.declarations.map(RefDeclaration).flatMap(_.expand)

    case _ => Nil
  }

  def resolveFlatly(node: Node, ctx: Seq[Seq[Referrable]], ns: Option[JavaNamespace], returnType: Option[Type]): Unit = node match {
    case local @ CLocal(name) =>
      local.ref = Some(Referrable.findCRef(ctx, name, local))
    case inv @ CInvocation(app, _) =>
      app match {
        case CLocal(name) => inv.ref = Some(Referrable.findCRef(ctx, name, inv))
        case other => throw DynamicFunctionsUnsupported(other)
      }
    case inv @ GpgpuCudaKernelInvocation(name, _, _, _) =>
      inv.ref = Some(Referrable.findCRef(ctx, name, inv))
    case goto @ CGoto(name) =>
      goto.ref = Some(ctx.flatten.collectFirst {
        case CRefLabeledStatement(stat @ CLabeledStatement(label, _)) if label == name => stat
      }.getOrElse(throw NoSuchName(name, goto)))

    case local @ JavaLocal(name) =>
      local.ref = Some(Referrable.findJavaRef(ctx, name, local))
    case deref @ JavaDeref(obj, field) =>
      // TODO
      obj.t match {
        case JavaTNamespace(names) =>
          deref.ref = Some((if(names.size == 1) {
            ResolveTypes.findJavaClass(ctx, ns, names.head)
          } else {
            ResolveTypes.findFqJavaClass(ctx, ns, names)
          }).map(JavaDerefRefClass).getOrElse(JavaDerefRefNamespace(names :+ field)))
        case JavaTClassValue(ref) =>
          deref.ref = Some(ref.decl.decls.collect {
            case fields: JavaFields => fields.decls.indices.map((fields, _))
          }.flatten.collectFirst {
            case (fields, idx) if fields.decls(idx)._1 == field =>
              if(!fields.modifiers.contains(JavaStatic()(DiagnosticOrigin))) {
                throw ExcludedByCompilationError(
                  deref.o.messageInContext(s"Field $field of class ${ref.decl.name} is not static"))
              }
              JavaRefField(fields, idx)
          }.getOrElse(throw NoSuchName(field, deref)))
        case cls @ JavaTClass(_) =>
          deref.ref = Some(cls.ref.getOrElse(throw VerificationResult.Unreachable("excluded by ResolveTypes")).decls.collect {
            case fields: JavaFields => fields.decls.indices.map((fields, _))
          }.flatten.collectFirst {
            case (fields, idx) if fields.decls(idx)._1 == field =>
              if(fields.modifiers.contains(JavaStatic()(DiagnosticOrigin))) {
                throw ExcludedByCompilationError(
                  deref.o.messageInContext(s"Field $field of class ${cls.names.map(_._1).mkString(".")} is not static"))
              }
              JavaRefField(fields, idx)
          }.getOrElse(throw NoSuchName(field, deref)))
      }

    case process: ModelProcess =>
      (process.modifies ++ process.accessible).foreach(_.tryResolve(Referrable.findModelField(ctx, _, process)))
    case action: ModelAction =>
      (action.modifies ++ action.accessible).foreach(_.tryResolve(Referrable.findModelField(ctx, _, action)))

    case local @ Local(ref) =>
      ref.tryResolve(Referrable.findLocal(ctx, _, local))
    case deref @ Deref(obj, ref) =>
      obj.t match {
        case TClass(cls) => ref.tryResolve(name => cls.decl.declarations.collectFirst {
          case field: InstanceField if RefDeclaration(field).name == name => field
        } match {
          case Some(field) => field
          case None => throw NoSuchName(name, deref)
        })
      }

    case inv @ ProcedureInvocation(ref, _, _) => ref.tryResolve(Referrable.findApplicable(ctx, _, inv))
    case inv @ MethodInvocation(_, ref, _, _) => ref.tryResolve(Referrable.findApplicable(ctx, _, inv))
    case inv @ FunctionInvocation(ref, _) => ref.tryResolve(Referrable.findApplicable(ctx, _, inv))
    case inv @ InstanceFunctionInvocation(_, ref, _) => ref.tryResolve(Referrable.findApplicable(ctx, _, inv))
    case inv @ ActionApply(ref, _) => ref.tryResolve(Referrable.findApplicable(ctx, _, inv))
    case inv @ ProcessApply(ref, _) => ref.tryResolve(Referrable.findApplicable(ctx, _, inv))

    case e @ NewObject(ref) => ref.tryResolve(Referrable.findClass(ctx, _, e))
    case old @ Old(_, Some(ref)) => ref.tryResolve(Referrable.findLabel(ctx, _, old))

    case atomic @ ParAtomic(ref, _) => ref.tryResolve(Referrable.findParInvariant(ctx, _, atomic))
    case barrier @ ParBarrier(ref, _, _, _, _) => ref.tryResolve(Referrable.findParBlock(ctx, _, barrier))
    case block @ ParBlock(_, after, _, _, _, _) => after.foreach(_.tryResolve(Referrable.findParBlock(ctx, _, block)))
    case goto @ Goto(ref) => ref.tryResolve(Referrable.findLabel(ctx, _, goto))
    case create @ ModelCreate(_, ref, _) => ref.tryResolve(Referrable.findModel(ctx, _, create))
    case break @ Break(Some(ref)) => ref.tryResolve(Referrable.findLabel(ctx, _, break))
    case continue @ Continue(Some(ref)) => ref.tryResolve(Referrable.findLabel(ctx, _, continue))

    case res @ AmbiguousResult() =>
      res.ref = Some(returnType.getOrElse(throw ResultOutsideMethod(res)))

    case _ =>
  }
}