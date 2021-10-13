package vct.col.resolve

import hre.util.FuncTools
import vct.col.ast.{ApplicableContract, Block, DiagnosticOrigin, Expr, JavaClass, JavaClassOrInterface, JavaConstructor, JavaImport, JavaMethod, JavaName, JavaNamespace, JavaStatic, JavaTClass, Origin, SourceNameOrigin, TClass, TNotAValue, TType, Variable}
import vct.result.VerificationResult
import vct.col.ast.Constant._

case object Java {
  case class JavaSystemOrigin(preferredName: String) extends Origin {
    override def messageInContext(message: String): String =
      s"At: [Class loaded from JRE with reflection]\n$message"
  }

  private implicit val o: Origin = DiagnosticOrigin
  val JAVA_LANG_OBJECT: JavaTClass = JavaTClass(Seq(("java", None), ("lang", None), ("Object", None)))

  def findLoadedJavaTypeName(potentialFQName: Seq[String], ctx: TypeResolutionContext): Option[JavaTypeNameTarget] = {
    ctx.stack.last.foreach {
      case RefJavaNamespace(JavaNamespace(pkg, _, decls)) =>
        for(decl <- decls) {
          Referrable.from(decl).foreach {
            case target: JavaTypeNameTarget =>
              if(pkg.map(_.names).getOrElse(Nil) :+ target.name == potentialFQName) {
                return Some(target)
              }
            case _ =>
          }
        }
      case _ =>
    }

    None
  }

  def findRuntimeJavaTypeName(potentialFQName: Seq[String], ctx: TypeResolutionContext): Option[JavaClassOrInterface] = {
    implicit val o: Origin = JavaSystemOrigin("unknown_jre")

    try {
      val classLoader = this.getClass.getClassLoader
      val cls = classLoader.loadClass(potentialFQName.mkString("."))

      val colClass = JavaClass(
        name = cls.getSimpleName,
        modifiers = Nil,
        typeParams = cls.getTypeParameters.map(param => new Variable(TType(JAVA_LANG_OBJECT))(JavaSystemOrigin(param.getName))),
        ext = JAVA_LANG_OBJECT,
        imp = Nil,
        decls = Seq(JavaConstructor(Nil, "", Nil, Nil, Nil, Block(Seq()), ApplicableContract(true, true, true, Nil, Nil, Nil))),
      )(JavaSystemOrigin(cls.getSimpleName))

      ctx.externallyLoadedClasses += JavaNamespace(Some(JavaName(potentialFQName.init)), Nil, Seq(colClass))
      Some(colClass)
    } catch {
      case _: ClassNotFoundException => None
    }
  }

  def findJavaTypeName(names: Seq[String], ctx: TypeResolutionContext): Option[JavaTypeNameTarget] = {
    val namespace = ctx.namespace.getOrElse(throw VerificationResult.Unreachable("A JavaClass declared outside a JavaNamespace is invalid."))
    val potentialFQNames: Seq[Seq[String]] = names match {
      case Seq(singleName) =>
        val inThisPackage = namespace.pkg match {
          case Some(pkg) => pkg.names :+ singleName
          case None => Seq(singleName)
        }
        val fromImport = namespace.imports.collect {
          case JavaImport(false, name, /*star = */ true) => name.names :+ singleName
          case JavaImport(false, name, /*star = */ false) if name.names.last == singleName => name.names
        }
        val fromPredef = Seq("java", "lang", singleName)
        fromImport :+ inThisPackage :+ fromPredef
      case moreNames => Seq(moreNames)
    }

    FuncTools.firstOption(potentialFQNames, findLoadedJavaTypeName(_, ctx))
      .orElse(FuncTools.firstOption(potentialFQNames, findRuntimeJavaTypeName(_, ctx)).map(RefJavaClass))
  }

  def findJavaName(name: String, ctx: ReferenceResolutionContext): Option[JavaNameTarget] =
    ctx.stack.flatten.collectFirst {
      case target: JavaNameTarget if target.name == name => target
    }

  def findDeref(obj: Expr, name: String, ctx: ReferenceResolutionContext): Option[JavaDerefTarget] =
    (obj.t match {
      case t @ TNotAValue() => t.decl.get match {
        case RefUnloadedJavaNamespace(pkg) =>
          Some(findJavaTypeName(pkg :+ name, ctx.asTypeResolutionContext)
            .getOrElse(RefUnloadedJavaNamespace(pkg :+ name)))
        case RefJavaClass(decl) =>
          decl.decls.flatMap(Referrable.from).collectFirst {
            case ref @ RefJavaField(decls, idx) if ref.name == name && ref.decls.modifiers.contains(JavaStatic()) => ref
          }
        case _ => None
      }
      case t @ JavaTClass(_) => t.ref.get match {
        case RefAxiomaticDataType(decl) => None
        case RefModel(decl) => decl.declarations.flatMap(Referrable.from).collectFirst {
          case ref @ RefModelField(_) if ref.name == name => ref
        }
        case RefJavaClass(decl) => decl.decls.flatMap(Referrable.from).collectFirst {
          case ref @ RefJavaField(_, _) if ref.name == name && !ref.decls.modifiers.contains(JavaStatic()) => ref
        }
      }
      case _ => None
    }).orElse(Spec.builtinField(obj, name))

  def findMethodInClass(cls: JavaClassOrInterface, method: String, args: Seq[Expr]): Option[JavaInvocationTarget] =
    cls.decls.flatMap(Referrable.from).collectFirst {
      case ref: RefJavaMethod if ref.name == method && Util.compat(args, ref.decl.parameters) => ref
      case ref: RefInstanceFunction if ref.name == method && Util.compat(args, ref.decl.args) => ref
      case ref: RefInstanceMethod if ref.name == method && Util.compat(args, ref.decl.args) => ref
      case ref: RefInstancePredicate if ref.name == method && Util.compat(args, ref.decl.args) => ref
    }

  def findMethod(obj: Expr, method: String, args: Seq[Expr]): Option[JavaInvocationTarget] =
    (obj.t match {
      case t @ JavaTClass(_) =>
        t.ref.get match {
          case RefAxiomaticDataType(decl) => decl.decls.flatMap(Referrable.from).collectFirst {
            case ref: RefADTFunction if ref.name == method => ref
          }
          case RefModel(decl) => decl.declarations.flatMap(Referrable.from).collectFirst {
            case ref: RefModelAction if ref.name == method => ref
            case ref: RefModelProcess if ref.name == method => ref
          }
          case RefJavaClass(decl) => findMethodInClass(decl, method, args)
        }
      case _ => throw NotApplicable(obj) // FIXME
    }).orElse(Spec.builtinInstanceMethod(obj, method))

  def findMethod(ctx: ReferenceResolutionContext, method: String, args: Seq[Expr]): Option[JavaInvocationTarget] =
    ctx.stack.flatten.collectFirst {
      case ref: RefJavaMethod if ref.name == method && Util.compat(args, ref.decl.parameters) => ref
      case ref: RefInstanceFunction if ref.name == method && Util.compat(args, ref.decl.args) => ref
      case ref: RefInstanceMethod if ref.name == method && Util.compat(args, ref.decl.args) => ref
      case ref: RefInstancePredicate if ref.name == method && Util.compat(args, ref.decl.args) => ref
      case ref: RefFunction if ref.name == method && Util.compat(args, ref.decl.args) => ref
      case ref: RefProcedure if ref.name == method && Util.compat(args, ref.decl.args) => ref
      case ref: RefPredicate if ref.name == method && Util.compat(args, ref.decl.args) => ref
      case ref: RefADTFunction if ref.name == method && Util.compat(args, ref.decl.args) => ref
      case ref: RefModelProcess if ref.name == method && Util.compat(args, ref.decl.args) => ref
      case ref: RefModelAction if ref.name == method && Util.compat(args, ref.decl.args) => ref
    }
}
