package vct.col.resolve

import com.typesafe.scalalogging.LazyLogging
import hre.data.BitString
import hre.util.FuncTools
import vct.col.ast._
import vct.col.ast.util.Declarator
import vct.col.check.CheckError
import vct.col.origin._
import vct.col.resolve.ResolveReferences.scanScope
import vct.col.ref.Ref
import vct.col.resolve.ctx._
import vct.col.resolve.lang.{C, CPP, Java, LLVM, PVL, Spec}
import vct.col.resolve.Resolve.{MalformedBipAnnotation, SpecContractParser, SpecExprParser, getLit, isBip}
import vct.col.resolve.lang.JavaAnnotationData.{BipComponent, BipData, BipGuard, BipInvariant, BipPort, BipPure, BipStatePredicate, BipTransition}
import vct.col.rewrite.InitialGeneration
import vct.result.VerificationError.{Unreachable, UserError}

import scala.collection.immutable.{AbstractSeq, LinearSeq}

case object Resolve {
  trait SpecExprParser {
    // If parsing fails, throw/terminate
    def parse[G](input: String, o: Origin): Expr[G]
  }

  trait SpecContractParser {
    def parse[G](input: LlvmFunctionContract[G], o: Origin): ApplicableContract[G]

    def parse[G](input: LlvmGlobal[G], o: Origin): GlobalDeclaration[G]
  }

  def extractLiteral(e: Expr[_]): Option[String] = e match {
    case JavaStringValue(guardName, _) =>
      Some(guardName)
    case local @ JavaLocal(_) =>
      local.ref match {
        case Some(RefJavaField(decls, id)) =>
          decls.decls(id).init match {
            case Some(JavaStringValue(data, _)) => Some(data)
            case _ => None
          }
        case _ => None
      }
    case _ => None
  }

  case class MalformedBipAnnotation(n: Node[_], err: String) extends UserError {
    override def code: String = "badBipAnnotation"
    override def text: String = n.o.messageInContext(s"Malformed JavaBIP annotation: $err")
  }

  case class UnexpectedComplicatedExpression(e: Expr[_]) extends UserError {
    override def code: String = "unexpectedComplicatedExpression"
    override def text: String = e.o.messageInContext("This expression must either be a string literal or trivially resolve to one")
  }

  def getLit(e: Expr[_]): String =
    extractLiteral(e).getOrElse(throw UnexpectedComplicatedExpression(e))

  def isBip(node: Node[_], `type`: String): Boolean = node match {
    case JavaAnnotation(JavaTClass(r, _), _) => isBip(r.decl, `type`)
    case c: JavaClassOrInterface[_] => c.modifiers.contains(JavaBipAnnotation[InitialGeneration]()(DiagnosticOrigin)) && c.name == `type`
    case _ => false
  }


}

case object ResolveTypes {
  sealed trait JavaClassPathEntry
  case object JavaClassPathEntry {
    case object SourcePackageRoot extends JavaClassPathEntry
    case class Path(root: java.nio.file.Path) extends JavaClassPathEntry
  }

  def resolve[G](program: Program[G], externalJavaLoader: Option[ExternalJavaLoader] = None, javaClassPath: Seq[JavaClassPathEntry]): Seq[GlobalDeclaration[G]] = {
    val ctx = TypeResolutionContext[G](externalJavaLoader = externalJavaLoader, javaClassPath = javaClassPath)
    resolve(program, ctx)
    ctx.externallyLoadedElements.toSeq
  }

  def resolve[G](node: Node[G], ctx: TypeResolutionContext[G]): Unit = {
    val innerContext = enterContext(node, ctx)
    node.subnodes.foreach(resolve(_, innerContext))
    resolveOne(node, ctx)
  }

  def scanImport[G](imp: JavaImport[G], ctx: TypeResolutionContext[G]): Seq[Referrable[G]] /* importable? */ = imp match {
    case imp @ JavaImport(/* static = */ true, JavaName(fullyQualifiedTypeName :+ staticMember), /* star = */ false) =>
      val staticType = Java.findJavaTypeName(fullyQualifiedTypeName, ctx)
        .getOrElse(throw NoSuchNameError("class", fullyQualifiedTypeName.mkString("."), imp))
      Seq(Java.findStaticMember(staticType, staticMember)
        .getOrElse(throw NoSuchNameError("static member", (fullyQualifiedTypeName :+ staticMember).mkString("."), imp)))
    case imp @ JavaImport(/* static = */ true, JavaName(fullyQualifiedTypeName), /* star = */ true) =>
      val typeName = Java.findJavaTypeName(fullyQualifiedTypeName, ctx)
        .getOrElse(throw NoSuchNameError("class", fullyQualifiedTypeName.mkString("."), imp))
      Java.getStaticMembers(typeName)
    // Non-static imports are resolved on demand
    case _ => Seq()
  }

  def enterContext[G](node: Node[G], ctx: TypeResolutionContext[G]): TypeResolutionContext[G] = node match {
    case Program(decls) =>
      ctx.copy(stack=decls.flatMap(Referrable.from) +: ctx.stack)
    case ns: JavaNamespace[G] =>
      // Static imports need to be imported at this stage, because they influence how names are resolved.
      // E.g.: in the expression f.g, f is either a 1) variable, 2) parameter or 3) field. If none of those, it must be a
      // 4) statically imported field or typename, or 5) a non-static imported typename. If it's not that, it's a package name.
      // ctx.stack needs to be modified for this, and hence this importing is done in enterContext instead of in resolveOne.
      val ctxWithNs = ctx.copy(namespace=Some(ns))
      ctxWithNs.copy(stack=(ns.declarations.flatMap(Referrable.from) ++ ns.imports.flatMap(scanImport(_, ctxWithNs))) +: ctx.stack)
    case Scope(locals, body) => ctx
      .copy(stack = ((locals ++ scanScope(body, /* inGPUkernel = */false)).flatMap(Referrable.from)) +: ctx.stack)
    case decl: Declarator[G] =>
      ctx.copy(stack=decl.declarations.flatMap(Referrable.from) +: ctx.stack)
    case _ => ctx
  }

  def resolveOne[G](node: Node[G], ctx: TypeResolutionContext[G]): Unit = node match {
    case javaClass @ JavaNamedType(genericNames) =>
      val names = genericNames.map(_._1)
      javaClass.ref = Some(Java.findJavaTypeName(names, ctx)
        .getOrElse(throw NoSuchNameError("class", names.mkString("."), javaClass)))
    case t @ JavaTClass(ref, _) =>
      ref.tryResolve(name => ???)
    case t @ CTypedefName(name) =>
      t.ref = Some(C.findCTypeName(name, ctx).getOrElse(
        throw NoSuchNameError("struct", name, t)
      ))
    case t@CStructSpecifier(name) =>
      t.ref = Some(C.findCStruct(name, ctx).getOrElse(
        throw NoSuchNameError("struct", name, t)
      ))
    case t@CPPTypedefName(nestedName, _) =>
      t.ref = Some(CPP.findCPPTypeName(nestedName, ctx).getOrElse(
        throw NoSuchNameError("class, struct, or namespace", nestedName, t)
      ))
    case t @ PVLNamedType(name, typeArgs) =>
      t.ref = Some(PVL.findTypeName(name, ctx).getOrElse(
        throw NoSuchNameError("class", name, t)))
    case t @ TModel(ref) =>
      ref.tryResolve(name => Spec.findModel(name, ctx).getOrElse(throw NoSuchNameError("model", name, t)))
    case t @ TClass(ref, _) =>
      ref.tryResolve(name => Spec.findClass(name, ctx).getOrElse(throw NoSuchNameError("class", name, t)))
    case t @ TAxiomatic(ref, _) =>
      ref.tryResolve(name => Spec.findAdt(name, ctx).getOrElse(throw NoSuchNameError("adt", name, t)))
    case t @ SilverPartialTAxiomatic(ref, partialTypeArgs) =>
      ref.tryResolve(name => Spec.findAdt(name, ctx).getOrElse(throw NoSuchNameError("adt", name, t)))
      partialTypeArgs.foreach(mapping => mapping._1.tryResolve(name => Spec.findAdtTypeArg(ref.decl, name).getOrElse(throw NoSuchNameError("type variable", name, t))))
    case local: JavaLocal[G] =>
      Java.findJavaName(local.name, fromStaticContext = false, ctx) match {
        case Some(
        _: RefVariable[G] | _: RefJavaField[G] | _: RefJavaLocalDeclaration[G] | // Regular names
        _ // Statically imported, or regular previously imported typename
        ) => // Nothing to do. Local will get properly resolved next phase
        case None =>
          // Unknown what this local refers though. Try importing it as a type; otherwise, it's the start of a package
          Java.findJavaTypeName(Seq(local.name), ctx) match {
            case Some(_) => // already imported so nothing to do
            case None =>
              local.ref = Some(RefUnloadedJavaNamespace(Seq(local.name)))
          }
      }
    case deref: JavaDeref[G] =>
      val ref = deref.obj match {
        case javalocal : JavaLocal[G] =>  javalocal.ref
        case javaderef : JavaDeref[G] => javaderef.ref
        case _ => None
      }
      ref match {
        case Some(RefUnloadedJavaNamespace(names)) =>
          Java.findJavaTypeName(names :+ deref.field, ctx) match {
            case Some(_) => // already imported so nothing to do
            case None =>
              deref.ref = Some(RefUnloadedJavaNamespace(names :+ deref.field))
          }
        case _ => // we did not find a package so we don't do anything
      }
    case endpoint: PVLEndpoint[G] =>
      endpoint.cls.tryResolve(name => PVL.findTypeName(name, ctx) match {
        case Some(RefClass(cls: Class[G])) => cls
        case Some(_) => throw ForbiddenEndpointType(endpoint)
        case None => throw NoSuchNameError("class", name, endpoint)
      })

    case _ =>
  }
}

case object ResolveReferences extends LazyLogging {
  def resolve[G](program: Program[G], jp: SpecExprParser, lsp: SpecContractParser): Seq[CheckError] = {
    resolve(program, ReferenceResolutionContext[G](jp, lsp))
  }

  def resolve[G](node: Node[G], ctx: ReferenceResolutionContext[G], inGPUKernel: Boolean=false): Seq[CheckError] = {
    val inGPU = inGPUKernel || (node match {
      case f: CFunctionDefinition[G] => f.specs.collectFirst{case _: CGpgpuKernelSpecifier[G] => ()}.isDefined
      case _ => false
    })

    val childErrors = node match {
      case l @ Let(binding, value, main) =>
        val innerCtx = enterContext(node, ctx, inGPU).withCheckContext(l.enterCheckContext(ctx.checkContext))
        resolve(binding, innerCtx) ++
        resolve(value, ctx) ++
        resolve(main, innerCtx)
      case _ =>
        val innerCtx = enterContext(node, ctx, inGPU)
        node.checkContextRecursor(ctx.checkContext, { (ctx, node) =>
          resolve(node, innerCtx.withCheckContext(ctx), inGPU)
        }).flatten
    }

    if(childErrors.nonEmpty) childErrors
    else {
      resolveFlatly(node, ctx)
      node.check(ctx.checkContext)
    }
  }

  def scanScope[G](node: Node[G], inGPUKernel: Boolean): Seq[Declaration[G]] = node match {
    case _: Scope[G] => Nil
    // Remove shared memory locations from the body level of a GPU kernel, we want to reason about them at the top level
    case CDeclarationStatement(decl) if !(inGPUKernel && decl.decl.specs.collectFirst{case GPULocal() => ()}.isDefined)
    => Seq(decl)
    case CPPDeclarationStatement(decl) => Seq(decl)
    case JavaLocalDeclarationStatement(decl) => Seq(decl)
    case LocalDecl(v) => Seq(v)
    case other => other.subnodes.flatMap(scanScope(_, inGPUKernel))
  }

  def scanLabels[G](node: Node[G]): Seq[Declaration[G]] = node.transSubnodes.collect {
    case decl: LabelDecl[G] => decl
    case decl: SendDecl[G] => decl
  }

  def scanBlocks[G](node: ParRegion[G]): Seq[ParBlock[G]] = node match {
    case ParParallel(regions) => regions.flatMap(scanBlocks)
    case ParSequential(regions) => regions.flatMap(scanBlocks)
    case block: ParBlock[G] => Seq(block)
  }

  def scanShared[G](node: Node[G]): Seq[Declaration[G]] = node.transSubnodes.collect {
    case decl: CLocalDeclaration[G] if decl.decl.specs.collectFirst{case GPULocal() => ()}.isDefined => decl
  }

  def scanJavaBipGuards[G](nodes: Seq[Declaration[G]]): Seq[(Expr[G], JavaMethod[G])] = nodes.collect {
    case m: JavaMethod[G] if BipGuard.getName(m).isDefined => (BipGuard.getName(m).get, m)
  }

  def scanJavaBipStatePredicates[G](nodes: Seq[JavaModifier[G]]): Seq[(Expr[G], JavaAnnotation[G])] = nodes.collect {
    case ann: JavaAnnotation[G] if isBip(ann, "StatePredicate") => (ann.expect("state"), ann)
  }

  def enterContext[G](node: Node[G], ctx: ReferenceResolutionContext[G], inGPUKernel: Boolean = false): ReferenceResolutionContext[G] = (node match {
    case ns: JavaNamespace[G] => ctx
      .copy(currentJavaNamespace=Some(ns))
      .copy(stack = ns.imports.flatMap(ResolveTypes.scanImport[G](_, ctx.asTypeResolutionContext)) +: ctx.stack)
      .declare(ns.declarations)
    case cls: JavaClassOrInterface[G] => {
      val newCtx = ctx
        .copy(currentJavaClass = Some(cls))
        .copy(currentThis = Some(RefJavaClass(cls)))
        .declare(cls.decls)
        .declareJavaBipGuards(scanJavaBipGuards(cls.decls))
        .declareJavaBipStatePredicates(scanJavaBipStatePredicates(cls.modifiers))

      /* JavaBIP _name keys_ of guard annotations/states cut in line because transitions need to refer to them,
          _by string value_. So they are fully resolved before proceeding.
          E.g. the following two annotations on a method are considered equivalent by the current JavaBIP engine implementation,
          given that INIT is some static final field containing "initState":
          @Transition(source = INIT, ...)
          @Transition(source = "initState", ...)
          To make this stringly lookup work, state predicate names & guard names must be resolved before
          anything else in java classes
          */
      newCtx.javaBipGuards.keys.map(resolve(_, newCtx))
      newCtx.javaBipStatePredicates.keys.map(resolve(_, newCtx))
      newCtx
    }
    case deref: JavaDeref[G] => return ctx
      .copy(topLevelJavaDeref = ctx.topLevelJavaDeref.orElse(Some(deref)))
    case cls: Class[G] => ctx
      .copy(currentThis=Some(RefClass(cls)))
      .declare(cls.declarations)
      // Ensure occurrences of type variables within the class that defines them are ignored when substituting
      .appendTypeEnv(cls.typeArgs.map(v => (v, TVar[G](v.ref))).toMap)
    case adt: AxiomaticDataType[G] => ctx
      // Ensure occurrences of type variables within the adt that defines them are ignored when substituting
      .declare(adt.declarations)
      .appendTypeEnv(adt.typeArgs.map(v => (v, TVar[G](v.ref))).toMap)
    case seqProg: SeqProg[G] => ctx
      .copy(currentThis = Some(RefSeqProg(seqProg)))
      .declare(seqProg.decls)
      .declare(seqProg.endpoints)
      .declare(seqProg.args)
    case seqProg: PVLSeqProg[G] => ctx
      .copy(currentThis = Some(RefPVLSeqProg(seqProg)))
      .declare(seqProg.args)
      .declare(seqProg.declarations)
    case method: JavaMethod[G] => ctx
      .copy(currentResult=Some(RefJavaMethod(method)))
      .copy(inStaticJavaContext=method.modifiers.collectFirst { case _: JavaStatic[_] => () }.nonEmpty)
      .declare(method.declarations ++ method.body.map(scanLabels).getOrElse(Nil))
    case fields: JavaFields[G] => ctx
      .copy(currentInitializerType=Some(fields.t))
      .copy(inStaticJavaContext=fields.modifiers.collectFirst { case _: JavaStatic[_] => () }.nonEmpty)
    case init: JavaSharedInitialization[G] => ctx
      .copy(inStaticJavaContext=init.isStatic)
    case locals: JavaLocalDeclaration[G] => ctx
      .copy(currentInitializerType=Some(locals.t))
    case decl: JavaVariableDeclaration[G] => ctx
      .copy(currentInitializerType=ctx.currentInitializerType.map(t => FuncTools.repeat((t: Type[G]) => TArray(t), decl.moreDims, t)))
    case arr: JavaNewLiteralArray[G] => ctx
      .copy(currentInitializerType=Some(FuncTools.repeat((t: Type[G]) => TArray(t), arr.dims, arr.baseType)))
    case init: JavaLiteralArray[G] => ctx
      .copy(currentInitializerType=Some(ctx.currentInitializerType.get match {
        case TArray(elem) => elem
        case _ => throw WrongArrayInitializer(init)
      }))
    case func: CFunctionDefinition[G] =>
      var res = ctx
        .copy(currentResult=Some(RefCFunctionDefinition(func)))
        .declare(C.paramsFromDeclarator(func.declarator) ++ scanLabels(func.body) ++ func.contract.givenArgs ++ func.contract.yieldsArgs)
      if(func.specs.collectFirst{case _: CGpgpuKernelSpecifier[G] => ()}.isDefined)
        res = res.declare(scanShared(func.body))
      res
    case func: CGlobalDeclaration[G] =>
      if(func.decl.contract.nonEmpty && func.decl.inits.size > 1) {
        throw MultipleForwardDeclarationContractError(func)
      }
      func.decl.inits.zipWithIndex.foldLeft(
        ctx.declare(func.decl.contract.givenArgs ++ func.decl.contract.yieldsArgs)
      ) {
        case (ctx, (init, idx)) =>
          val info = C.getDeclaratorInfo(init.decl)
          ctx
            .declare(info.params.getOrElse(Nil))
            .copy(currentResult=info.params.map(_ => RefCGlobalDeclaration(func, idx)))
      }
    case func: CPPFunctionDefinition[G] =>
      ctx
        .copy(currentResult = Some(RefCPPFunctionDefinition(func)))
        .declare(CPP.paramsFromDeclarator(func.declarator) ++ scanLabels(func.body) ++ func.contract.givenArgs ++ func.contract.yieldsArgs)
    case func: CPPGlobalDeclaration[G] =>
      if (func.decl.contract.nonEmpty && func.decl.inits.size > 1) {
        throw MultipleForwardDeclarationContractError(func)
      }
      func.decl.inits.zipWithIndex.foldLeft(
        ctx.declare(func.decl.contract.givenArgs ++ func.decl.contract.yieldsArgs)
      ) {
        case (ctx, (init, idx)) =>
          val info = CPP.getDeclaratorInfo(init.decl)
          ctx
            .declare(info.params.getOrElse(Nil))
            .copy(currentResult = info.params.map(_ => RefCPPGlobalDeclaration(func, idx)))
      }
    case func: CPPLambdaDefinition[G] =>
      ctx.declare(CPP.paramsFromDeclarator(func.declarator) ++ scanLabels(func.body) ++ func.contract.givenArgs ++ func.contract.yieldsArgs)
    case func: LlvmFunctionDefinition[G] => ctx
      .copy(currentResult = Some(RefLlvmFunctionDefinition(func)))
    case func: LlvmSpecFunction[G] => ctx
      .copy(currentResult = Some(RefLlvmSpecFunction(func)))
      .declare(func.args)
    case par: ParStatement[G] => ctx
      .declare(scanBlocks(par.impl).map(_.decl))
    case Scope(locals, body) => ctx
      .declare(locals ++ scanScope(body, inGPUKernel))
    case app: ContractApplicable[G] => ctx
      .copy(currentResult = Some(Referrable.from(app).head.asInstanceOf[ResultTarget[G]] /* PB TODO: ew */))
      .declare(app.declarations ++ app.body.map(scanLabels).getOrElse(Nil))
    case app: Applicable[G] => ctx
      .declare(app.declarations ++ app.body.map(scanLabels).getOrElse(Nil))
    case declarator: Declarator[G] => ctx
      .declare(declarator.declarations)
    case _ => ctx
  }).copy(topLevelJavaDeref = None)

  def resolveFlatly[G](node: Node[G], ctx: ReferenceResolutionContext[G]): Unit = node match {
    case local@CLocal(name) =>
      local.ref = Some(C.findCName(name, ctx).getOrElse(throw NoSuchNameError("local", name, local)))
    case local@CPPLocal(name, arg) =>
      local.ref = Some(CPP.findCPPName(name, ctx).headOption.getOrElse(throw NoSuchNameError("local", name, local)))
    case local @ JavaLocal(name) =>
      val start: Option[JavaNameTarget[G]] = if (ctx.javaBipGuardsEnabled) {
        Java.findJavaBipGuard(ctx, name).map(RefJavaBipGuard(_))
      } else { None }
      local.ref = Some(start.orElse(
        Java.findJavaName(name, fromStaticContext = ctx.inStaticJavaContext, ctx.asTypeResolutionContext)
          .orElse(Java.findJavaTypeName(Seq(name), ctx.asTypeResolutionContext) match {
            case Some(target: JavaNameTarget[G]) => Some(target)
            case Some(_) | None => None
          }))
          .getOrElse(
            if (ctx.topLevelJavaDeref.isEmpty) throw NoSuchNameError("local", name, local)
            else RefUnloadedJavaNamespace(Seq(name))))
    case local @ PVLLocal(name) =>
      local.ref = Some(PVL.findName(name, ctx).getOrElse(throw NoSuchNameError("local", name, local)))
    case local@Local(ref) =>
      ref.tryResolve(name => Spec.findLocal(name, ctx).getOrElse(throw NoSuchNameError("local", name, local)))
    case local@TVar(ref) =>
      ref.tryResolve(name => Spec.findLocal(name, ctx).getOrElse(throw NoSuchNameError("type variable", name, local)))
    case funcOf@FunctionOf(v, vars) =>
      v.tryResolve(name => Spec.findLocal(name, ctx).getOrElse(throw NoSuchNameError("local", name, funcOf)))
      vars.foreach(v => v.tryResolve(name => Spec.findLocal(name, ctx).getOrElse(throw NoSuchNameError("local", name, funcOf))))
    case local@SilverLocalAssign(ref, _) =>
      ref.tryResolve(name => Spec.findLocal(name, ctx).getOrElse(throw NoSuchNameError("local", name, local)))
    case local@PVLEndpointName(name) => PVL.findName(name, ctx) match {
      case Some(ref: RefPVLEndpoint[G]) => local.ref = Some(ref)
      case Some(_) => throw ForbiddenEndpointNameType(local)
      case None => throw NoSuchNameError("endpoint", name, local)
    }
    case access@PVLAccess(subject, field) =>
        access.ref = Some(PVL.findDerefOfClass(subject.cls, field).getOrElse(throw NoSuchNameError("field", field, access)))
    case endpoint: PVLEndpoint[G] =>
      endpoint.ref = Some(PVL.findConstructor(TClass(endpoint.cls.decl.ref[Class[G]], Seq()), Seq(), endpoint.args).getOrElse(throw ConstructorNotFound(endpoint)))
    case parAssign: PVLSeqAssign[G] =>
      parAssign.receiver.tryResolve(receiver => PVL.findName(receiver, ctx) match {
        case Some(RefPVLEndpoint(decl)) => decl
        case Some(_) => throw ForbiddenEndpointNameType(parAssign)
        case None => throw NoSuchNameError("endpoint", receiver, parAssign)
      })
      parAssign.field.tryResolve(field => PVL.findDerefOfClass[G](parAssign.receiver.decl.cls.decl, field) match {
        case Some(RefField(field)) => field
        case Some(_) => throw UnassignableField(parAssign)
        case None => throw NoSuchNameError("field", field, parAssign)
      })
    case deref@CStructDeref(struct, field) =>
      deref.ref = Some(C.findPointerDeref(struct, field, ctx, deref.blame).getOrElse(throw NoSuchNameError("field", field, deref)))
    case deref@CStructAccess(obj, field) =>
      deref.ref = Some(C.findDeref(obj, field, ctx, deref.blame).getOrElse(throw NoSuchNameError("field", field, deref)))
    case deref@CPPClassMethodOrFieldAccess(obj, methodOrFieldName) =>
      deref.ref = Some(CPP.findDeref(obj, methodOrFieldName, ctx, deref.blame).headOption.getOrElse(throw NoSuchNameError("field or instance method", methodOrFieldName, deref)))
    case deref@JavaDeref(obj, field) =>
      deref.ref = Some(Java.findDeref(obj, field, ctx, deref.blame).getOrElse(throw NoSuchNameError("field", field, deref)))
      if (ctx.topLevelJavaDeref.contains(deref) && (deref.ref match {
        case Some(RefUnloadedJavaNamespace(_)) => true
        case _ => false
      })) throw NoSuchNameError("field", field, deref)
    case deref @ PVLDeref(obj, field) =>
      deref.ref = Some(PVL.findDeref(obj, field, ctx, deref.blame).getOrElse(throw NoSuchNameError("field", field, deref)))
    case deref@Deref(obj, field) =>
      field.tryResolve(name => Spec.findField(obj, name).getOrElse(throw NoSuchNameError("field", name, deref)))
    case deref@ModelDeref(obj, field) =>
      field.tryResolve(name => Spec.findModelField(obj, name).getOrElse(throw NoSuchNameError("field", name, deref)))
    case deref@SilverDeref(_, field) =>
      field.tryResolve(name => Spec.findSilverField(name, ctx).getOrElse(throw NoSuchNameError("field", name, deref)))
    case deref@SilverFieldLocation(_, field) =>
      field.tryResolve(name => Spec.findSilverField(name, ctx).getOrElse(throw NoSuchNameError("field", name, deref)))
    case deref@SilverCurFieldPerm(_, field) =>
      field.tryResolve(name => Spec.findSilverField(name, ctx).getOrElse(throw NoSuchNameError("field", name, deref)))
    case deref@SilverFieldAssign(_, field, _) =>
      field.tryResolve(name => Spec.findSilverField(name, ctx).getOrElse(throw NoSuchNameError("field", name, deref)))

    case inv@CInvocation(obj, _, givenMap, yields) =>
      inv.ref = Some(C.resolveInvocation(obj, ctx))
      Spec.resolveGiven(givenMap, inv.ref.get, inv)
      Spec.resolveYields(ctx, yields, inv.ref.get, inv)
    case inv@CPPInvocation(obj, args, givenMap, yields) =>
      inv.ref = Some(CPP.resolveInvocation(obj, args, ctx))
      Spec.resolveGiven(givenMap, inv.ref.get, inv)
      Spec.resolveYields(ctx, yields, inv.ref.get, inv)
    case inv@GpgpuCudaKernelInvocation(name, blocks, threads, args, givenMap, yields) =>
      val kernel = C.findCName(name, ctx).getOrElse(throw NoSuchNameError("kernel", name, inv))
      inv.ref = Some(kernel match {
        case target: CInvocationTarget[G] => target
        case _ => throw NotApplicable(inv)
      })
      Spec.resolveGiven(givenMap, inv.ref.get, inv)
      Spec.resolveYields(ctx, yields, inv.ref.get, inv)
    case inv@JavaInvocation(obj, _, method, args, givenMap, yields) =>
      inv.ref = Some((obj match {
        case Some(obj) => Java.findMethod(ctx, obj, method, args, inv.blame)
        case None => Java.findMethod(ctx, method, args)
      }).getOrElse(throw NoSuchNameError("method", method, inv)))
      Spec.resolveGiven(givenMap, inv.ref.get, inv)
      Spec.resolveYields(ctx, yields, inv.ref.get, inv)
    case inv@JavaNewClass(args, typeArgs, name, givenMap, yields) =>
      inv.ref = Some(Java.findConstructor(name, args).getOrElse(throw NoSuchConstructor(inv)))
      Spec.resolveGiven(givenMap, inv.ref.get, inv)
      Spec.resolveYields(ctx, yields, inv.ref.get, inv)
    case inv@PVLInvocation(None, method, args, typeArgs, givenMap, yields) =>
      inv.ref = Some(PVL.findMethod(method, args, typeArgs, ctx).getOrElse(throw NoSuchNameError("method", method, inv)))
      Spec.resolveGiven(givenMap, inv.ref.get, inv)
      Spec.resolveYields(ctx, yields, inv.ref.get, inv)
    case inv@PVLInvocation(Some(obj), method, args, typeArgs, givenMap, yields) =>
      inv.ref = Some(PVL.findInstanceMethod(obj, method, args, typeArgs, inv.blame).getOrElse(throw NoSuchNameError("method", method, inv)))
      Spec.resolveGiven(givenMap, inv.ref.get, inv)
      Spec.resolveYields(ctx, yields, inv.ref.get, inv)
    case inv@PVLNew(t, typeArgs, args, givenMap, yields) =>
      inv.ref = Some(PVL.findConstructor(t, typeArgs, args).getOrElse(throw NoSuchConstructor(inv)))
      Spec.resolveGiven(givenMap, inv.ref.get, inv)
      Spec.resolveYields(ctx, yields, inv.ref.get, inv)
    case n@NewObject(ref) =>
      ref.tryResolve(name => Spec.findClass(name, ctx.asTypeResolutionContext).getOrElse(throw NoSuchNameError("class", name, n)))
    case n@ModelNew(ref) =>
      ref.tryResolve(name => Spec.findModel(name, ctx.asTypeResolutionContext).getOrElse(throw NoSuchNameError("model", name, n)))
    case n@SilverNewRef(target, fields) =>
      target.tryResolve(name => Spec.findLocal(name, ctx).getOrElse(throw NoSuchNameError("local", name, n)))
      fields.foreach(_.tryResolve(name => Spec.findSilverField(name, ctx).getOrElse(throw NoSuchNameError("field", name, n))))
    case inv@ADTFunctionInvocation(typeArgs, ref, args) =>
      typeArgs match {
        case Some((adt, typeArgs)) =>
          // Fully-qualified external invocation
          adt.tryResolve(name => Spec.findAdt(name, ctx.asTypeResolutionContext).getOrElse(throw NoSuchNameError("adt", name, inv)))
          ref.tryResolve(name => Spec.findAdtFunction(adt.decl, name).getOrElse(throw NoSuchNameError("function", name, inv)))
        case None =>
          // Non-qualified internal invocation
          ???
      }
    case inv@SilverPartialADTFunctionInvocation(name, args, partialTypeArgs) =>
      inv.ref = Some(Spec.findAdtFunction(name, ctx).getOrElse(throw NoSuchNameError("function", name, inv)))
      partialTypeArgs.foreach(mapping => mapping._1.tryResolve(name => Spec.findAdtTypeArg(inv.adt, name).getOrElse(throw NoSuchNameError("type variable", name, inv))))
    case inv @ InvokeProcedure(ref, _, _, _, givenMap, yields) =>
      ref.tryResolve(name => Spec.findProcedure(name, ctx).getOrElse(throw NoSuchNameError("procedure", name, inv)))
      Spec.resolveGiven(givenMap, RefProcedure(ref.decl), inv)
      Spec.resolveYields(ctx, yields, RefProcedure(ref.decl), inv)
    case inv @ ProcedureInvocation(ref, _, _, _, givenMap, yields) =>
      ref.tryResolve(name => Spec.findProcedure(name, ctx).getOrElse(throw NoSuchNameError("procedure", name, inv)))
      Spec.resolveGiven(givenMap, RefProcedure(ref.decl), inv)
      Spec.resolveYields(ctx, yields, RefProcedure(ref.decl), inv)
    case inv@FunctionInvocation(ref, _, _, givenMap, yields) =>
      ref.tryResolve(name => Spec.findFunction(name, ctx).getOrElse(throw NoSuchNameError("function", name, inv)))
      Spec.resolveGiven(givenMap, RefFunction(ref.decl), inv)
      Spec.resolveYields(ctx, yields, RefFunction(ref.decl), inv)
    case inv@PredicateApply(ref, _, _) =>
      ref.tryResolve(name => Spec.findPredicate(name, ctx).getOrElse(throw NoSuchNameError("predicate", name, inv)))
    case inv@SilverCurPredPerm(ref, _) =>
      ref.tryResolve(name => Spec.findPredicate(name, ctx).getOrElse(throw NoSuchNameError("predicate", name, inv)))
    case inv @ InvokeMethod(obj, ref, _, _, _, givenMap, yields) =>
      ref.tryResolve(name => Spec.findMethod(obj, name).getOrElse(throw NoSuchNameError("method", name, inv)))
      Spec.resolveGiven(givenMap, RefInstanceMethod(ref.decl), inv)
      Spec.resolveYields(ctx, yields, RefInstanceMethod(ref.decl), inv)
    case inv @ MethodInvocation(obj, ref, _, _, _, givenMap, yields) =>
      ref.tryResolve(name => Spec.findMethod(obj, name).getOrElse(throw NoSuchNameError("method", name, inv)))
      Spec.resolveGiven(givenMap, RefInstanceMethod(ref.decl), inv)
      Spec.resolveYields(ctx, yields, RefInstanceMethod(ref.decl), inv)
    case inv@InstanceFunctionInvocation(obj, ref, _, _, givenMap, yields) =>
      ref.tryResolve(name => Spec.findInstanceFunction(obj, name).getOrElse(throw NoSuchNameError("function", name, inv)))
    case inv@InstancePredicateApply(obj, ref, _, _) =>
      ref.tryResolve(name => Spec.findInstancePredicate(obj, name).getOrElse(throw NoSuchNameError("predicate", name, inv)))
    case inv @ CoalesceInstancePredicateApply(obj, ref, _, _) =>
      ref.tryResolve(name => Spec.findInstancePredicate(obj, name).getOrElse(throw NoSuchNameError("predicate", name, inv)))

    case defn: CFunctionDefinition[G] =>
      defn.ref = C.findForwardDeclaration(defn.declarator, ctx)
    case decl: CInit[G] =>
      decl.ref = C.findDefinition(decl.decl, ctx)

    case defn: CPPFunctionDefinition[G] =>
      defn.ref = CPP.findForwardDeclaration(defn.declarator, ctx)
    case decl: CPPInit[G] =>
      decl.ref = CPP.findDefinition(decl.decl, ctx)

    case goto@CGoto(name) =>
      goto.ref = Some(Spec.findLabel(name, ctx).getOrElse(throw NoSuchNameError("label", name, goto)))
    case goto@Goto(lbl) =>
      lbl.tryResolve(name => Spec.findLabel(name, ctx).getOrElse(throw NoSuchNameError("label", name, goto)))
    case brk@Break(Some(lbl)) =>
      lbl.tryResolve(name => Spec.findLabel(name, ctx).getOrElse(throw NoSuchNameError("label", name, brk)))
    case cont@Continue(Some(lbl)) =>
      lbl.tryResolve(name => Spec.findLabel(name, ctx).getOrElse(throw NoSuchNameError("label", name, cont)))
    case old@Old(_, Some(lbl)) =>
      lbl.tryResolve(name => Spec.findLabel(name, ctx).getOrElse(throw NoSuchNameError("label", name, old)))

    case recv@Recv(ref) =>
      ref.tryResolve(name => Spec.findSend(name, ctx).getOrElse(throw NoSuchNameError("send statement", name, recv)))

    case res@AmbiguousResult() =>
      res.ref = Some(ctx.currentResult.getOrElse(throw ResultOutsideMethod(res)))
    case diz@AmbiguousThis() =>
      diz.ref = Some(ctx.currentThis.getOrElse(throw WrongThisPosition(diz)))

    case proc: ModelProcess[G] =>
      proc.modifies.foreach(_.tryResolve(name => Spec.findModelField(name, ctx)
        .getOrElse(throw NoSuchNameError("field", name, proc))))
      proc.accessible.foreach(_.tryResolve(name => Spec.findModelField(name, ctx)
        .getOrElse(throw NoSuchNameError("field", name, proc))))
    case act: ModelAction[G] =>
      act.modifies.foreach(_.tryResolve(name => Spec.findModelField(name, ctx)
        .getOrElse(throw NoSuchNameError("field", name, act))))
      act.accessible.foreach(_.tryResolve(name => Spec.findModelField(name, ctx)
        .getOrElse(throw NoSuchNameError("field", name, act))))

    case inv@ProcessApply(ref, _) =>
      ref.tryResolve(name => ???)
    case inv@ActionApply(ref, _) =>
      ref.tryResolve(name => ???)

    case atomic@ParAtomic(invs, _) =>
      invs.foreach(_.tryResolve(name => Spec.findParInvariant(name, ctx)
        .getOrElse(throw NoSuchNameError("invariant", name, atomic))))
    case barrier@ParBarrier(block, invs, _, _, _) =>
      block.tryResolve(name => Spec.findParBlock(name, ctx)
        .getOrElse(throw NoSuchNameError("block", name, barrier)))
      invs.foreach(_.tryResolve(name => Spec.findParInvariant(name, ctx)
        .getOrElse(throw NoSuchNameError("invariant", name, barrier))))

    case arr@JavaLiteralArray(_) =>
      arr.typeContext = Some(ctx.currentInitializerType.get match {
        case t@TArray(_) => t
        case _ => throw WrongArrayInitializer(arr)
      })

    case ann@JavaAnnotation(_, _) if isBip(ann, "Transition") =>
      val guard: Option[Expr[G]] = ann.get("guard").map { g =>
        val expr: Expr[G] = ctx.javaParser.parse(getLit(g), g.o)
        // TODO: What about check errors here?
        resolve(expr, ctx.enableJavaBipGuards())
        expr
      }

      def extractExpr(s: Option[Expr[_]]): (String, Origin) = s match {
        case None => ("true", ann.o)
        case Some(s @ JavaStringValue(data, _)) => (data, s.o)
        case Some(n) => throw MalformedBipAnnotation(n, "pre- and post-conditions must be string literals")
      }

      val (r, ro) = extractExpr(ann.get("requires"))
      val requires: Expr[G] = ctx.javaParser.parse(r, ro)
      resolve(requires, ctx)

      val (e, eo) = extractExpr(ann.get("ensures"))
      val ensures: Expr[G] = ctx.javaParser.parse(e, eo)
      resolve(ensures, ctx)

      val name = getLit(ann.expect("name"))
      val source = getLit(ann.expect("source"))
      val target = getLit(ann.expect("target"))
      ann.data = Some(BipTransition[G](name,
        Java.findJavaBipStatePredicate(ctx, source),
        Java.findJavaBipStatePredicate(ctx, target),
        ann.get("guard").map(getLit(_)),
        guard, requires, ensures)(ann.o))

    case ann@JavaAnnotation(_, _) if isBip(ann, "Invariant") =>
      val expr: Expr[G] = ctx.javaParser.parse(getLit(ann.expect("value")), ann.expect("value").o)
      resolve(expr, ctx)
      ann.data = Some(BipInvariant(expr))

    case ann@JavaAnnotation(_, _) if isBip(ann, "StatePredicate") =>
      val expr: Expr[G] = ctx.javaParser.parse(getLit(ann.expect("expr")), ann.expect("expr").o)
      resolve(expr, ctx) // TODO (RR): Throwing away errors here?
      ann.data = Some(BipStatePredicate(getLit(ann.expect("state")), expr)(ann.o))

    case ann@JavaAnnotation(_, _) if isBip(ann, "ComponentType") =>
      ann.data = Some(BipComponent(getLit(ann.expect("name")),
        Java.findJavaBipStatePredicate(ctx, getLit(ann.expect("initial")))))

    case ann@JavaAnnotation(_, _) if isBip(ann, "Data") =>
      ann.data = Some(BipData(getLit(ann.expect("name")))(ann.o))

    case ann@JavaAnnotation(_, _) if isBip(ann, "Guard") =>
      ann.data = Some(BipGuard(getLit(ann.expect("name"))))

    case ann: JavaAnnotation[G] if isBip(ann, "Port") =>
      val portType: BipPortType[G] = ann.expect("type") match {
        case p @ JavaDeref(_, "enforceable") => BipEnforceable[G]()(p.o)
        case p @ JavaDeref(_, "spontaneous") => BipSpontaneous[G]()(p.o)
        case p @ JavaDeref(_, "internal") => BipInternal[G]()(p.o)
        case e => throw MalformedBipAnnotation(e, "Can be either PortType.enforceable, spontaneous, or internal")
      }
      ann.data = Some(BipPort[G](getLit(ann.expect("name")), portType)(ann.o))

    case ann: JavaAnnotation[G] if isBip(ann, "Pure") =>
      ann.data = Some(BipPure[G]())

    case portName @ JavaBipGlueName(JavaTClass(Ref(cls: JavaClass[G]), Nil), name) =>
      portName.data = Some((cls, getLit(name)))

    case contract: LlvmFunctionContract[G] =>
      val applicableContract = ctx.llvmSpecParser.parse(contract, contract.o)
      contract.data = Some(applicableContract)
      resolve(applicableContract, ctx)
    case local: LlvmLocal[G] =>
      local.ref = ctx.currentResult.get match {
        case RefLlvmFunctionDefinition(decl) =>
          decl.contract.variableRefs.find(ref => ref._1 == local.name) match {
            case Some(ref) => Some(ref._2)
            case None => throw NoSuchNameError("local", local.name, local)
          }
        case RefLlvmSpecFunction(_) =>
          Some(Spec.findLocal(local.name, ctx).getOrElse(throw NoSuchNameError("local", local.name, local)).ref)
        case _ => None
      }
    case inv: LlvmAmbiguousFunctionInvocation[G] =>
      inv.ref = LLVM.findCallable(inv.name, ctx) match {
        case Some(callable) => Some(callable.ref)
        case None => throw NoSuchNameError("function", inv.name, inv)
      }
    case glob: LlvmGlobal[G] =>
      val decl = ctx.llvmSpecParser.parse(glob, glob.o)
      glob.data = Some(decl)
      resolve(decl, ctx)
    case _ =>
  }
}
