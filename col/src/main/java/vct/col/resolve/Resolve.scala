package vct.col.resolve

import vct.col.ast.{AmbiguousResult, AmbiguousThis, Applicable, CDeclarationStatement, CFunctionDefinition, CGoto, CInvocation, CLabeledStatement, CLocal, CStructAccess, CTypedefName, CheckError, Class, ContractApplicable, Declaration, Declarator, Deref, DiagnosticOrigin, GlobalDeclaration, Goto, GpgpuCudaKernelInvocation, JavaClassOrInterface, JavaConstructor, JavaDeref, JavaInvocation, JavaLocal, JavaLocalDeclarationStatement, JavaMethod, JavaName, JavaNamespace, JavaTClass, JavaTUnion, LabelDecl, Local, LocalDecl, ModelAction, ModelProcess, Node, PVLConstructor, PVLDeref, PVLInvocation, PVLLocal, PVLNamedType, PVLNew, ParAtomic, ParBarrier, Program, Recv, Scope, Send, TClass}

case object Resolve {
  def resolve(program: Program): Seq[CheckError] = {
    ResolveTypes.resolve(program)
    ResolveReferences.resolve(program)
  }
}

case object ResolveTypes {
  def resolve(program: Program): Seq[GlobalDeclaration] = {
    val ctx = TypeResolutionContext()
    resolve(program, ctx)
    ctx.externallyLoadedClasses.toSeq
  }

  def resolve(node: Node, ctx: TypeResolutionContext): Unit = {
    val innerContext = enterContext(node, ctx)
    node.subnodes.foreach(resolve(_, innerContext))
    resolveOne(node, ctx)
  }

  def enterContext(node: Node, ctx: TypeResolutionContext): TypeResolutionContext = node match {
    case Program(decls) =>
      ctx.replace(stack=decls.flatMap(Referrable.from) +: ctx.stack)
    case ns @ JavaNamespace(_, _, decls) =>
      ctx.replace(stack=decls.flatMap(Referrable.from) +: ctx.stack, namespace=Some(ns))
    case _ => ctx
  }

  def resolveOne(node: Node, ctx: TypeResolutionContext): Unit = node match {
    case javaClass @ JavaTClass(genericNames) =>
      val names = genericNames.map(_._1)
      javaClass.ref = Some(Java.findJavaTypeName(names, ctx).getOrElse(
        throw NoSuchNameError("class", names.mkString("."), javaClass)))
    case t @ CTypedefName(name) =>
      t.ref = Some(C.findCTypeName(name, ctx).getOrElse(
        throw NoSuchNameError("struct", name, t)
      ))
    case t @ PVLNamedType(name) =>
      t.ref = Some(PVL.findTypeName(name, ctx).getOrElse(
        throw NoSuchNameError("class", name, t)))
    case _ =>
  }
}

case object ResolveReferences {
  def resolve(program: Program): Seq[CheckError] = {
    resolve(program, ReferenceResolutionContext())
  }

  def resolve(node: Node, ctx: ReferenceResolutionContext): Seq[CheckError] = {
    val innerCtx = enterContext(node, ctx)
    val childErrors = node.subnodes.flatMap(resolve(_, innerCtx))

    if(childErrors.nonEmpty) childErrors
    else {
      resolveFlatly(node, ctx)
      node.check(ctx.checkContext)
    }
  }

  def scanScope(node: Node): Seq[Declaration] = node match {
    case _: Scope => Nil
    case CDeclarationStatement(decl) => Seq(decl)
    case JavaLocalDeclarationStatement(decl) => Seq(decl)
    case LocalDecl(v) => Seq(v)
    case other => other.subnodes.flatMap(scanScope)
  }

  def scanLabels(node: Node): Seq[LabelDecl] = node.transSubnodes.collect {
    case decl: LabelDecl => decl
  }

  def enterContext(node: Node, ctx: ReferenceResolutionContext): ReferenceResolutionContext = (node match {
    case ns: JavaNamespace => ctx
      .replace(currentJavaNamespace=Some(ns)).declare(ns.declarations)
    case cls: JavaClassOrInterface => ctx
      .replace(currentJavaClass=Some(cls))
      .replace(currentThisType=Some(JavaTClass(Seq((cls.name, None)))(DiagnosticOrigin)))
      .declare(cls.decls)
    case cls: Class => ctx
      .replace(currentThisType=Some(TClass(cls.ref)))
      .declare(cls.declarations)
    case app: ContractApplicable => ctx
      .replace(currentReturnType=Some(app.returnType))
      .declare(app.declarations ++ app.body.map(scanLabels).getOrElse(Nil))
    case method: JavaMethod => ctx
      .replace(currentReturnType=Some(method.returnType)).declare(method.declarations)
    case func: CFunctionDefinition => ctx
      .replace(currentReturnType=Some(C.typeOrReturnTypeFromDeclaration(func.specs, func.declarator)))
      .declare(C.paramsFromDeclarator(func.declarator) ++ scanLabels(func.body)) // FIXME suspect wrt contract declarations and stuff
    case Scope(locals, body) => ctx
      .declare(locals ++ scanScope(body))
    case app: Applicable => ctx
      .declare(app.declarations ++ app.body.map(scanLabels).getOrElse(Nil))
    case declarator: Declarator => ctx
      .declare(declarator.declarations)
    case _ => ctx
  }).replace(checkContext=node.enterCheckContext(ctx.checkContext))

  def resolveFlatly(node: Node, ctx: ReferenceResolutionContext): Unit = node match {
    case local @ CLocal(name) =>
      local.ref = Some(C.findCName(name, ctx).getOrElse(throw NoSuchNameError("local", name, local)))
    case local @ JavaLocal(name) =>
      local.ref = Some(Java.findJavaName(name, ctx).getOrElse(throw NoSuchNameError("local", name, local)))
    case local @ PVLLocal(name) =>
      local.ref = Some(PVL.findName(name, ctx).getOrElse(throw NoSuchNameError("local", name, local)))
    case local @ Local(ref) =>
      ref.tryResolve(name => Spec.findLocal(name, ctx).getOrElse(throw NoSuchNameError("local", name, local)))

    case deref @ CStructAccess(obj, field) =>
      deref.ref = Some(C.findDeref(obj, field, ctx).getOrElse(throw NoSuchNameError("field", field, deref)))
    case deref @ JavaDeref(obj, field) =>
      deref.ref = Some(Java.findDeref(obj, field, ctx).getOrElse(throw NoSuchNameError("field", field, deref)))
    case deref @ PVLDeref(obj, field) =>
      deref.ref = Some(PVL.findDeref(obj, field, ctx).getOrElse(throw NoSuchNameError("field", field, deref)))

    case inv @ CInvocation(obj, _, _, _) =>
      inv.ref = Some(C.resolveInvocation(obj, ctx))
    case inv @ GpgpuCudaKernelInvocation(name, blocks, threads, args, _, _) =>
      val kernel = C.findCName(name, ctx).getOrElse(throw NoSuchNameError("kernel", name, inv))
      inv.ref = Some(kernel match {
        case target: CInvocationTarget => target
        case _ => throw NotApplicable(inv)
      })
    case inv @ JavaInvocation(obj, _, method, args, _, _) =>
      inv.ref = Some((obj match {
        case Some(obj) => Java.findMethod(obj, method, args)
        case None => Java.findMethod(ctx, method, args)
      }).getOrElse(throw NoSuchNameError("method", method, inv)))
    case inv @ PVLInvocation(None, method, args, _, _) =>
      inv.ref = Some(PVL.findMethod(method, args, ctx).getOrElse(throw NoSuchNameError("method", method, inv)))
    case inv @ PVLInvocation(Some(obj), method, args, _, _) =>
      inv.ref = Some(PVL.findInstanceMethod(obj, method, args).getOrElse(throw NoSuchNameError("method", method, inv)))

    case goto @ CGoto(name) =>
      goto.ref = Some(Spec.findLabel(name, ctx).getOrElse(throw NoSuchNameError("label", name, goto)))
    case goto @ Goto(lbl) =>
      lbl.tryResolve(name => Spec.findLabel(name, ctx).getOrElse(throw NoSuchNameError("label", name, goto)))
    case send @ Send(_, lbl, _) =>
      lbl.tryResolve(name => Spec.findLabel(name, ctx).getOrElse(throw NoSuchNameError("label", name, send)))
    case recv @ Recv(_, lbl, _) =>
      lbl.tryResolve(name => Spec.findLabel(name, ctx).getOrElse(throw NoSuchNameError("label", name, recv)))

    case n @ PVLNew(name, args) =>


    case res @ AmbiguousResult() =>
      res.ref = Some(ctx.currentReturnType.getOrElse(throw ResultOutsideMethod(res)))
    case diz @ AmbiguousThis() =>
      diz.ref = Some(ctx.currentThisType.get)

    case proc: ModelProcess =>
      proc.modifies.foreach(_.tryResolve(name => Spec.findModelField(name, ctx)
        .getOrElse(throw NoSuchNameError("field", name, proc))))
      proc.accessible.foreach(_.tryResolve(name => Spec.findModelField(name, ctx)
        .getOrElse(throw NoSuchNameError("field", name, proc))))

    case act: ModelAction =>
      act.modifies.foreach(_.tryResolve(name => Spec.findModelField(name, ctx)
        .getOrElse(throw NoSuchNameError("field", name, act))))
      act.accessible.foreach(_.tryResolve(name => Spec.findModelField(name, ctx)
        .getOrElse(throw NoSuchNameError("field", name, act))))

    case atomic @ ParAtomic(invs, _) =>
      invs.foreach(_.tryResolve(name => Spec.findParInvariant(name, ctx)
        .getOrElse(throw NoSuchNameError("invariant", name, atomic))))

    case barrier @ ParBarrier(block, invs, _, _, _) =>
      block.tryResolve(name => Spec.findParBlock(name, ctx)
        .getOrElse(throw NoSuchNameError("block", name, barrier)))
      invs.foreach(_.tryResolve(name => Spec.findParInvariant(name, ctx)
        .getOrElse(throw NoSuchNameError("invariant", name, barrier))))

    case _ =>
  }
}