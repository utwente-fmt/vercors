package vct.col.resolve

import vct.col.ast._
import vct.col.ast.temporaryimplpackage.util.Declarator
import vct.col.check.CheckError
import vct.col.origin._

case object Resolve {
  def resolve(program: Program[_]): Seq[CheckError] = {
    ResolveTypes.resolve(program)
    ResolveReferences.resolve(program)
  }
}

case object ResolveTypes {
  def resolve[G](program: Program[G]): Seq[GlobalDeclaration[G]] = {
    val ctx = TypeResolutionContext[G]()
    resolve(program, ctx)
    ctx.externallyLoadedElements.toSeq
  }

  def resolve[G](node: Node[G], ctx: TypeResolutionContext[G]): Unit = {
    val innerContext = enterContext(node, ctx)
    node.subnodes.foreach(resolve(_, innerContext))
    resolveOne(node, ctx)
  }

  def enterContext[G](node: Node[G], ctx: TypeResolutionContext[G]): TypeResolutionContext[G] = node match {
    case Program(decls, _) =>
      ctx.replace(stack=decls.flatMap(Referrable.from) +: ctx.stack)
    case ns: JavaNamespace[G] =>
      ctx.replace(stack=ns.declarations.flatMap(Referrable.from) +: ctx.stack, namespace=Some(ns))
    case decl: Declarator[G] =>
      ctx.replace(stack=decl.declarations.flatMap(Referrable.from) +: ctx.stack)
    case _ => ctx
  }

  def resolveOne[G](node: Node[G], ctx: TypeResolutionContext[G]): Unit = node match {
    case javaClass @ JavaNamedType(genericNames) =>
      val names = genericNames.map(_._1)
      javaClass.ref = Some(Java.findJavaTypeName(names, ctx).getOrElse(
        throw NoSuchNameError("class", names.mkString("."), javaClass)))
    case t @ CTypedefName(name) =>
      t.ref = Some(C.findCTypeName(name, ctx).getOrElse(
        throw NoSuchNameError("struct", name, t)
      ))
    case t @ PVLNamedType(name, typeArgs) =>
      t.ref = Some(PVL.findTypeName(name, ctx).getOrElse(
        throw NoSuchNameError("class", name, t)))
    case _ =>
  }
}

case object ResolveReferences {
  def resolve[G](program: Program[G]): Seq[CheckError] = {
    resolve(program, ReferenceResolutionContext[G]())
  }

  def resolve[G](node: Node[G], ctx: ReferenceResolutionContext[G]): Seq[CheckError] = {
    val innerCtx = enterContext(node, ctx)
    val childErrors = node.subnodes.flatMap(resolve(_, innerCtx))

    if(childErrors.nonEmpty) childErrors
    else {
      resolveFlatly(node, ctx)
      node.check(ctx.checkContext)
    }
  }

  def scanScope[G](node: Node[G]): Seq[Declaration[G]] = node match {
    case _: Scope[G] => Nil
    case CDeclarationStatement(decl) => Seq(decl)
    case JavaLocalDeclarationStatement(decl) => Seq(decl)
    case LocalDecl(v) => Seq(v)
    case other => other.subnodes.flatMap(scanScope)
  }

  def scanLabels[G](node: Node[G]): Seq[LabelDecl[G]] = node.transSubnodes.collect {
    case decl: LabelDecl[G] => decl
  }

  def scanBlocks[G](node: ParRegion[G]): Seq[ParBlock[G]] = node match {
    case ParParallel(regions) => regions.flatMap(scanBlocks)
    case ParSequential(regions) => regions.flatMap(scanBlocks)
    case block: ParBlock[G] => Seq(block)
  }

  def enterContext[G](node: Node[G], ctx: ReferenceResolutionContext[G]): ReferenceResolutionContext[G] = (node match {
    case ns: JavaNamespace[G] => ctx
      .replace(currentJavaNamespace=Some(ns)).declare(ns.declarations)
    case cls: JavaClassOrInterface[G] => ctx
      .replace(currentJavaClass=Some(cls))
      .replace(currentThis=Some(RefJavaClass(cls)))
      .declare(cls.decls)
    case cls: Class[G] => ctx
      .replace(currentThis=Some(RefClass(cls)))
      .declare(cls.declarations)
    case app: ContractApplicable[G] => ctx
      .replace(currentResult=Some(Referrable.from(app).head.asInstanceOf[ResultTarget[G]] /* PB TODO: ew */))
      .declare(app.declarations ++ app.body.map(scanLabels).getOrElse(Nil))
    case method: JavaMethod[G] => ctx
      .replace(currentResult=Some(RefJavaMethod(method)))
      .declare(method.declarations ++ method.body.map(scanLabels).getOrElse(Nil))
    case func: CFunctionDefinition[G] => ctx
      .replace(currentResult=Some(RefCFunctionDefinition(func)))
      .declare(C.paramsFromDeclarator(func.declarator) ++ scanLabels(func.body)) // FIXME suspect wrt contract declarations and stuff
    case par: ParStatement[G] => ctx
      .declare(scanBlocks(par.impl).map(_.decl))
    case Scope(locals, body) => ctx
      .declare(locals ++ scanScope(body))
    case app: Applicable[G] => ctx
      .declare(app.declarations ++ app.body.map(scanLabels).getOrElse(Nil))
    case declarator: Declarator[G] => ctx
      .declare(declarator.declarations)
    case _ => ctx
  }).replace(checkContext=node.enterCheckContext(ctx.checkContext))

  def resolveFlatly[G](node: Node[G], ctx: ReferenceResolutionContext[G]): Unit = node match {
    case local @ CLocal(name) =>
      local.ref = Some(C.findCName(name, ctx).getOrElse(throw NoSuchNameError("local", name, local)))
    case local @ JavaLocal(name) =>
      local.ref = Some(Java.findJavaName(name, ctx).getOrElse(throw NoSuchNameError("local", name, local)))
    case local @ PVLLocal(name) =>
      local.ref = Some(PVL.findName(name, ctx).getOrElse(throw NoSuchNameError("local", name, local)))
    case local @ Local(ref) =>
      ref.tryResolve(name => Spec.findLocal(name, ctx).getOrElse(throw NoSuchNameError("local", name, local)))

    case deref @ CStructAccess(obj, field) =>
      deref.ref = Some(C.findDeref(obj, field, ctx, deref.blame).getOrElse(throw NoSuchNameError("field", field, deref)))
    case deref @ JavaDeref(obj, field) =>
      deref.ref = Some(Java.findDeref(obj, field, ctx, deref.blame).getOrElse(throw NoSuchNameError("field", field, deref)))
    case deref @ PVLDeref(obj, field) =>
      deref.ref = Some(PVL.findDeref(obj, field, ctx, deref.blame).getOrElse(throw NoSuchNameError("field", field, deref)))

    case inv @ CInvocation(obj, _, _, _) =>
      inv.ref = Some(C.resolveInvocation(obj, ctx))
    case inv @ GpgpuCudaKernelInvocation(name, blocks, threads, args, _, _) =>
      val kernel = C.findCName(name, ctx).getOrElse(throw NoSuchNameError("kernel", name, inv))
      inv.ref = Some(kernel match {
        case target: CInvocationTarget[G] => target
        case _ => throw NotApplicable(inv)
      })
    case inv @ JavaInvocation(obj, _, method, args, _, _) =>
      inv.ref = Some((obj match {
        case Some(obj) => Java.findMethod(obj, method, args, inv.blame)
        case None => Java.findMethod(ctx, method, args)
      }).getOrElse(throw NoSuchNameError("method", method, inv)))
    case inv @ PVLInvocation(None, method, args, typeArgs, _, _) =>
      inv.ref = Some(PVL.findMethod(method, args, typeArgs, ctx).getOrElse(throw NoSuchNameError("method", method, inv)))
    case inv @ PVLInvocation(Some(obj), method, args, typeArgs, _, _) =>
      inv.ref = Some(PVL.findInstanceMethod(obj, method, args, typeArgs, inv.blame).getOrElse(throw NoSuchNameError("method", method, inv)))
    case inv @ ADTFunctionInvocation(typeArgs, ref, args) =>
      typeArgs match {
        case Some((adt, typeArgs)) =>
          // Fully-qualified external invocation
          adt.tryResolve(name => Spec.findAdt(ctx, name).getOrElse(throw NoSuchNameError("adt", name, inv)))
          ref.tryResolve(name => Spec.findAdtFunction(adt.decl, name).getOrElse(throw NoSuchNameError("function", name, inv)))
        case None =>
          // Non-qualified internal invocation
          ???
      }

    case goto @ CGoto(name) =>
      goto.ref = Some(Spec.findLabel(name, ctx).getOrElse(throw NoSuchNameError("label", name, goto)))
    case goto @ Goto(lbl) =>
      lbl.tryResolve(name => Spec.findLabel(name, ctx).getOrElse(throw NoSuchNameError("label", name, goto)))
    case send @ Send(_, lbl, _) =>
      lbl.tryResolve(name => Spec.findLabel(name, ctx).getOrElse(throw NoSuchNameError("label", name, send)))
    case recv @ Recv(_, lbl, _) =>
      lbl.tryResolve(name => Spec.findLabel(name, ctx).getOrElse(throw NoSuchNameError("label", name, recv)))
    case brk @ Break(Some(lbl)) =>
      lbl.tryResolve(name => Spec.findLabel(name, ctx).getOrElse(throw NoSuchNameError("label", name, brk)))
    case cont @ Continue(Some(lbl)) =>
      lbl.tryResolve(name => Spec.findLabel(name, ctx).getOrElse(throw NoSuchNameError("label", name, cont)))

    case res @ AmbiguousResult() =>
      res.ref = Some(ctx.currentResult.getOrElse(throw ResultOutsideMethod(res)))
    case diz @ AmbiguousThis() =>
      diz.ref = Some(ctx.currentThis.get)

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