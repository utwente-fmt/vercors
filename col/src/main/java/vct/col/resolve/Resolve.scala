package vct.col.resolve

import hre.util.FuncTools
import vct.col.ast._
import vct.col.ast.temporaryimplpackage.util.Declarator
import vct.col.check.CheckError
import vct.col.origin._

case object Resolve {
  def resolve(program: Program[_], externalJavaLoader: Option[ExternalJavaLoader] = None): Seq[CheckError] = {
    ResolveTypes.resolve(program, externalJavaLoader)
    ResolveReferences.resolve(program)
  }
}

case object ResolveTypes {
  def resolve[G](program: Program[G], externalJavaLoader: Option[ExternalJavaLoader] = None): Seq[GlobalDeclaration[G]] = {
    val ctx = TypeResolutionContext[G](externalJavaLoader = externalJavaLoader)
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
      ctx.copy(stack=decls.flatMap(Referrable.from) +: ctx.stack)
    case ns: JavaNamespace[G] =>
      ctx.copy(stack=ns.declarations.flatMap(Referrable.from) +: ctx.stack, namespace=Some(ns))
    case decl: Declarator[G] =>
      ctx.copy(stack=decl.declarations.flatMap(Referrable.from) +: ctx.stack)
    case _ => ctx
  }

  def resolveOne[G](node: Node[G], ctx: TypeResolutionContext[G]): Unit = node match {
    case javaClass @ JavaNamedType(genericNames) =>
      val names = genericNames.map(_._1)
      javaClass.ref = Some(Java.findJavaTypeName(names, ctx).getOrElse(
        throw NoSuchNameError("class", names.mkString("."), javaClass)))
    case t @ JavaTClass(ref, _) =>
      ref.tryResolve(name => ???)
    case t @ CTypedefName(name) =>
      t.ref = Some(C.findCTypeName(name, ctx).getOrElse(
        throw NoSuchNameError("struct", name, t)
      ))
    case t @ PVLNamedType(name, typeArgs) =>
      t.ref = Some(PVL.findTypeName(name, ctx).getOrElse(
        throw NoSuchNameError("class", name, t)))
    case t @ TModel(ref) =>
      ref.tryResolve(name => Spec.findModel(name, ctx).getOrElse(throw NoSuchNameError("model", name, t)))
    case t @ TClass(ref) =>
      ref.tryResolve(name => Spec.findClass(name, ctx).getOrElse(throw NoSuchNameError("class", name, t)))
    case t @ TAxiomatic(ref, _) =>
      ref.tryResolve(name => Spec.findAdt(name, ctx).getOrElse(throw NoSuchNameError("adt", name, t)))
    case t @ SilverPartialTAxiomatic(ref, partialTypeArgs) =>
      ref.tryResolve(name => Spec.findAdt(name, ctx).getOrElse(throw NoSuchNameError("adt", name, t)))
      partialTypeArgs.foreach(mapping => mapping._1.tryResolve(name => Spec.findAdtTypeArg(ref.decl, name).getOrElse(throw NoSuchNameError("type variable", name, t))))
    case cls: Class[G] =>
      // PB: needs to be in ResolveTypes if we want to support method inheritance at some point.
      cls.supports.foreach(_.tryResolve(name => Spec.findClass(name, ctx).getOrElse(throw NoSuchNameError("class", name, cls))))
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

  def scanLabels[G](node: Node[G]): Seq[Declaration[G]] = node.transSubnodes.collect {
    case decl: LabelDecl[G] => decl
    case decl: SendDecl[G] => decl
  }

  def scanBlocks[G](node: ParRegion[G]): Seq[ParBlock[G]] = node match {
    case ParParallel(regions) => regions.flatMap(scanBlocks)
    case ParSequential(regions) => regions.flatMap(scanBlocks)
    case block: ParBlock[G] => Seq(block)
  }

  def enterContext[G](node: Node[G], ctx: ReferenceResolutionContext[G]): ReferenceResolutionContext[G] = (node match {
    case ns: JavaNamespace[G] => ctx
      .copy(currentJavaNamespace=Some(ns)).declare(ns.declarations)
    case cls: JavaClassOrInterface[G] => ctx
      .copy(currentJavaClass=Some(cls))
      .copy(currentThis=Some(RefJavaClass(cls)))
      .declare(cls.decls)
    case cls: Class[G] => ctx
      .copy(currentThis=Some(RefClass(cls)))
      .declare(cls.declarations)
    case app: ContractApplicable[G] => ctx
      .copy(currentResult=Some(Referrable.from(app).head.asInstanceOf[ResultTarget[G]] /* PB TODO: ew */))
      .declare(app.declarations ++ app.body.map(scanLabels).getOrElse(Nil))
    case method: JavaMethod[G] => ctx
      .copy(currentResult=Some(RefJavaMethod(method)))
      .declare(method.declarations ++ method.body.map(scanLabels).getOrElse(Nil))
    case fields: JavaFields[G] => ctx
      .copy(currentInitializerType=Some(fields.t))
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
    case func: CFunctionDefinition[G] => ctx
      .copy(currentResult=Some(RefCFunctionDefinition(func)))
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
  }).copy(checkContext=node.enterCheckContext(ctx.checkContext))

  def resolveFlatly[G](node: Node[G], ctx: ReferenceResolutionContext[G]): Unit = node match {
    case local @ CLocal(name) =>
      local.ref = Some(C.findCName(name, ctx).getOrElse(throw NoSuchNameError("local", name, local)))
    case local @ JavaLocal(name) =>
      local.ref = Some(Java.findJavaName(name, ctx).getOrElse(throw NoSuchNameError("local", name, local)))
    case local @ PVLLocal(name) =>
      local.ref = Some(PVL.findName(name, ctx).getOrElse(throw NoSuchNameError("local", name, local)))
    case local @ Local(ref) =>
      ref.tryResolve(name => Spec.findLocal(name, ctx).getOrElse(throw NoSuchNameError("local", name, local)))
    case local @ TVar(ref) =>
      ref.tryResolve(name => Spec.findLocal(name, ctx).getOrElse(throw NoSuchNameError("type variable", name, local)))
    case funcOf @ FunctionOf(v, vars) =>
      v.tryResolve(name => Spec.findLocal(name, ctx).getOrElse(throw NoSuchNameError("local", name, funcOf)))
      vars.foreach(v => v.tryResolve(name => Spec.findLocal(name, ctx).getOrElse(throw NoSuchNameError("local", name, funcOf))))
    case local @ SilverLocalAssign(ref, _) =>
      ref.tryResolve(name => Spec.findLocal(name, ctx).getOrElse(throw NoSuchNameError("local", name, local)))

    case deref @ CStructAccess(obj, field) =>
      deref.ref = Some(C.findDeref(obj, field, ctx, deref.blame).getOrElse(throw NoSuchNameError("field", field, deref)))
    case deref @ JavaDeref(obj, field) =>
      deref.ref = Some(Java.findDeref(obj, field, ctx, deref.blame).getOrElse(throw NoSuchNameError("field", field, deref)))
    case deref @ PVLDeref(obj, field) =>
      deref.ref = Some(PVL.findDeref(obj, field, ctx, deref.blame).getOrElse(throw NoSuchNameError("field", field, deref)))
    case deref @ Deref(obj, field) =>
      field.tryResolve(name => Spec.findField(obj, name).getOrElse(throw NoSuchNameError("field", name, deref)))
    case deref @ ModelDeref(obj, field) =>
      field.tryResolve(name => Spec.findModelField(obj, name).getOrElse(throw NoSuchNameError("field", name, deref)))
    case deref @ SilverDeref(_, field) =>
      field.tryResolve(name => Spec.findSilverField(name, ctx).getOrElse(throw NoSuchNameError("field", name, deref)))
    case deref @ SilverCurFieldPerm(_, field) =>
      field.tryResolve(name => Spec.findSilverField(name, ctx).getOrElse(throw NoSuchNameError("field", name, deref)))
    case deref @ SilverFieldAssign(_, field, _) =>
      field.tryResolve(name => Spec.findSilverField(name, ctx).getOrElse(throw NoSuchNameError("field", name, deref)))

    case inv @ CInvocation(obj, _, givenMap, yields) =>
      inv.ref = Some(C.resolveInvocation(obj, ctx))
      Spec.resolveGiven(givenMap, inv.ref.get, inv)
      Spec.resolveYields(ctx, yields, inv.ref.get, inv)
    case inv @ GpgpuCudaKernelInvocation(name, blocks, threads, args, givenMap, yields) =>
      val kernel = C.findCName(name, ctx).getOrElse(throw NoSuchNameError("kernel", name, inv))
      inv.ref = Some(kernel match {
        case target: CInvocationTarget[G] => target
        case _ => throw NotApplicable(inv)
      })
      Spec.resolveGiven(givenMap, inv.ref.get, inv)
      Spec.resolveYields(ctx, yields, inv.ref.get, inv)
    case inv @ JavaInvocation(obj, _, method, args, givenMap, yields) =>
      inv.ref = Some((obj match {
        case Some(obj) => Java.findMethod(obj, method, args, inv.blame)
        case None => Java.findMethod(ctx, method, args)
      }).getOrElse(throw NoSuchNameError("method", method, inv)))
      Spec.resolveGiven(givenMap, inv.ref.get, inv)
      Spec.resolveYields(ctx, yields, inv.ref.get, inv)
    case inv @ JavaNewClass(args, typeArgs, name, givenMap, yields) =>
      inv.ref = Some(Java.findConstructor(name, args).getOrElse(throw NoSuchConstructor(inv)))
      Spec.resolveGiven(givenMap, inv.ref.get, inv)
      Spec.resolveYields(ctx, yields, inv.ref.get, inv)
    case inv @ PVLInvocation(None, method, args, typeArgs, givenMap, yields) =>
      inv.ref = Some(PVL.findMethod(method, args, typeArgs, ctx).getOrElse(throw NoSuchNameError("method", method, inv)))
      Spec.resolveGiven(givenMap, inv.ref.get, inv)
      Spec.resolveYields(ctx, yields, inv.ref.get, inv)
    case inv @ PVLInvocation(Some(obj), method, args, typeArgs, givenMap, yields) =>
      inv.ref = Some(PVL.findInstanceMethod(obj, method, args, typeArgs, inv.blame).getOrElse(throw NoSuchNameError("method", method, inv)))
      Spec.resolveGiven(givenMap, inv.ref.get, inv)
      Spec.resolveYields(ctx, yields, inv.ref.get, inv)
    case inv @ PVLNew(t, args, givenMap, yields) =>
      inv.ref = Some(PVL.findConstructor(t, args).getOrElse(throw NoSuchConstructor(inv)))
      Spec.resolveGiven(givenMap, inv.ref.get, inv)
      Spec.resolveYields(ctx, yields, inv.ref.get, inv)
    case n @ NewObject(ref) =>
      ref.tryResolve(name => Spec.findClass(name, ctx.asTypeResolutionContext).getOrElse(throw NoSuchNameError("class", name, n)))
    case n @ ModelNew(ref) =>
      ref.tryResolve(name => Spec.findModel(name, ctx.asTypeResolutionContext).getOrElse(throw NoSuchNameError("model", name, n)))
    case n @ SilverNewRef(target, fields) =>
      target.tryResolve(name => Spec.findLocal(name, ctx).getOrElse(throw NoSuchNameError("local", name, n)))
      fields.foreach(_.tryResolve(name => Spec.findSilverField(name, ctx).getOrElse(throw NoSuchNameError("field", name, n))))
    case inv @ ADTFunctionInvocation(typeArgs, ref, args) =>
      typeArgs match {
        case Some((adt, typeArgs)) =>
          // Fully-qualified external invocation
          adt.tryResolve(name => Spec.findAdt(name, ctx.asTypeResolutionContext).getOrElse(throw NoSuchNameError("adt", name, inv)))
          ref.tryResolve(name => Spec.findAdtFunction(adt.decl, name).getOrElse(throw NoSuchNameError("function", name, inv)))
        case None =>
          // Non-qualified internal invocation
          ???
      }
    case inv @ SilverPartialADTFunctionInvocation(name, args, partialTypeArgs) =>
      inv.ref = Some(Spec.findAdtFunction(name, ctx).getOrElse(throw NoSuchNameError("function", name, inv)))
      partialTypeArgs.foreach(mapping => mapping._1.tryResolve(name => Spec.findAdtTypeArg(inv.adt, name).getOrElse(throw NoSuchNameError("type variable", name, inv))))
    case inv @ InvokeProcedure(ref, _, outArgs, _, givenMap, yields) =>
      ref.tryResolve(name => Spec.findProcedure(name, ctx).getOrElse(throw NoSuchNameError("procedure", name, inv)))
      outArgs.foreach(ref => ref.tryResolve(name => Spec.findLocal(name, ctx).getOrElse(throw NoSuchNameError("local", name, inv))))
      Spec.resolveGiven(givenMap, RefProcedure(ref.decl), inv)
      Spec.resolveYields(ctx, yields, RefProcedure(ref.decl), inv)
    case inv @ ProcedureInvocation(ref, _, outArgs, _, givenMap, yields) =>
      ref.tryResolve(name => Spec.findProcedure(name, ctx).getOrElse(throw NoSuchNameError("procedure", name, inv)))
      outArgs.foreach(ref => ref.tryResolve(name => Spec.findLocal(name, ctx).getOrElse(throw NoSuchNameError("local", name, inv))))
      Spec.resolveGiven(givenMap, RefProcedure(ref.decl), inv)
      Spec.resolveYields(ctx, yields, RefProcedure(ref.decl), inv)
    case inv @ FunctionInvocation(ref, _, _, givenMap, yields) =>
      ref.tryResolve(name => Spec.findFunction(name, ctx).getOrElse(throw NoSuchNameError("function", name, inv)))
      Spec.resolveGiven(givenMap, RefFunction(ref.decl), inv)
      Spec.resolveYields(ctx, yields, RefFunction(ref.decl), inv)
    case inv @ PredicateApply(ref, _, _) =>
      ref.tryResolve(name => Spec.findPredicate(name, ctx).getOrElse(throw NoSuchNameError("predicate", name, inv)))
    case inv @ SilverCurPredPerm(ref, _) =>
      ref.tryResolve(name => Spec.findPredicate(name, ctx).getOrElse(throw NoSuchNameError("predicate", name, inv)))
    case inv @ InvokeMethod(obj, ref, _, outArgs, _, givenMap, yields) =>
      ref.tryResolve(name => Spec.findMethod(obj, name).getOrElse(throw NoSuchNameError("method", name, inv)))
      outArgs.foreach(ref => ref.tryResolve(name => Spec.findLocal(name, ctx).getOrElse(throw NoSuchNameError("local", name, inv))))
      Spec.resolveGiven(givenMap, RefInstanceMethod(ref.decl), inv)
      Spec.resolveYields(ctx, yields, RefInstanceMethod(ref.decl), inv)
    case inv @ MethodInvocation(obj, ref, _, outArgs, _, givenMap, yields) =>
      ref.tryResolve(name => Spec.findMethod(obj, name).getOrElse(throw NoSuchNameError("method", name, inv)))
      outArgs.foreach(ref => ref.tryResolve(name => Spec.findLocal(name, ctx).getOrElse(throw NoSuchNameError("local", name, inv))))
      Spec.resolveGiven(givenMap, RefInstanceMethod(ref.decl), inv)
      Spec.resolveYields(ctx, yields, RefInstanceMethod(ref.decl), inv)
    case inv @ InstanceFunctionInvocation(obj, ref, _, _, givenMap, yields) =>
      ref.tryResolve(name => Spec.findInstanceFunction(obj, name).getOrElse(throw NoSuchNameError("function", name, inv)))
    case inv @ InstancePredicateApply(obj, ref, _, _) =>
      ref.tryResolve(name => Spec.findInstancePredicate(obj, name).getOrElse(throw NoSuchNameError("predicate", name, inv)))

    case goto @ CGoto(name) =>
      goto.ref = Some(Spec.findLabel(name, ctx).getOrElse(throw NoSuchNameError("label", name, goto)))
    case goto @ Goto(lbl) =>
      lbl.tryResolve(name => Spec.findLabel(name, ctx).getOrElse(throw NoSuchNameError("label", name, goto)))
    case brk @ Break(Some(lbl)) =>
      lbl.tryResolve(name => Spec.findLabel(name, ctx).getOrElse(throw NoSuchNameError("label", name, brk)))
    case cont @ Continue(Some(lbl)) =>
      lbl.tryResolve(name => Spec.findLabel(name, ctx).getOrElse(throw NoSuchNameError("label", name, cont)))
    case old @ Old(_, Some(lbl)) =>
      lbl.tryResolve(name => Spec.findLabel(name, ctx).getOrElse(throw NoSuchNameError("label", name, old)))

    case recv @ Recv(ref) =>
      ref.tryResolve(name => Spec.findSend(name, ctx).getOrElse(throw NoSuchNameError("send statement", name, recv)))

    case res @ AmbiguousResult() =>
      res.ref = Some(ctx.currentResult.getOrElse(throw ResultOutsideMethod(res)))
    case diz @ AmbiguousThis() =>
      // PB: now obsolete?
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

    case inv @ ProcessApply(ref, _) =>
      ref.tryResolve(name => ???)
    case inv @ ActionApply(ref, _) =>
      ref.tryResolve(name => ???)

    case atomic @ ParAtomic(invs, _) =>
      invs.foreach(_.tryResolve(name => Spec.findParInvariant(name, ctx)
        .getOrElse(throw NoSuchNameError("invariant", name, atomic))))
    case barrier @ ParBarrier(block, invs, _, _, _) =>
      block.tryResolve(name => Spec.findParBlock(name, ctx)
        .getOrElse(throw NoSuchNameError("block", name, barrier)))
      invs.foreach(_.tryResolve(name => Spec.findParInvariant(name, ctx)
        .getOrElse(throw NoSuchNameError("invariant", name, barrier))))

    case arr @ JavaLiteralArray(_) =>
      arr.typeContext = Some(ctx.currentInitializerType.get match {
        case t @ TArray(_) => t
        case _ => throw WrongArrayInitializer(arr)
      })

    case _ =>
  }
}