package vct.col.ast

object ResolveReferences {

  def resolveFlatly(): Unit = {}
//    case process: ModelProcess =>
//      (process.modifies ++ process.accessible).foreach(_.tryResolve(Refferino.findModelField(ctx, _, process)))
//    case action: ModelAction =>
//      (action.modifies ++ action.accessible).foreach(_.tryResolve(Refferino.findModelField(ctx, _, action)))
//
//    case deref @ Deref(obj, ref) =>
//      obj.t match {
//        case TClass(cls) => ref.tryResolve(name => cls.decl.declarations.collectFirst {
//          case field: InstanceField if RefDeclaration(field).name == name => field
//        } match {
//          case Some(field) => field
//          case None => throw NoSuchName(name, deref)
//        })
//      }
//
//    case inv @ ProcedureInvocation(ref, _, _) => ref.tryResolve(Refferino.findApplicable(ctx, _, inv))
//    case inv @ MethodInvocation(_, ref, _, _) => ref.tryResolve(Refferino.findApplicable(ctx, _, inv))
//    case inv @ FunctionInvocation(ref, _) => ref.tryResolve(Refferino.findApplicable(ctx, _, inv))
//    case inv @ InstanceFunctionInvocation(_, ref, _) => ref.tryResolve(Refferino.findApplicable(ctx, _, inv))
//    case inv @ ActionApply(ref, _) => ref.tryResolve(Refferino.findApplicable(ctx, _, inv))
//    case inv @ ProcessApply(ref, _) => ref.tryResolve(Refferino.findApplicable(ctx, _, inv))
//
//    case e @ NewObject(ref) => ref.tryResolve(Refferino.findClass(ctx, _, e))
//    case old @ Old(_, Some(ref)) => ref.tryResolve(Refferino.findLabel(ctx, _, old))
//
//    case atomic @ ParAtomic(ref, _) => ref.tryResolve(Refferino.findParInvariant(ctx, _, atomic))
//    case barrier @ ParBarrier(ref, _, _, _, _) => ref.tryResolve(Refferino.findParBlock(ctx, _, barrier))
//    case block @ ParBlock(_, after, _, _, _, _) => after.foreach(_.tryResolve(Refferino.findParBlock(ctx, _, block)))
//    case create @ ModelCreate(_, ref, _) => ref.tryResolve(Refferino.findModel(ctx, _, create))
//    case break @ Break(Some(ref)) => ref.tryResolve(Refferino.findLabel(ctx, _, break))
//    case continue @ Continue(Some(ref)) => ref.tryResolve(Refferino.findLabel(ctx, _, continue))

}