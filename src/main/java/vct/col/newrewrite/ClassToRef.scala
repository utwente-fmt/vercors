package vct.col.newrewrite

import vct.col.ast._
import vct.col.origin._
import vct.col.util.AstBuildHelpers._
import hre.util.ScopedStack
import vct.col.newrewrite.error.{ExcludedByPassOrder, ExtraNode}
import vct.col.ref.Ref
import vct.col.rewrite.Rewriter
import vct.col.util.SuccessionMap

case class ClassToRef() extends Rewriter {
  case object This extends Origin {
    override def preferredName: String = "this"
    override def messageInContext(message: String): String =
      s"[At generated parameter for 'this']: $message"
  }

  val fieldSucc: SuccessionMap[Field, SilverField] = SuccessionMap()

  val diz: ScopedStack[Variable] = ScopedStack()

  override def dispatch(decl: Declaration): Unit = decl match {
    case cls: Class =>
      cls.drop()
      cls.declarations.foreach {
        case function: InstanceFunction =>
          val thisVar = new Variable(TRef())(This)
          diz.having(thisVar) {
            new Function(
              returnType = dispatch(function.returnType),
              args = collectInScope(variableScopes) {
                thisVar.declareDefault(this)
                function.args.foreach(dispatch)
              },
              typeArgs = collectInScope(variableScopes) { function.typeArgs.foreach(dispatch) },
              body = function.body.map(dispatch),
              contract = dispatch(function.contract),
              inline = function.inline,
            )(function.blame)(function.o).succeedDefault(this, function)
          }
        case method: InstanceMethod =>
          val thisVar = new Variable(TRef())(This)
          diz.having(thisVar) {
            new Procedure(
              returnType = dispatch(method.returnType),
              args = collectInScope(variableScopes) {
                thisVar.declareDefault(this)
                method.args.foreach(dispatch)
              },
              outArgs = collectInScope(variableScopes) { method.outArgs.foreach(dispatch) },
              typeArgs = collectInScope(variableScopes) { method.typeArgs.foreach(dispatch) },
              body = method.body.map(dispatch),
              contract = dispatch(method.contract),
              inline = method.inline,
              pure = method.pure,
            )(method.blame)(method.o).succeedDefault(this, method)
          }
        case predicate: InstancePredicate =>
          val thisVar = new Variable(TRef())(This)
          diz.having(thisVar) {
            new Predicate(
              args = collectInScope(variableScopes) {
                thisVar.declareDefault(this)
                predicate.args.foreach(dispatch)
              },
              body = predicate.body.map(dispatch),
              threadLocal = predicate.threadLocal,
              inline = predicate.inline,
            )(predicate.o).succeedDefault(this, predicate)
          }
        case field: Field =>
          fieldSucc(field) = new SilverField(dispatch(field.t))(field.o)
          fieldSucc(field).declareDefault(this)
        case _ =>
          throw ExtraNode
      }
    case decl => rewriteDefault(decl)
  }

  override def dispatch(stat: Statement): Statement = stat match {
    case Assign(Local(Ref(v)), NewObject(Ref(cls))) =>
      SilverNewRef(succ(v), cls.declarations.collect { case field: InstanceField => fieldSucc.ref(field) })(stat.o)
    case other => rewriteDefault(other)
  }

  override def dispatch(e: Expr): Expr = e match {
    case inv @ MethodInvocation(obj, Ref(method), args, outArgs, typeArgs) =>
      ProcedureInvocation(
        ref = succ(method),
        args = dispatch(obj) +: args.map(dispatch),
        outArgs = outArgs.map(r => succ[Variable](r.decl)),
        typeArgs = typeArgs.map(dispatch),
      )(inv.blame)(inv.o)
    case inv @ InstancePredicateApply(obj, Ref(pred), args) =>
      PredicateApply(succ(pred), dispatch(obj) +: args.map(dispatch))(inv.o)
    case inv @ InstanceFunctionInvocation(obj, Ref(func), args, typeArgs) =>
      FunctionInvocation(succ(func), dispatch(obj) +: args.map(dispatch), typeArgs.map(dispatch))(inv.blame)(inv.o)
    case AmbiguousThis() =>
      Local(diz.top.ref)(e.o)
    case deref @ Deref(obj, Ref(field)) =>
      SilverDeref(dispatch(obj), fieldSucc.ref(field))(deref.blame)(deref.o)
    case NewObject(_) => ???
    case _ => rewriteDefault(e)
  }

  override def dispatch(t: Type): Type = t match {
    case TClass(_) => TRef()
    case t => rewriteDefault(t)
  }
}