package vct.col.newrewrite

import vct.col.ast._
import AstBuildHelpers._
import Constant._
import hre.util.ScopedStack
import vct.col.ast.util.SuccessionMap
import vct.col.newrewrite.error.ExcludedByPassOrder

case class ClassToRef() extends Rewriter {
  case object This extends Origin {
    override def preferredName: String = "this"
    override def messageInContext(message: String): String =
      s"[At generated parameter for 'this']: $message"
  }

  val functionSucc: SuccessionMap[InstanceFunction, Function] = SuccessionMap()
  val methodSucc: SuccessionMap[InstanceMethod, Procedure] = SuccessionMap()
  val predicateSucc: SuccessionMap[InstancePredicate, Predicate] = SuccessionMap()
  val fieldSucc: SuccessionMap[Field, SilverField] = SuccessionMap()

  val diz: ScopedStack[Variable] = ScopedStack()

  override def dispatch(decl: Declaration): Unit = decl match {
    case cls: Class =>
      cls.declarations.foreach {
        case declaration: ExtraClassDeclaration =>
          throw ExcludedByPassOrder("Extra declarations should be resolved away already by LangSpecificToCol", Some(declaration))
        case function: InstanceFunction =>
          val thisVar = new Variable(TRef())(This)
          diz.having(thisVar) {
            functionSucc(function) = new Function(
              returnType = dispatch(function.returnType),
              args = collectInScope(variableScopes) {
                thisVar.declareDefault(this)
                function.args.foreach(dispatch)
              },
              body = function.body.map(dispatch),
              contract = dispatch(function.contract),
              inline = function.inline,
            )(function.blame)(function.o)
            functionSucc(function).declareDefault(this)
          }
        case method: InstanceMethod =>
          val thisVar = new Variable(TRef())(This)
          diz.having(thisVar) {
            methodSucc(method) = new Procedure(
              returnType = dispatch(method.returnType),
              args = collectInScope(variableScopes) {
                thisVar.declareDefault(this)
                method.args.foreach(dispatch)
              },
              outArgs = collectInScope(variableScopes) { method.outArgs.foreach(dispatch) },
              body = method.body.map(dispatch),
              contract = dispatch(method.contract),
              inline = method.inline,
              pure = method.pure,
            )(method.blame)(method.o)
          }
        case predicate: InstancePredicate =>
          val thisVar = new Variable(TRef())(This)
          diz.having(thisVar) {
            predicateSucc(predicate) = new Predicate(
              args = collectInScope(variableScopes) {
                thisVar.declareDefault(this)
                predicate.args.foreach(dispatch)
              },
              body = predicate.body.map(dispatch),
              threadLocal = predicate.threadLocal,
              inline = predicate.inline,
            )(predicate.o)
          }
        case field: Field =>
          fieldSucc(field) = new SilverField(field.t)(field.o)
          fieldSucc(field).declareDefault(this)
      }
  }

  override def dispatch(e: Expr): Expr = e match {
    case inv @ MethodInvocation(obj, Ref(method), args, outArgs) =>
      ProcedureInvocation(
        ref = methodSucc.ref(method),
        args = dispatch(obj) +: args.map(dispatch),
        outArgs = inv.outArgs.map(r => typedSucc[Variable](r.decl)),
      )(inv.blame)(inv.o)
    case inv @ InstancePredicateApply(obj, Ref(pred), args) =>
      PredicateApply(predicateSucc.ref(pred), dispatch(obj) +: args.map(dispatch))(inv.o)
    case inv @ InstanceFunctionInvocation(obj, Ref(func), args) =>
      FunctionInvocation(functionSucc.ref(func), dispatch(obj) +: args.map(dispatch))(inv.blame)(inv.o)
    case AmbiguousThis() =>
      Local(diz.head.ref)(e.o)
    case deref @ Deref(obj, Ref(field)) =>
      SilverDeref(dispatch(obj), fieldSucc.ref(field))(deref.blame)(deref.o)
    case _ => rewriteDefault(e)
  }

  override def dispatch(t: Type): Type = t match {
    case TClass(_) => TRef()
    case t => rewriteDefault(t)
  }
}