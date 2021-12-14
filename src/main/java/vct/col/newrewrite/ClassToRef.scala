package vct.col.newrewrite

import vct.col.ast._
import vct.col.origin._
import vct.col.util.AstBuildHelpers._
import hre.util.ScopedStack
import vct.col.newrewrite.error.{ExcludedByPassOrder, ExtraNode}
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.SuccessionMap

case object ClassToRef extends RewriterBuilder

case class ClassToRef[Pre <: Generation]() extends Rewriter[Pre] {
  case object This extends Origin {
    override def preferredName: String = "this"
    override def messageInContext(message: String): String =
      s"[At generated parameter for 'this']: $message"
  }

  val fieldSucc: SuccessionMap[Field[Pre], SilverField[Post]] = SuccessionMap()

  val diz: ScopedStack[Variable[Post]] = ScopedStack()

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case cls: Class[Pre] =>
      cls.drop()
      cls.declarations.foreach {
        case function: InstanceFunction[Pre] =>
          val thisVar = new Variable[Post](TRef())(This)
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
        case method: InstanceMethod[Pre] =>
          val thisVar = new Variable[Post](TRef())(This)
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
        case predicate: InstancePredicate[Pre] =>
          val thisVar = new Variable[Post](TRef())(This)
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
        case field: Field[Pre] =>
          fieldSucc(field) = new SilverField(dispatch(field.t))(field.o)
          fieldSucc(field).declareDefault(this)
        case _ =>
          throw ExtraNode
      }
    case decl => rewriteDefault(decl)
  }

  override def dispatch(stat: Statement[Pre]): Statement[Post] = stat match {
    case Assign(Local(Ref(v)), NewObject(Ref(cls))) =>
      SilverNewRef[Post](succ(v), cls.declarations.collect { case field: InstanceField[Pre] => fieldSucc.ref(field) })(stat.o)
    case other => rewriteDefault(other)
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case inv @ MethodInvocation(obj, Ref(method), args, outArgs, typeArgs) =>
      ProcedureInvocation[Post](
        ref = succ(method),
        args = dispatch(obj) +: args.map(dispatch),
        outArgs = outArgs.map(succ[Variable[Post]]),
        typeArgs = typeArgs.map(dispatch),
      )(inv.blame)(inv.o)
    case inv @ InstancePredicateApply(obj, Ref(pred), args) =>
      PredicateApply[Post](succ(pred), dispatch(obj) +: args.map(dispatch))(inv.o)
    case inv @ InstanceFunctionInvocation(obj, Ref(func), args, typeArgs) =>
      FunctionInvocation[Post](succ(func), dispatch(obj) +: args.map(dispatch), typeArgs.map(dispatch))(inv.blame)(inv.o)
    case ThisObject(_) =>
      Local[Post](diz.top.ref)(e.o)
    case deref @ Deref(obj, Ref(field)) =>
      SilverDeref[Post](dispatch(obj), fieldSucc.ref(field))(deref.blame)(deref.o)
    case NewObject(_) => ???
    case _ => rewriteDefault(e)
  }

  override def dispatch(t: Type[Pre]): Type[Post] = t match {
    case TClass(_) => TRef()
    case t => rewriteDefault(t)
  }
}