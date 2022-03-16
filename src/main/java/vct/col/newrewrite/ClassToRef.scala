package vct.col.newrewrite

import vct.col.ast._
import vct.col.origin._
import vct.col.util.AstBuildHelpers._
import hre.util.ScopedStack
import vct.col.newrewrite.error.{ExcludedByPassOrder, ExtraNode}
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.SuccessionMap
import RewriteHelpers._

import scala.collection.mutable

case object ClassToRef extends RewriterBuilder {
  override def key: String = "classToRef"
  override def desc: String = "Flatten classes into the Ref type, and encode the class type hierarchy operationally."

  case object TypeOfOrigin extends Origin {
    override def preferredName: String = "type"
    override def context: String =
      "[At function type]"
  }

  case object InstanceOfOrigin extends Origin {
    override def preferredName: String = "subtype"
    override def context: String =
      "[At function subtype]"
  }

  case class InstanceNullPreconditionFailed(inner: Blame[InstanceNull], inv: InvokingNode[_]) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      inner.blame(InstanceNull(inv))
  }
}

case class ClassToRef[Pre <: Generation]() extends Rewriter[Pre] {
  import vct.col.newrewrite.ClassToRef._

  case object This extends Origin {
    override def preferredName: String = "this"
    override def context: String =
      "[At generated parameter for 'this']"
  }

  val fieldSucc: SuccessionMap[Field[Pre], SilverField[Post]] = SuccessionMap()

  val diz: ScopedStack[Variable[Post]] = ScopedStack()

  var typeNumberStore: mutable.Map[Class[Pre], Int] = mutable.Map()
  val typeOf: SuccessionMap[Unit, Function[Post]] = SuccessionMap()
  val instanceOf: SuccessionMap[Unit, Function[Post]] = SuccessionMap()

  def typeNumber(cls: Class[Pre]): Int =
    typeNumberStore.getOrElseUpdate(cls, typeNumberStore.size + 1)

  def makeTypeOf: Function[Post] = {
    implicit val o: Origin = TypeOfOrigin
    val obj = new Variable[Post](TRef())
    withResult((result: Result[Post]) => function(
      blame = AbstractApplicable,
      returnType = TInt(),
      args = Seq(obj),
      ensures = UnitAccountedPredicate(
        (result >= const(0) && result <= const(typeNumberStore.size)) &&
          ((obj.get === Null()) ==> (result === const(0))) &&
          ((obj.get !== Null()) ==> (result !== const(0)))
      ),
    ))
  }

  def makeInstanceOf: Function[Post] = {
    implicit val o: Origin = InstanceOfOrigin
    val sub = new Variable[Post](TInt())
    val sup = new Variable[Post](TInt())
    function(
      blame = PanicBlame("instanceof has no postcondition."),
      returnType = TBool(),
      args = Seq(sub, sup),
      requires = UnitAccountedPredicate(
        sub.get >= const(0) && sub.get <= const(typeNumberStore.size) && sup.get >= const(0) && sup.get <= const(typeNumberStore.size)
      ),
      body = Some(
        ((sub.get === const(0)) ==> tt) &&
          foldAnd(typeNumberStore.map {
            case (cls, subNum) =>
              val supNums = (cls +: cls.transSupportArrows.map(_._2)).distinct.map(typeNumber)
              (sub.get === const(subNum)) ==> foldOr(supNums.map(supNum => sup.get === const(supNum)))
          }.toSeq)
      ),
    )
  }

  override def dispatch(program: Program[Pre]): Program[Rewritten[Pre]] =
    program.rewrite(declarations = collectInScope(globalScopes) {
      program.declarations.foreach(dispatch)
      implicit val o: Origin = TypeOfOrigin
      typeOf(()) = makeTypeOf
      typeOf(()).declareDefault(this)
      instanceOf(()) = makeInstanceOf
      instanceOf(()).declareDefault(this)
    })

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case cls: Class[Pre] =>
      typeNumber(cls)
      cls.drop()
      cls.declarations.foreach {
        case function: InstanceFunction[Pre] =>
          implicit val o: Origin = function.o
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
              contract = function.contract.rewrite(
                requires = SplitAccountedPredicate(
                  left = UnitAccountedPredicate(thisVar.get !== Null()),
                  right = SplitAccountedPredicate(
                    left = UnitAccountedPredicate(
                      /* FIXME FunctionInvocation[Post](instanceOf.ref(()), Seq(
                        FunctionInvocation[Post](typeOf.ref(()), Seq(thisVar.get), Nil, Nil, Nil)(PanicBlame("typeOf requires nothing.")),
                        const(typeNumber(cls)),
                      ), Nil, Nil, Nil)(PanicBlame("instanceOf requires nothing.")) */ tt[Post]
                    ),
                    right = dispatch(function.contract.requires),
                  ),
                )
              ),
              inline = function.inline,
            )(function.blame)(function.o).succeedDefault(function)
          }
        case method: InstanceMethod[Pre] =>
          implicit val o: Origin = method.o
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
              contract = method.contract.rewrite(
                requires = SplitAccountedPredicate(
                  left = UnitAccountedPredicate(thisVar.get !== Null()),
                  right = SplitAccountedPredicate(
                    left = UnitAccountedPredicate(
                      /* FIXME FunctionInvocation[Post](instanceOf.ref(()), Seq(
                        FunctionInvocation[Post](typeOf.ref(()), Seq(thisVar.get), Nil, Nil, Nil)(PanicBlame("typeOf requires nothing.")),
                        const(typeNumber(cls)),
                      ), Nil, Nil, Nil)(PanicBlame("instanceOf requires nothing.")) */ tt[Post]
                    ),
                    right = dispatch(method.contract.requires),
                  ),
                ),
              ),
              inline = method.inline,
              pure = method.pure,
            )(method.blame)(method.o).succeedDefault(method)
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
            )(predicate.o).succeedDefault(predicate)
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
      implicit val o: Origin = stat.o
      Block(Seq(
        SilverNewRef[Post](succ(v), cls.declarations.collect { case field: InstanceField[Pre] => fieldSucc.ref(field) }),
        Inhale(FunctionInvocation[Post](typeOf.ref(()), Seq(Local(succ(v))), Nil, Nil, Nil)(PanicBlame("typeOf requires nothing.")) === const(typeNumber(cls))),
      ))
    case inv @ InvokeMethod(obj, Ref(method), args, outArgs, typeArgs, givenMap, yields) =>
      InvokeProcedure[Post](
        ref = succ(method),
        args = dispatch(obj) +: args.map(dispatch),
        outArgs = outArgs.map(succ[Variable[Post]]),
        typeArgs = typeArgs.map(dispatch),
        givenMap = givenMap.map { case (Ref(v), e) => (succ(v), dispatch(e)) },
        yields = yields.map { case (Ref(e), Ref(v)) => (succ(e), succ(v)) },
      )(PreBlameSplit.left(InstanceNullPreconditionFailed(inv.blame, inv), inv.blame))(inv.o)
    case fold @ Fold(inv: InstancePredicateApply[Pre]) =>
      Fold(rewriteInstancePredicateApply(inv))(fold.blame)(fold.o)
    case unfold @ Unfold(inv: InstancePredicateApply[Pre]) =>
      Unfold(rewriteInstancePredicateApply(inv))(unfold.blame)(unfold.o)
    case other => rewriteDefault(other)
  }

  def rewriteInstancePredicateApply(inv: InstancePredicateApply[Pre]): PredicateApply[Post] =
    PredicateApply[Post](succ(inv.ref), freshSuccessionScope { dispatch(inv.obj) } +: inv.args.map(dispatch), dispatch(inv.perm))(inv.o)

  override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case Unfolding(inv: InstancePredicateApply[Pre], e) =>
      Unfolding(rewriteInstancePredicateApply(inv), dispatch(e))(e.o)
    case inv @ MethodInvocation(obj, Ref(method), args, outArgs, typeArgs, givenMap, yields) =>
      ProcedureInvocation[Post](
        ref = succ(method),
        args = dispatch(obj) +: args.map(dispatch),
        outArgs = outArgs.map(succ[Variable[Post]]),
        typeArgs = typeArgs.map(dispatch),
        givenMap = givenMap.map { case (Ref(v), e) => (succ(v), dispatch(e)) },
        yields = yields.map { case (Ref(e), Ref(v)) => (succ(e), succ(v)) },
      )(PreBlameSplit.left(InstanceNullPreconditionFailed(inv.blame, inv), PreBlameSplit.left(PanicBlame("incorrect instance method type?"), inv.blame)))(inv.o)
    case inv @ InstancePredicateApply(obj, Ref(pred), args, perm) =>
      implicit val o: Origin = inv.o
      rewriteInstancePredicateApply(inv) &* freshSuccessionScope { dispatch(obj) !== Null() }
    case inv @ InstanceFunctionInvocation(obj, Ref(func), args, typeArgs, givenMap, yields) =>
      FunctionInvocation[Post](
        ref = succ(func),
        args = dispatch(obj) +: args.map(dispatch),
        typeArgs.map(dispatch),
        givenMap = givenMap.map { case (Ref(v), e) => (succ(v), dispatch(e)) },
        yields = yields.map { case (Ref(e), Ref(v)) => (succ(e), succ(v)) },
      )(PreBlameSplit.left(InstanceNullPreconditionFailed(inv.blame, inv), PreBlameSplit.left(PanicBlame("incorrect instance function type?"), inv.blame)))(inv.o)
    case ThisObject(_) =>
      Local[Post](diz.top.ref)(e.o)
    case deref @ Deref(obj, Ref(field)) =>
      SilverDeref[Post](dispatch(obj), fieldSucc.ref(field))(deref.blame)(deref.o)
    case NewObject(_) => ???
    case TypeValue(t) => t match {
      case TClass(Ref(cls)) => const(typeNumber(cls))(e.o)
      case other => ???
    }
    case TypeOf(value) => FunctionInvocation[Post](typeOf.ref(()), Seq(dispatch(value)), Nil, Nil, Nil)(PanicBlame("typeOf requires nothing"))(e.o)
    case InstanceOf(value, TypeValue(TUnion(ts))) =>
      implicit val o: Origin = e.o
      dispatch(foldOr(ts.map(t => InstanceOf(value, TypeValue(t)))))
    case InstanceOf(value, typeValue) => FunctionInvocation[Post](instanceOf.ref(()), Seq(
      FunctionInvocation[Post](typeOf.ref(()), Seq(dispatch(value)), Nil, Nil, Nil)(PanicBlame("typeOf requires nothing"))(e.o),
      dispatch(typeValue),
    ), Nil, Nil, Nil)(PanicBlame("instanceOf requires nothing"))(e.o)
    case Cast(value, typeValue) => dispatch(value) // Discard for now, should assert instanceOf(value, typeValue)
    case _ => rewriteDefault(e)
  }

  override def dispatch(t: Type[Pre]): Type[Post] = t match {
    case TClass(_) => TRef()
    case t => rewriteDefault(t)
  }
}