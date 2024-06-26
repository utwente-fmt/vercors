package vct.col.rewrite

import vct.col.ast._
import vct.col.origin._
import vct.col.util.AstBuildHelpers._
import hre.util.ScopedStack
import vct.col.rewrite.error.{ExcludedByPassOrder, ExtraNode}
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.SuccessionMap
import RewriteHelpers._

import scala.collection.mutable

case object ClassToRef extends RewriterBuilder {
  override def key: String = "classToRef"
  override def desc: String =
    "Flatten classes into the Ref type, and encode the class type hierarchy operationally."

  private def TypeOfOrigin: Origin =
    Origin(Seq(PreferredName(Seq("type")), LabelContext("classToRef")))

  private def InstanceOfOrigin: Origin =
    Origin(Seq(PreferredName(Seq("subtype")), LabelContext("classToRef")))

  case class InstanceNullPreconditionFailed(
      inner: Blame[InstanceNull],
      inv: InvokingNode[_],
  ) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      inner.blame(InstanceNull(inv))
  }
}

case class ClassToRef[Pre <: Generation]() extends Rewriter[Pre] {
  import vct.col.rewrite.ClassToRef._

  private def This: Origin =
    Origin(Seq(PreferredName(Seq("this")), LabelContext("classToRef")))

  val fieldSucc: SuccessionMap[Field[Pre], SilverField[Post]] = SuccessionMap()
  val methodSucc: SuccessionMap[InstanceMethod[Pre], Procedure[Post]] =
    SuccessionMap()
  val consSucc: SuccessionMap[Constructor[Pre], Procedure[Post]] =
    SuccessionMap()
  val functionSucc: SuccessionMap[InstanceFunction[Pre], Function[Post]] =
    SuccessionMap()
  val predicateSucc: SuccessionMap[InstancePredicate[Pre], Predicate[Post]] =
    SuccessionMap()

  val diz: ScopedStack[Expr[Post]] = ScopedStack()

  var typeNumberStore: mutable.Map[Class[Pre], Int] = mutable.Map()
  val typeOf: SuccessionMap[Unit, Function[Post]] = SuccessionMap()
  val instanceOf: SuccessionMap[Unit, Function[Post]] = SuccessionMap()

  def typeNumber(cls: Class[Pre]): Int =
    typeNumberStore.getOrElseUpdate(cls, typeNumberStore.size + 1)

  def makeTypeOf: Function[Post] = {
    implicit val o: Origin = TypeOfOrigin
    val obj = new Variable[Post](TRef())
    withResult((result: Result[Post]) =>
      function(
        blame = AbstractApplicable,
        contractBlame = TrueSatisfiable,
        returnType = TInt(),
        args = Seq(obj),
        ensures = UnitAccountedPredicate(
          (result >= const(0) && result <= const(typeNumberStore.size)) &&
            ((obj.get === Null()) ==> (result === const(0))) &&
            ((obj.get !== Null()) ==> (result !== const(0)))
        ),
      )
    )
  }

  def makeInstanceOf: Function[Post] = {
    implicit val o: Origin = InstanceOfOrigin
    val sub = new Variable[Post](TInt())
    val sup = new Variable[Post](TInt())
    function(
      blame = PanicBlame("instanceof has no postcondition."),
      contractBlame = UnsafeDontCare
        .Satisfiability("that just indicates there are no types"),
      returnType = TBool(),
      args = Seq(sub, sup),
      requires = UnitAccountedPredicate(
        sub.get >= const(0) && sub.get <= const(typeNumberStore.size) &&
          sup.get >= const(0) && sup.get <= const(typeNumberStore.size)
      ),
      body = Some(
        ((sub.get === const(0)) ==> tt) &&
          foldAnd(typeNumberStore.map { case (cls, subNum) =>
            val supNums = (cls +: cls.transSupportArrows.map(_._2.cls.decl))
              .distinct.map(typeNumber)
            (sub.get === const(subNum)) ==>
              foldOr(supNums.map(supNum => sup.get === const(supNum)))
          }.toSeq)
      ),
    )
  }

  override def dispatch(program: Program[Pre]): Program[Rewritten[Pre]] =
    program.rewrite(declarations =
      globalDeclarations.collect {
        program.declarations.foreach(dispatch)
        implicit val o: Origin = TypeOfOrigin
        typeOf(()) = makeTypeOf
        globalDeclarations.declare(typeOf(()))
        instanceOf(()) = makeInstanceOf
        globalDeclarations.declare(instanceOf(()))
      }._1
    )

  override def dispatch(decl: Declaration[Pre]): Unit =
    decl match {
      case cls: Class[Pre] =>
        if (cls.typeArgs.nonEmpty)
          throw vct.result.VerificationError.Unreachable(
            "Class type parameters should be encoded using monomorphization earlier"
          )

        typeNumber(cls)
        cls.drop()
        cls.decls.foreach {
          case function: InstanceFunction[Pre] =>
            implicit val o: Origin = function.o
            val thisVar = new Variable[Post](TRef())(This)
            diz.having(thisVar.get) {
              functionSucc(function) = globalDeclarations
                .declare(labelDecls.scope {
                  new Function(
                    returnType = dispatch(function.returnType),
                    args =
                      variables.collect {
                        variables.declare(thisVar)
                        function.args.foreach(dispatch)
                      }._1,
                    typeArgs = variables.dispatch(function.typeArgs),
                    body = function.body.map(dispatch),
                    contract = function.contract.rewrite(requires =
                      SplitAccountedPredicate(
                        left = UnitAccountedPredicate(thisVar.get !== Null()),
                        right = SplitAccountedPredicate(
                          left = UnitAccountedPredicate(
                            /* FIXME FunctionInvocation[Post](instanceOf.ref(()), Seq(
                        FunctionInvocation[Post](typeOf.ref(()), Seq(thisVar.get), Nil, Nil, Nil)(PanicBlame("typeOf requires nothing.")),
                        const(typeNumber(cls)),
                      ), Nil, Nil, Nil)(PanicBlame("instanceOf requires nothing.")) */ tt[
                              Post
                            ]
                          ),
                          right = dispatch(function.contract.requires),
                        ),
                      )
                    ),
                    doInline = function.doInline,
                  )(function.blame)(function.o)
                })
            }
          case method: InstanceMethod[Pre] =>
            implicit val o: Origin = method.o
            val thisVar = new Variable[Post](TRef())(This)
            diz.having(thisVar.get) {
              methodSucc(method) = globalDeclarations.declare(labelDecls.scope {
                new Procedure(
                  returnType = dispatch(method.returnType),
                  args =
                    variables.collect {
                      variables.declare(thisVar)
                      method.args.foreach(dispatch)
                    }._1,
                  outArgs = variables.dispatch(method.outArgs),
                  typeArgs = variables.dispatch(method.typeArgs),
                  body = method.body.map(dispatch),
                  contract = method.contract.rewrite(requires =
                    SplitAccountedPredicate(
                      left = UnitAccountedPredicate(thisVar.get !== Null()),
                      right = SplitAccountedPredicate(
                        left = UnitAccountedPredicate(
                          /* FIXME FunctionInvocation[Post](instanceOf.ref(()), Seq(
                        FunctionInvocation[Post](typeOf.ref(()), Seq(thisVar.get), Nil, Nil, Nil)(PanicBlame("typeOf requires nothing.")),
                        const(typeNumber(cls)),
                      ), Nil, Nil, Nil)(PanicBlame("instanceOf requires nothing.")) */ tt[
                            Post
                          ]
                        ),
                        right = dispatch(method.contract.requires),
                      ),
                    )
                  ),
                  doInline = method.doInline,
                  pure = method.pure,
                )(method.blame)(method.o)
              })
            }
          case cons: Constructor[Pre] =>
            implicit val o: Origin = cons.o
            val thisVar = new Variable[Post](TRef())(This)
            consSucc(cons) = globalDeclarations.declare(labelDecls.scope {
              new Procedure(
                returnType = TVoid(),
                args = variables.collect { cons.args.map(dispatch) }._1,
                outArgs =
                  thisVar +:
                    variables.collect { cons.outArgs.map(dispatch) }._1,
                typeArgs = variables.collect { cons.typeArgs.map(dispatch) }._1,
                body = cons.body.map(body =>
                  diz.having(thisVar.get) {
                    Block(Seq(
                      instantiate(cons.cls.decl, thisVar.ref),
                      dispatch(body),
                    ))
                  }
                ),
                contract =
                  diz.having(thisVar.get) {
                    cons.contract.rewrite(ensures =
                      SplitAccountedPredicate(
                        left = UnitAccountedPredicate(
                          (thisVar.get !== Null()) &&
                            (FunctionInvocation[Post](
                              typeOf.ref(()),
                              Seq(thisVar.get),
                              Nil,
                              Nil,
                              Nil,
                            )(PanicBlame("typeOf requires nothing"))(cons.o) ===
                              const(typeNumber(cls))(cons.o))
                        ),
                        right = dispatch(cons.contract.ensures),
                      )
                    )
                  },
              )(PostBlameSplit.left(
                PanicBlame(
                  "Constructor cannot return null value or value of wrong type."
                ),
                cons.blame,
              ))
            })
          case predicate: InstancePredicate[Pre] =>
            val thisVar = new Variable[Post](TRef())(This)
            diz.having(thisVar.get(predicate.o)) {
              predicateSucc(predicate) = globalDeclarations.declare(
                new Predicate(
                  args =
                    variables.collect {
                      variables.declare(thisVar)
                      predicate.args.foreach(dispatch)
                    }._1,
                  body = predicate.body.map(dispatch),
                  threadLocal = predicate.threadLocal,
                  doInline = predicate.doInline,
                )(predicate.o)
              )
            }
          case field: Field[Pre] =>
            fieldSucc(field) = new SilverField(dispatch(field.t))(field.o)
            globalDeclarations.declare(fieldSucc(field))
          case _ => throw ExtraNode
        }
      case decl => rewriteDefault(decl)
    }

  def instantiate(cls: Class[Pre], target: Ref[Post, Variable[Post]])(
      implicit o: Origin
  ): Statement[Post] = {
    Block(Seq(
      SilverNewRef[Post](
        target,
        cls.decls.collect { case field: InstanceField[Pre] =>
          fieldSucc.ref(field)
        },
      ),
      Inhale(
        FunctionInvocation[Post](
          typeOf.ref(()),
          Seq(Local(target)),
          Nil,
          Nil,
          Nil,
        )(PanicBlame("typeOf requires nothing.")) === const(typeNumber(cls))
      ),
    ))
  }

  override def dispatch(stat: Statement[Pre]): Statement[Post] =
    stat match {
      case Instantiate(Ref(cls), Local(Ref(v))) =>
        instantiate(cls, succ(v))(stat.o)
      case inv @ InvokeMethod(
            obj,
            Ref(method),
            args,
            outArgs,
            typeArgs,
            givenMap,
            yields,
          ) =>
        InvokeProcedure[Post](
          ref = methodSucc.ref(method),
          args = dispatch(obj) +: args.map(dispatch),
          outArgs = outArgs.map(dispatch),
          typeArgs = typeArgs.map(dispatch),
          givenMap = givenMap.map { case (Ref(v), e) =>
            (succ(v), dispatch(e))
          },
          yields = yields.map { case (e, Ref(v)) => (dispatch(e), succ(v)) },
        )(PreBlameSplit.left(
          InstanceNullPreconditionFailed(inv.blame, inv),
          PreBlameSplit
            .left(PanicBlame("incorrect instance method type?"), inv.blame),
        ))(inv.o)
      case inv @ InvokeConstructor(
            Ref(cons),
            _,
            out,
            args,
            outArgs,
            typeArgs,
            givenMap,
            yields,
          ) =>
        InvokeProcedure[Post](
          ref = consSucc.ref(cons),
          args = args.map(dispatch),
          outArgs = dispatch(out) +: outArgs.map(dispatch),
          typeArgs = typeArgs.map(dispatch),
          givenMap = givenMap.map { case (Ref(v), e) =>
            (succ(v), dispatch(e))
          },
          yields = yields.map { case (e, Ref(v)) => (dispatch(e), succ(v)) },
        )(inv.blame)(inv.o)
      case fold @ Fold(inv: InstancePredicateApply[Pre]) =>
        Fold(rewriteInstancePredicateApply(inv))(fold.blame)(fold.o)
      case unfold @ Unfold(inv: InstancePredicateApply[Pre]) =>
        Unfold(rewriteInstancePredicateApply(inv))(unfold.blame)(unfold.o)
      case other => rewriteDefault(other)
    }

  def rewriteInstancePredicateApply(
      inv: InstancePredicateApply[Pre]
  ): PredicateApply[Post] =
    PredicateApply[Post](
      predicateSucc.ref(inv.ref.decl),
      dispatch(inv.obj) +: inv.args.map(dispatch),
      dispatch(inv.perm),
    )(inv.o)

  override def dispatch(e: Expr[Pre]): Expr[Post] =
    e match {
      case u @ Unfolding(inv: InstancePredicateApply[Pre], e) =>
        Unfolding(rewriteInstancePredicateApply(inv), dispatch(e))(u.blame)(e.o)
      case inv @ MethodInvocation(
            obj,
            Ref(method),
            args,
            outArgs,
            typeArgs,
            givenMap,
            yields,
          ) =>
        ProcedureInvocation[Post](
          ref = methodSucc.ref(method),
          args = dispatch(obj) +: args.map(dispatch),
          outArgs = outArgs.map(dispatch),
          typeArgs = typeArgs.map(dispatch),
          givenMap = givenMap.map { case (Ref(v), e) =>
            (succ(v), dispatch(e))
          },
          yields = yields.map { case (e, Ref(v)) => (dispatch(e), succ(v)) },
        )(PreBlameSplit.left(
          InstanceNullPreconditionFailed(inv.blame, inv),
          PreBlameSplit
            .left(PanicBlame("incorrect instance method type?"), inv.blame),
        ))(inv.o)
      case inv @ InstancePredicateApply(obj, Ref(pred), args, perm) =>
        implicit val o: Origin = inv.o
        rewriteInstancePredicateApply(inv) &* (dispatch(obj) !== Null())
      case inv @ InstanceFunctionInvocation(
            obj,
            Ref(func),
            args,
            typeArgs,
            givenMap,
            yields,
          ) =>
        FunctionInvocation[Post](
          ref = functionSucc.ref(func),
          args = dispatch(obj) +: args.map(dispatch),
          typeArgs.map(dispatch),
          givenMap = givenMap.map { case (Ref(v), e) =>
            (succ(v), dispatch(e))
          },
          yields = yields.map { case (e, Ref(v)) => (dispatch(e), succ(v)) },
        )(PreBlameSplit.left(
          InstanceNullPreconditionFailed(inv.blame, inv),
          PreBlameSplit
            .left(PanicBlame("incorrect instance function type?"), inv.blame),
        ))(inv.o)
      case ThisObject(_) => diz.top
      case deref @ Deref(obj, Ref(field)) =>
        SilverDeref[Post](dispatch(obj), fieldSucc.ref(field))(deref.blame)(
          deref.o
        )
      case TypeValue(t) =>
        t match {
          case TClass(Ref(cls), Seq()) => const(typeNumber(cls))(e.o)
          case other => ???
        }
      case TypeOf(value) =>
        FunctionInvocation[Post](
          typeOf.ref(()),
          Seq(dispatch(value)),
          Nil,
          Nil,
          Nil,
        )(PanicBlame("typeOf requires nothing"))(e.o)
      case InstanceOf(value, TypeValue(TUnion(ts))) =>
        implicit val o: Origin = e.o
        dispatch(foldOr(ts.map(t => InstanceOf(value, TypeValue(t)))))
      case InstanceOf(value, typeValue) =>
        FunctionInvocation[Post](
          instanceOf.ref(()),
          Seq(
            FunctionInvocation[Post](
              typeOf.ref(()),
              Seq(dispatch(value)),
              Nil,
              Nil,
              Nil,
            )(PanicBlame("typeOf requires nothing"))(e.o),
            dispatch(typeValue),
          ),
          Nil,
          Nil,
          Nil,
        )(PanicBlame("instanceOf requires nothing"))(e.o)
      case Cast(value, typeValue) =>
        dispatch(
          value
        ) // Discard for now, should assert instanceOf(value, typeValue)
      case Result(Ref(app)) =>
        app match {
          case function: Function[Pre] => Result[Post](succ(function))(e.o)
          case function: InstanceFunction[Pre] =>
            Result[Post](functionSucc.ref(function))(e.o)
          case procedure: Procedure[Pre] => Result[Post](succ(procedure))(e.o)
          case method: InstanceMethod[Pre] =>
            Result[Post](methodSucc.ref(method))(e.o)
          case function: InstanceOperatorFunction[Pre] =>
            throw ExcludedByPassOrder(
              "Instance operator functions are already compiled away",
              Some(function),
            )
          case method: InstanceOperatorMethod[Pre] =>
            throw ExcludedByPassOrder(
              "Instance operator methods are already compiled away",
              Some(method),
            )
          case function: LlvmSpecFunction[Pre] =>
            throw ExcludedByPassOrder(
              "Llvm spec functions are already compiled away",
              Some(function),
            )
        }
      case _ => rewriteDefault(e)
    }

  override def dispatch(t: Type[Pre]): Type[Post] =
    t match {
      case TClass(_, _) => TRef()
      case TAnyClass() => TRef()
      case t => rewriteDefault(t)
    }

  override def dispatch(loc: Location[Pre]): Location[Post] =
    loc match {
      case InstancePredicateLocation(predicate, obj, args) =>
        PredicateLocation[Post](
          predicateSucc.ref(predicate.decl),
          dispatch(obj) +: args.map(dispatch),
        )(loc.o)
      case FieldLocation(obj, field) =>
        SilverFieldLocation[Post](dispatch(obj), fieldSucc.ref(field.decl))(
          loc.o
        )
      case default => rewriteDefault(default)
    }
}
