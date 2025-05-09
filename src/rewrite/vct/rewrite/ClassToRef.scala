package vct.col.rewrite

import vct.col.ast._
import vct.col.origin._
import vct.result.VerificationError
import vct.col.util.AstBuildHelpers._
import hre.util.ScopedStack
import vct.col.rewrite.error.{ExcludedByPassOrder, ExtraNode}
import vct.col.ref.Ref
import vct.col.resolve.ctx.Referrable
import vct.col.typerules.CoercionUtils
import vct.col.util.SuccessionMap

import scala.collection.mutable

case object ClassToRef extends RewriterBuilder {
  override def key: String = "classToRef"
  override def desc: String =
    "Flatten classes into the Ref type, and encode the class type hierarchy operationally."

  private def TypeOfOrigin: Origin =
    Origin(Seq(PreferredName(Seq("type")), LabelContext("classToRef")))

  private def InstanceOfOrigin: Origin =
    Origin(Seq(PreferredName(Seq("subtype")), LabelContext("classToRef")))

  val ByValueClassADTLabel = "ByValueClassADT"

  private case class InstanceNullPreconditionFailed(
      inner: Blame[InstanceNull],
      inv: InvokingNode[_],
  ) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      inner.blame(InstanceNull(inv))
  }

  private case class DerefFieldPointerBlame(
      inner: Blame[InsufficientPermission],
      node: HeapDeref[_],
      clazz: ByValueClass[_],
      field: String,
  ) extends Blame[PointerDerefError] {
    override def blame(error: PointerDerefError): Unit = {
      inner.blame(InsufficientPermission(node))
    }
  }

  private case class SubscriptToAddBlame(blame: Blame[PointerSubscriptError])
      extends Blame[PointerAddError] {
    override def blame(error: PointerAddError): Unit = {
      error match {
        case e @ PointerNull(_) => blame.blame(e)
        case e @ PointerBounds(_) => blame.blame(e)
      }
    }
  }
}

case class ClassToRef[Pre <: Generation]() extends Rewriter[Pre] {
  import vct.col.rewrite.ClassToRef._

  private def This: Origin =
    Origin(Seq(PreferredName(Seq("this")), LabelContext("classToRef")))

  val byRefFieldSucc: SuccessionMap[Field[Pre], SilverField[Post]] =
    SuccessionMap()
  val byValFieldSucc: SuccessionMap[Field[Pre], ADTFunction[Post]] =
    SuccessionMap()
  val byValClassSucc
      : SuccessionMap[ByValueClass[Pre], AxiomaticDataType[Post]] =
    SuccessionMap()
  val methodSucc: SuccessionMap[InstanceMethod[Pre], Procedure[Post]] =
    SuccessionMap()
  val consSucc: SuccessionMap[Constructor[Pre], Procedure[Post]] =
    SuccessionMap()
  val byValConsSucc: SuccessionMap[ByValueClass[Pre], ADTFunction[Post]] =
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

  private def unwrapCastAxioms(
      axiomType: TAxiomatic[Post],
      oldT: Type[Pre],
      newT: Type[Post],
      unique: Option[BigInt],
      fieldRef: Ref[Post, ADTFunction[Post]],
      structSize: Expr[Post],
      fieldSize: Expr[Post],
      toSize: Expr[Post],
  )(implicit o: Origin): Seq[ADTAxiom[Post]] = {
    (oldT match {
      case t: TByValueClass[Pre] => {
        // TODO: If there are no fields we should ignore the first field and add the axioms for the second field
        t.cls.decl.decls.collectFirst({ case field: InstanceField[Pre] =>
          unwrapCastAxioms(
            axiomType,
            field.t,
            dispatch(field.t),
            field.flags.collectFirst { case Unique(unique) => unique },
            fieldRef,
            structSize,
            fieldSize,
            dispatch(t.cls.decl.asInstanceOf[ByValueClass[Pre]].sizes(1)),
          )
        }).getOrElse(Nil)
      }
      case _ => Nil
    }) :+ new ADTAxiom[Post](forall(
      TNonNullPointer(axiomType, None),
      body = { a =>
        InlinePattern(
          PointerCast(a, TNonNullPointer(newT, unique), structSize, toSize)
        ) === PointerCast(
          adtFunctionInvocation(
            fieldRef,
            args = Seq(PointerToAdt(a, axiomType)(NonNullPointerNull)),
          ),
          TNonNullPointer(newT, unique),
          fieldSize,
          toSize,
        )
      },
    ))
  }

  override def dispatch(program: Program[Pre]): Program[Rewritten[Pre]] =
    program.rewrite(declarations =
      globalDeclarations.collect {
        program.declarations.foreach(dispatch)
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
          throw VerificationError.Unreachable(
            "Class type parameters should be encoded using monomorphization earlier"
          )

        typeNumber(cls)

        val thisType = dispatch(cls.classType(Nil))
        cls.decls.foreach {
          case function: InstanceFunction[Pre] =>
            implicit val o: Origin = function.o
            val thisVar = new Variable[Post](thisType)(This)
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
                    inline = function.inline,
                  )(function.blame)(function.o)
                })
            }
          case method: InstanceMethod[Pre] =>
            implicit val o: Origin = method.o
            val thisVar = new Variable[Post](thisType)(This)
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
                  inline = method.inline,
                  pure = method.pure,
                )(method.blame)(method.o)
              })
            }
          case cons: Constructor[Pre] =>
            implicit val o: Origin = cons.o
            val thisVar = new Variable[Post](thisType)(This)
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
            val thisVar = new Variable[Post](thisType)(This)
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
                  inline = predicate.inline,
                )(predicate.o)
              )
            }
          case field: Field[Pre] =>
            if (cls.isInstanceOf[ByReferenceClass[Pre]]) {
              byRefFieldSucc(field) =
                new SilverField(dispatch(field.t))(field.o)
              globalDeclarations.declare(byRefFieldSucc(field))
            }
          case _ => throw ExtraNode
        }
        cls match {
          case cls: ByValueClass[Pre] => encodeByValueClass(cls)
          case _ => cls.drop()
        }
      case decl => super.dispatch(decl)
    }

  private def encodeByValueClass(cls: ByValueClass[Pre]) = {
    implicit val o: Origin = cls.o
    val axiomType = TAxiomatic[Post](byValClassSucc.ref(cls), Nil)
    var castAxioms: Seq[ADTAxiom[Post]] = Seq()
    val (fieldFunctions, fieldInverses, fieldTypes) =
      cls.decls.collect { case field: InstanceField[Pre] =>
        val newT = dispatch(field.t)
        val unique = field.flags.collectFirst { case Unique(unique) => unique }
        val nonnullT = TNonNullPointer(newT, unique)
        byValFieldSucc(field) =
          new ADTFunction[Post](
            Seq(new Variable(axiomType)(field.o)),
            nonnullT,
          )(field.o)
        if (castAxioms.isEmpty) {
          // This is the first field
          castAxioms =
            castAxioms :+ new ADTAxiom[Post](forall(
              TNonNullPointer(axiomType, None),
              body = { a =>
                InlinePattern(PointerCast(
                  a,
                  TNonNullPointer(newT, unique),
                  dispatch(cls.sizes.head),
                  dispatch(cls.sizes(1)),
                )) === adtFunctionInvocation(
                  byValFieldSucc.ref(field),
                  args = Seq(PointerToAdt(a, axiomType)(NonNullPointerNull)),
                )
              },
            ))

          castAxioms =
            castAxioms ++
              (field.t match {
                case t: TByValueClass[Pre] =>
                  // TODO: If there are no fields we should ignore the first field and add the axioms for the second field
                  t.cls.decl.decls
                    .collectFirst({ case innerF: InstanceField[Pre] =>
                      unwrapCastAxioms(
                        axiomType,
                        innerF.t,
                        dispatch(innerF.t),
                        unique,
                        byValFieldSucc.ref(field),
                        dispatch(cls.sizes.head),
                        dispatch(cls.sizes(1)),
                        dispatch(cls.sizes(2)),
                      )
                    }).getOrElse(Nil)
                case _ => Nil
              })
        }
        (
          byValFieldSucc(field),
          new ADTFunction[Post](
            Seq(new Variable(nonnullT)(field.o)),
            axiomType,
          )(
            field.o.copy(
              field.o.originContents.filterNot(_.isInstanceOf[SourceName])
            ).where(name =
              "inv_" + field.o.find[SourceName].map(_.name).getOrElse("unknown")
            )
          ),
          nonnullT,
        )
      }.unzip3
    val constructor =
      new ADTFunction[Post](
        fieldTypes.zipWithIndex.map { case (t, i) =>
          new Variable(t)(Origin(
            Seq(PreferredName(Seq("p_" + i)), LabelContext("classToRef"))
          ))
        },
        axiomType,
      )(
        cls.o.copy(cls.o.originContents.filterNot(_.isInstanceOf[SourceName]))
          .where(name =
            "new_" + cls.o.find[SourceName].map(_.name).getOrElse("unknown")
          )
      )
    val destructorAxioms = fieldFunctions.zip(fieldInverses).map {
      case (f, inv) =>
        new ADTAxiom[Post](forall(
          axiomType,
          body = { a =>
            adtFunctionInvocation[Post](
              inv.ref,
              None,
              args = Seq(
                adtFunctionInvocation[Post](f.ref, None, args = Seq(a))
              ),
            ) === a
          },
          triggers = { a =>
            Seq(Seq(adtFunctionInvocation[Post](f.ref, None, args = Seq(a))))
          },
        ))
    }
    byValConsSucc(cls) = constructor
    byValClassSucc(cls) =
      new AxiomaticDataType[Post](
        destructorAxioms ++ fieldFunctions ++ fieldInverses ++ castAxioms,
        Nil,
      )(o.withContent(LabelContext(ByValueClassADTLabel)))
    globalDeclarations.succeed(cls, byValClassSucc(cls))
  }

  def instantiate(cls: Class[Pre], target: Ref[Post, Variable[Post]])(
      implicit o: Origin
  ): Statement[Post] = {
    cls match {
      case cls: ByReferenceClass[Pre] =>
        Block(Seq(
          SilverNewRef[Post](
            target,
            cls.decls.collect { case field: InstanceField[Pre] =>
              byRefFieldSucc.ref(field)
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
      case _: ByValueClass[Pre] => throw ExtraNode
    }
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
      case other => super.dispatch(other)
    }

  override def dispatch(node: ApplyAnyPredicate[Pre]): ApplyAnyPredicate[Post] =
    node match {
      case inv: InstancePredicateApply[Pre] =>
        PredicateApply[Post](
          predicateSucc.ref(inv.ref.decl),
          dispatch(inv.obj) +: inv.args.map(dispatch),
        )(inv.o)
      case other => other.rewriteDefault()
    }

  override def dispatch(e: Expr[Pre]): Expr[Post] =
    e match {
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
      case ptrOf @ AddrOf(Deref(obj, Ref(field)))
          if obj.t.asByValueClass.isDefined =>
        adtFunctionInvocation[Post](
          byValFieldSucc.ref(field),
          args = Seq(dispatch(obj)),
        )(ptrOf.o)
      case deref @ Deref(obj, Ref(field)) =>
        obj.t match {
          case _: TByReferenceClass[Pre] =>
            SilverDeref[Post](dispatch(obj), byRefFieldSucc.ref(field))(
              deref.blame
            )(deref.o)
          case t: TByValueClass[Pre] =>
            if (field.t.asByValueClass.isDefined) {
              PointerToAdt(
                adtFunctionInvocation[Post](
                  byValFieldSucc.ref(field),
                  args = Seq(dispatch(obj)),
                )(deref.o),
                dispatch(field.t),
              )(DerefFieldPointerBlame(
                deref.blame,
                deref,
                t.cls.decl.asInstanceOf[ByValueClass[Pre]],
                Referrable.originNameOrEmpty(field),
              ))(deref.o)
            } else {
              DerefPointer(
                adtFunctionInvocation[Post](
                  byValFieldSucc.ref(field),
                  args = Seq(dispatch(obj)),
                )(deref.o)
              )(DerefFieldPointerBlame(
                deref.blame,
                deref,
                t.cls.decl.asInstanceOf[ByValueClass[Pre]],
                Referrable.originNameOrEmpty(field),
              ))(deref.o)
            }
        }
      case TypeValue(t) =>
        t match {
          case t: TClass[Pre] if t.typeArgs.isEmpty =>
            const(typeNumber(t.cls.decl))(e.o)
          // Keep any casts intact for the adtAny stage
          case _: TAnyValue[Pre] => e.rewriteDefault()
          case other => ???
        }
      case TypeOf(value) =>
        value.t match {
          case cls: TByReferenceClass[Pre] =>
            FunctionInvocation[Post](
              typeOf.ref(()),
              Seq(dispatch(value)),
              Nil,
              Nil,
              Nil,
            )(PanicBlame("typeOf requires nothing"))(e.o)
          case cls: TByValueClass[Pre] =>
            const[Post](typeNumber(cls.cls.decl))(e.o)
        }
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
      case Cast(value, typeValue)
          if value.t.asClass.isDefined ||
            CoercionUtils.getAnyCoercion(value.t, TAnyClass[Pre]()).isDefined =>
        dispatch(
          value
        ) // Discard for now, should assert instanceOf(value, typeValue)
      case Cast(value, TypeValue(t)) if value.t == t => dispatch(value)
      case PointerCast(value, t, _, _) if value.t == t => dispatch(value)
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
          case function: LLVMSpecFunction[Pre] =>
            throw ExcludedByPassOrder(
              "LLVM spec functions are already compiled away",
              Some(function),
            )
        }
      case p @ Perm(PredicateLocation(inv: InstancePredicateApply[Pre]), _) =>
        implicit val o: Origin = e.o
        Star[Post](p.rewrite(), dispatch(inv.obj) !== Null())
      case v @ Value(PredicateLocation(inv: InstancePredicateApply[Pre])) =>
        implicit val o: Origin = e.o
        Star[Post](v.rewrite(), dispatch(inv.obj) !== Null())
      case dp @ DerefPointer(ptr) if dp.t.asByValueClass.isDefined =>
        PointerToAdt(dispatch(ptr), dispatch(dp.t))(dp.blame)(dp.o)
      case ps @ PointerSubscript(ptr, index) if ps.t.asByValueClass.isDefined =>
        PointerToAdt(
          PointerAdd(dispatch(ptr), dispatch(index))(SubscriptToAddBlame(
            ps.blame
          ))(ps.o),
          dispatch(ps.t),
        )(ps.blame)(ps.o)
      case _ => super.dispatch(e)
    }

  override def dispatch(t: Type[Pre]): Type[Post] =
    t match {
      case _: TByReferenceClass[Pre] => TRef()
      case t: TByValueClass[Pre] =>
        TAxiomatic(
          byValClassSucc.ref(t.cls.decl.asInstanceOf[ByValueClass[Pre]]),
          Nil,
        )
      case TAnyClass() => TRef()
      case t => super.dispatch(t)
    }

  override def dispatch(loc: Location[Pre]): Location[Post] =
    loc match {
      case FieldLocation(obj, Ref(field)) =>
        obj.t match {
          case _: TByReferenceClass[Pre] =>
            SilverFieldLocation[Post](dispatch(obj), byRefFieldSucc.ref(field))(
              loc.o
            )
          case _: TByValueClass[Pre] =>
            PointerLocation[Post](
              adtFunctionInvocation[Post](
                byValFieldSucc.ref(field),
                None,
                args = Seq(dispatch(obj)),
              )(loc.o)
            )(NonNullPointerNull)(loc.o)
        }
      case default => super.dispatch(default)
    }
}
