package vct.rewrite

import vct.col.ast._
import vct.col.ref._
import vct.col.origin._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap
import vct.result.VerificationError.UserError

import scala.collection.mutable

case object VariableToPointer extends RewriterBuilder {
  override def key: String = "variableToPointer"

  override def desc: String =
    "Translate every local and field to a pointer such that it can have its address taken"

  case class UnsupportedAddrOf(loc: Expr[_]) extends UserError {
    override def code: String = "unsupportedAddrOf"

    override def text: String =
      loc.o.messageInContext(
        "Taking an address of this expression is not supported"
      )
  }
}

case class VariableToPointer[Pre <: Generation]() extends Rewriter[Pre] {

  import VariableToPointer._

  val addressedSet: mutable.Set[Node[Pre]] = new mutable.HashSet[Node[Pre]]()
  val heapVariableMap: SuccessionMap[HeapVariable[Pre], HeapVariable[Post]] =
    SuccessionMap()
  val variableMap: SuccessionMap[Variable[Pre], Variable[Post]] =
    SuccessionMap()
  val fieldMap: SuccessionMap[InstanceField[Pre], InstanceField[Post]] =
    SuccessionMap()

  override def dispatch(program: Program[Pre]): Program[Rewritten[Pre]] = {
    // TODO: Replace the isInstanceOf[TByReferenceClass] checks with something that more clearly communicates that we want to exclude all reference types
    addressedSet.addAll(program.collect {
      case AddrOf(Local(Ref(v))) if !v.t.isInstanceOf[TByReferenceClass[Pre]] =>
        v
      case AddrOf(DerefHeapVariable(Ref(v)))
          if !v.t.isInstanceOf[TByReferenceClass[Pre]] =>
        v
      case AddrOf(Deref(o, Ref(f)))
          if !f.t.isInstanceOf[TByReferenceClass[Pre]] &&
            !o.t.isInstanceOf[TByValueClass[Pre]] =>
        f
    })
    super.dispatch(program)
  }

  override def dispatch(decl: Declaration[Pre]): Unit =
    decl match {
      // TODO: Use some sort of NonNull pointer type instead
      case v: HeapVariable[Pre] if addressedSet.contains(v) =>
        heapVariableMap(v) = globalDeclarations
          .succeed(v, new HeapVariable(TNonNullPointer(dispatch(v.t)))(v.o))
      case v: Variable[Pre] if addressedSet.contains(v) =>
        variableMap(v) = variables
          .succeed(v, new Variable(TNonNullPointer(dispatch(v.t)))(v.o))
      case f: InstanceField[Pre] if addressedSet.contains(f) =>
        fieldMap(f) = classDeclarations.succeed(
          f,
          new InstanceField(
            TNonNullPointer(dispatch(f.t)),
            f.flags.map { it => dispatch(it) },
          )(f.o),
        )
      case other => allScopes.anySucceed(other, other.rewriteDefault())
    }

  override def dispatch(stat: Statement[Pre]): Statement[Post] = {
    implicit val o: Origin = stat.o
    stat match {
      case s: Scope[Pre] =>
        s.rewrite(
          locals = variables.dispatch(s.locals),
          body = Block(s.locals.filter { local => addressedSet.contains(local) }
            .map { local =>
              implicit val o: Origin = local.o
              Assign(
                Local[Post](variableMap.ref(local)),
                NewNonNullPointerArray(
                  variableMap(local).t.asPointer.get.element,
                  const(1),
                )(PanicBlame("Size is > 0")),
              )(PanicBlame("Initialisation should always succeed"))
            } ++ Seq(dispatch(s.body))),
        )
      case i @ Instantiate(cls, out) =>
        // TODO: Make sure that we recursively build newobject for byvalueclasses
        //       maybe get rid this entirely and only have it in encode by value class
        Block(Seq(i.rewriteDefault()) ++ cls.decl.declarations.flatMap {
          case f: InstanceField[Pre] =>
            if (f.t.asClass.isDefined) {
              Seq(
                Assign(
                  Deref[Post](dispatch(out), fieldMap.ref(f))(PanicBlame(
                    "Initialisation should always succeed"
                  )),
                  NewPointerArray(
                    fieldMap(f).t.asPointer.get.element,
                    const(1),
                  )(PanicBlame("Size is > 0")),
                )(PanicBlame("Initialisation should always succeed")),
                Assign(
                  PointerSubscript(
                    Deref[Post](dispatch(out), fieldMap.ref(f))(PanicBlame(
                      "Initialisation should always succeed"
                    )),
                    const[Post](0),
                  )(PanicBlame("Size is > 0")),
                  dispatch(NewObject[Pre](f.t.asClass.get.cls)),
                )(PanicBlame("Initialisation should always succeed")),
              )
            } else if (addressedSet.contains(f)) {
              Seq(
                Assign(
                  Deref[Post](dispatch(out), fieldMap.ref(f))(PanicBlame(
                    "Initialisation should always succeed"
                  )),
                  NewPointerArray(
                    fieldMap(f).t.asPointer.get.element,
                    const(1),
                  )(PanicBlame("Size is > 0")),
                )(PanicBlame("Initialisation should always succeed"))
              )
            } else { Seq() }
          case _ => Seq()
        })
      case other => other.rewriteDefault()
    }
  }

  override def dispatch(expr: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = expr.o
    expr match {
      case deref @ DerefHeapVariable(Ref(v)) if addressedSet.contains(v) =>
        DerefPointer(
          DerefHeapVariable[Post](heapVariableMap.ref(v))(deref.blame)
        )(PanicBlame("Should always be accessible"))
      case Local(Ref(v)) if addressedSet.contains(v) =>
        DerefPointer(Local[Post](variableMap.ref(v)))(PanicBlame(
          "Should always be accessible"
        ))
      case deref @ Deref(obj, Ref(f)) if addressedSet.contains(f) =>
        DerefPointer(Deref[Post](dispatch(obj), fieldMap.ref(f))(deref.blame))(
          PanicBlame("Should always be accessible")
        )
      case newObject @ NewObject(Ref(cls)) =>
        val obj = new Variable[Post](TByReferenceClass(succ(cls), Seq()))
        ScopedExpr(
          Seq(obj),
          With(
            Block(
              Seq(assignLocal(obj.get, newObject.rewriteDefault())) ++
                cls.declarations.flatMap {
                  case f: InstanceField[Pre] =>
                    if (f.t.asClass.isDefined) {
                      Seq(
                        Assign(
                          Deref[Post](obj.get, anySucc(f))(PanicBlame(
                            "Initialisation should always succeed"
                          )),
                          dispatch(NewObject[Pre](f.t.asClass.get.cls)),
                        )(PanicBlame("Initialisation should always succeed"))
                      )
                    } else if (addressedSet.contains(f)) {
                      Seq(
                        Assign(
                          Deref[Post](obj.get, fieldMap.ref(f))(PanicBlame(
                            "Initialisation should always succeed"
                          )),
                          NewPointerArray(
                            fieldMap(f).t.asPointer.get.element,
                            const(1),
                          )(PanicBlame("Size is > 0")),
                        )(PanicBlame("Initialisation should always succeed"))
                      )
                    } else { Seq() }
                  case _ => Seq()
                }
            ),
            obj.get,
          ),
        )
      case other => other.rewriteDefault()
    }
  }

  override def dispatch(loc: Location[Pre]): Location[Post] = {
    implicit val o: Origin = loc.o
    loc match {
      case HeapVariableLocation(Ref(v)) if addressedSet.contains(v) =>
        PointerLocation(
          DerefHeapVariable[Post](heapVariableMap.ref(v))(PanicBlame(
            "Should always be accessible"
          ))
        )(PanicBlame("Should always be accessible"))
      case FieldLocation(obj, Ref(f)) if addressedSet.contains(f) =>
        PointerLocation(Deref[Post](dispatch(obj), fieldMap.ref(f))(PanicBlame(
          "Should always be accessible"
        )))(PanicBlame("Should always be accessible"))
      case PointerLocation(AddrOf(Deref(obj, Ref(f))))
          if addressedSet.contains(f) =>
        FieldLocation[Post](dispatch(obj), fieldMap.ref(f))
      case PointerLocation(AddrOf(DerefHeapVariable(Ref(v))))
          if addressedSet.contains(v) =>
        HeapVariableLocation[Post](heapVariableMap.ref(v))
      case PointerLocation(AddrOf(local @ Local(_))) =>
        throw UnsupportedAddrOf(local)
      case other => other.rewriteDefault()
    }
  }
}
