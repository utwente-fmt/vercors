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

  override def desc: String = "Translate every local and field to a pointer such that it can have its address taken"

  case class UnsupportedAddrOf(loc: Expr[_]) extends UserError {
    override def code: String = "unsupportedAddrOf"

    override def text: String = loc.o.messageInContext("Taking an address of this expression is not supported")
  }
}

case class VariableToPointer[Pre <: Generation]() extends Rewriter[Pre] {

  import VariableToPointer._

  var stageTwo = false
  val addressedSet: mutable.Set[Node[Pre]] = new mutable.HashSet[Node[Pre]]()
  val heapVariableMap: SuccessionMap[HeapVariable[Pre], HeapVariable[Post]] = SuccessionMap()
  val variableMap: SuccessionMap[Variable[Pre], Variable[Post]] = SuccessionMap()
  val fieldMap: SuccessionMap[InstanceField[Pre], InstanceField[Post]] = SuccessionMap()

  override def dispatch(program: Program[Pre]): Program[Rewritten[Pre]] = {
    super.dispatch(program)
    stageTwo = true
    super.dispatch(program)
  }

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case v: HeapVariable[Pre] if stageTwo && addressedSet.contains(v) => heapVariableMap(v) = globalDeclarations.declare(new HeapVariable(TPointer(dispatch(v.t)))(v.o))
    case v: Variable[Pre] if stageTwo && addressedSet.contains(v) => variableMap(v) = variables.declare(new Variable(TPointer(dispatch(v.t)))(v.o))
    case f: InstanceField[Pre] if stageTwo && addressedSet.contains(f) => fieldMap(f) = classDeclarations.declare(new InstanceField(TPointer(dispatch(f.t)), f.flags.map { it => dispatch(it) })(f.o))
    case other => allScopes.anySucceed(other, other.rewriteDefault())
  }

  override def dispatch(stat: Statement[Pre]): Statement[Post] = {
    implicit val o: Origin = stat.o
    stat match {
      case s: Scope[Pre] if stageTwo => s.rewrite(locals = variables.dispatch(s.locals), body = Block(s.locals.filter { local => addressedSet.contains(local) }.map { local =>
        implicit val o: Origin = local.o
        Assign(Local[Post](variableMap.ref(local)), NewPointerArray(variableMap(local).t.asPointer.get.element, const(1))(PanicBlame("Size is > 0")))(PanicBlame("Initialisation should always succeed"))
      } ++ Seq(dispatch(s.body))))
      case i@Instantiate(cls, out) if stageTwo =>
        Block(Seq(i.rewriteDefault()) ++ cls.decl.declarations.flatMap {
          case f: InstanceField[Pre] if addressedSet.contains(f) => Seq(Assign(Deref[Post](dispatch(out), fieldMap.ref(f))(PanicBlame("Initialisation should always succeed")), NewPointerArray(fieldMap(f).t.asPointer.get.element, const(1))(PanicBlame("Size is > 0")))(PanicBlame("Initialisation should always succeed")))
          case _ => Seq()
        })
      case other => other.rewriteDefault()
    }
  }

  override def dispatch(expr: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = expr.o
    expr match {
      case AddrOf(Local(Ref(v))) if !stageTwo && !v.t.isInstanceOf[TClass[Pre]] =>
        addressedSet.add(v)
        expr.rewriteDefault()
      case AddrOf(DerefHeapVariable(Ref(v))) if !stageTwo && !v.t.isInstanceOf[TClass[Pre]] =>
        addressedSet.add(v)
        expr.rewriteDefault()
      case AddrOf(Deref(_, Ref(f))) if !stageTwo && !f.t.isInstanceOf[TClass[Pre]] =>
        addressedSet.add(f)
        expr.rewriteDefault()
      //      case AddrOf(inner) if !stageTwo =>
      //        throw UnsupportedAddrOf(inner)
      case deref@DerefHeapVariable(Ref(v)) if stageTwo && addressedSet.contains(v) =>
        DerefPointer(DerefHeapVariable[Post](heapVariableMap.ref(v))(deref.blame))(PanicBlame("Should always be accessible"))
      case Local(Ref(v)) if stageTwo && addressedSet.contains(v) =>
        DerefPointer(Local[Post](variableMap.ref(v)))(PanicBlame("Should always be accessible"))
      case deref@Deref(obj, Ref(f)) if stageTwo && addressedSet.contains(f) =>
        DerefPointer(Deref[Post](dispatch(obj), fieldMap.ref(f))(deref.blame))(PanicBlame("Should always be accessible"))
      case newObject@NewObject(Ref(cls)) if stageTwo =>
        val obj = new Variable[Post](TClass(succ(cls)))
        ScopedExpr(Seq(obj), With(Block(
          Seq(assignLocal(obj.get, newObject.rewriteDefault())) ++ cls.declarations.flatMap {
            case f: InstanceField[Pre] if addressedSet.contains(f) => Seq(Assign(Deref[Post](obj.get, fieldMap.ref(f))(PanicBlame("Initialisation should always succeed")), NewPointerArray(fieldMap(f).t.asPointer.get.element, const(1))(PanicBlame("Size is > 0")))(PanicBlame("Initialisation should always succeed")))
            case _ => Seq()
          }), obj.get))
      case other => other.rewriteDefault()
    }
  }

  override def dispatch(loc: Location[Pre]): Location[Post] = {
    implicit val o: Origin = loc.o
    loc match {
      case HeapVariableLocation(Ref(v)) if stageTwo && addressedSet.contains(v) => PointerLocation(DerefHeapVariable[Post](heapVariableMap.ref(v))(PanicBlame("Should always be accessible")))(PanicBlame("Should always be accessible"))
      case FieldLocation(obj, Ref(f)) if stageTwo && addressedSet.contains(f) => PointerLocation(Deref[Post](dispatch(obj), fieldMap.ref(f))(PanicBlame("Should always be accessible")))(PanicBlame("Should always be accessible"))
      case loc@PointerLocation(inner) if stageTwo && addressedSet.contains(inner) => PointerLocation(dispatch(inner))(loc.blame)
      case other => other.rewriteDefault()
    }
  }
}
