package vct.rewrite

import vct.col.ast.{Expr, _}
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.typerules.CoercingRewriter
import vct.result.VerificationError.UserError
import hre.util.ScopedStack

import scala.collection.mutable

case class DisallowedConstAssignment(target: Node[_]) extends UserError {
  override def code: String = "disallowedConstAssignment"
  override def text: String =
    target.o.messageInContext("Cannot assign to constant target.")
}

case class DisallowedQualifiedType(target: Node[_]) extends UserError {
  override def code: String = "disallowedQualifiedType"
  override def text: String =
    target.o.messageInContext("This qualified type is not allowed.")
}

case class DisallowedQualifiedMethodCoercion(calledOrigin: Origin) extends UserError {
  override def code: String = "disallowedQualifiedMethodCoercion"
  override def text: String =
    calledOrigin.messageInContext("The coercion of args with qualifiers for this method call is not allowed.")
}

case class DisallowedQualifiedCoercion(calledOrigin: Origin, source: Type[_], target: Type [_]) extends UserError {
  override def code: String = "disallowedQualifiedCoercion"
  override def text: String =
    calledOrigin.messageInContext(s"The coercion of $source to $target is not allowed.")
}

case object TypeQualifierCoercion extends RewriterBuilder {
  override def key: String = "TypeQualifierCoercion"
  override def desc: String =
    "Removes qualifiers from types."
}

case class TypeQualifierCoercion[Pre <: Generation]()
  extends CoercingRewriter[Pre] {

  override def applyCoercion(e: => Expr[Post], coercion: Coercion[Pre])(
    implicit o: Origin
  ): Expr[Post] = {
    coercion match {
      case CoerceFromConst(_) =>
      case CoerceToConst(_) =>
      case CoerceToUnique(_, _) =>
      case CoerceFromUnique(_, _) =>
      case CoerceBetweenUnique(_, _, _) =>
      case CoerceToUniquePointer(s, t) => return UniquePointerCoercion(e, dispatch(t))
      case CoerceFromUniquePointer(s, t) => return UniquePointerCoercion(e, dispatch(t))
      case CoerceBetweenUniquePointer(s, t) => return UniquePointerCoercion(e, dispatch(t))
      case _ =>
    }
    e
  }

  override def postCoerce(t: Type[Pre]): Type[Post] =
    t match {
      case TConst(t) => dispatch(t)
      case TUnique(_, _) => throw DisallowedQualifiedType(t)
      case TPointer(it) => makePointer(it)
      case other => other.rewriteDefault()
    }

  override def postCoerce(e: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = e.o
    e match {
      case PreAssignExpression(target, _) if target.t.isInstanceOf[TConst[Pre]] => throw DisallowedConstAssignment(target)
      case PostAssignExpression(target, _) if target.t.isInstanceOf[TConst[Pre]] => throw DisallowedConstAssignment(target)
      case npa @ NewPointerArray(t, size, _) =>
        val (info, newT) = getUnqualified(t)
        if(info.const) NewConstPointerArray(newT, dispatch(size))(npa.blame)
        else NewPointerArray(newT, dispatch(size), info.unique)(npa.blame)
      case other => other.rewriteDefault()
    }
  }

  override def postCoerce(s: Statement[Pre]): Statement[Post] =
    s match {
      case Assign(target, _) if getUnqualified(target.t)._1.const => throw DisallowedConstAssignment(target)
      case a@AssignInitial(target, value) => Assign(dispatch(target), dispatch(value))(a.blame)(a.o)
      case other => other.rewriteDefault()
    }

  case class InnerInfo(){
    var unique: Option[BigInt] = None
    var const: Boolean = false
  }

  def getUnqualified(t: Type[Pre], info: InnerInfo = InnerInfo()): (InnerInfo, Type[Post]) = t match {
    case TConst(_) | TUnique(_, _) if info.const || info.unique.isDefined =>
      throw DisallowedQualifiedType(t)
    case TConst(it) =>
      info.const = true
      getUnqualified(it, info)
    case TUnique(it, id) =>
      info.unique = Some(id)
      getUnqualified(it, info)
    case _ => (info, dispatch(t))
  }

  def makePointer(t: Type[Pre]): Type[Post] = {
    implicit val o: Origin = t.o
    val (info, resType) = getUnqualified(t)
    if(info.const) TConstPointer(resType)
    else if (info.unique.isDefined) TUniquePointer(resType, info.unique.get)
    else TPointer(resType)
  }
}

case object MakeUniqueMethodCopies extends RewriterBuilder {
  override def key: String = "MakeUniqueMethodCopies"
  override def desc: String =
    "Makes copies of called function that are specialized for unique pointers."
}

case class MakeUniqueMethodCopies[Pre <: Generation]()
    extends Rewriter[Pre] {
  val methodCopyTypes: ScopedStack[Map[Type[Pre], Type[Post]]] = ScopedStack()
  val callee: ScopedStack[Declaration[Pre]] = ScopedStack()
  val checkedCallees: mutable.Set[Declaration[Pre]] = mutable.Set()

  val abstractFunction: mutable.Map[(Function[Pre], Map[Type[Pre], Type[Post]]), Function[Post]] = mutable.Map()
  val abstractProcedure: mutable.Map[(Procedure[Pre], Map[Type[Pre], Type[Post]]), Procedure[Post]] = mutable.Map()
  def getCopyType(t: Type[Pre]): Option[Type[Post]] = methodCopyTypes.topOption.flatMap(m => m.get(t))

  override def dispatch(t: Type[Pre]): Type[Post] = getCopyType(t).getOrElse(t.rewriteDefault())

  case class UniqueCoercion(givenArgT: Type[Pre], originalParamT: Type[Pre])
  case class Args(originalParams: Seq[Variable[Pre]], coercions: Seq[(UniqueCoercion, BigInt)])

  def addArgs(params: Seq[Variable[Pre]], args: Seq[Expr[Pre]]): Args = {
    Args(params, containsUniqueCoerce(args))
  }

  def argsNoCoercions(args: Seq[Args]) : Boolean = args.forall(_.coercions.isEmpty)

  def removeCoercions(args: Seq[Expr[Pre]]): Seq[Expr[Post]] = args.map({
    case UniquePointerCoercion(e, t) => dispatch(e)
    case e => dispatch(e)
  })

  def containsUniqueCoerce(xs: Seq[Expr[Pre]]) : Seq[(UniqueCoercion, BigInt)] =
    xs.zipWithIndex.collect {
      case (UniquePointerCoercion(e, target), i) => (UniqueCoercion(e.t, target), i)
    }

  def getCoercionMap(coercions: Seq[UniqueCoercion], calledOrigin: Origin): Map[Type[Pre], Type[Post]] = {
    coercions.groupMapReduce[Type[Pre], Type[Post]](
      _.originalParamT)(
      c => dispatch(c.givenArgT))(
      // For any duplicates, we exit if they do not resolve to the same type
      (l, r) => if(l == r) l else throw DisallowedQualifiedMethodCoercion(calledOrigin))
  }

  def checkArgs(args: Seq[Variable[Pre]], coercedTypes: Set[Type[Pre]], coercedResults: Set[Type[Post]],
                coercedArgs: Set[BigInt], calledOrigin: Origin): Unit = {
    // Check if any non-coerced arguments contain a coerced type
    args.zipWithIndex.foreach({
      case (a, i) =>
        // Look at non-coerced args
        if(!coercedArgs.contains(i)) {
            // An argument cannot contain coercions themselves
          if(a.collectFirst { case _: UniquePointerCoercion[Pre] => () }.isDefined ||
            // The (sub)type cannot be coerced in another argument
            a.t.collectFirst { case t: Type[Pre] if coercedTypes.contains(t) => () }.isDefined ||
            // The (sub)type cannot be the same as a resulting coercion
            dispatch(a.t).collectFirst { case t: Type[Post] if coercedResults.contains(t) => () }.isDefined
          ){
            throw DisallowedQualifiedMethodCoercion(calledOrigin)
          }
        }
    })
  }

  def checkBody(body: Node[Pre], callee: Declaration[Pre], seenMethods: mutable.Set[Declaration[Pre]], calledOrigin: Origin): Unit = {
    body.collect {
      case inv: AnyMethodInvocation[Pre] if !seenMethods.contains(inv.ref.decl) =>
          if(inv.ref.decl == callee) throw DisallowedQualifiedMethodCoercion(calledOrigin)
          inv.ref.decl.body.map(checkBody(_, callee, seenMethods.addOne(inv.ref.decl), calledOrigin))
      case inv: AnyFunctionInvocation[Pre] if !seenMethods.contains(inv.ref.decl) =>
        if(inv.ref.decl == callee) throw DisallowedQualifiedMethodCoercion(calledOrigin)
        inv.ref.decl.body.map(checkBody(_, callee, seenMethods.addOne(inv.ref.decl), calledOrigin))
    }
  }

  /* So we need to check the following things:
  1. Any args with a same type that is being coerced, needs to be coerced to the exact same type.
    * This is also the case for any given, yields, and out args
  2. Any type that is coerced, cannot be contained in any other type
    * This rules out coercing pointer of pointers, but I see no easy around this at the time
  3. If the call is recursive, we do not allow this.
    * Also if there is a recursive call, further down the call tree, it is not allowed.
  4. The output types need to be unique
  5. We cannot coerce towards a type, for which this type was already present and not coerced
  */
  def getCoercionMapAndCheck(allArgs: Seq[Args], returnType: Type[Pre], returnCoercion: Option[UniqueCoercion],
                             anyReturnCoercion: Boolean,
                             calledOrigin: Origin,
                             body: Option[Node[Pre]], original: Declaration[Pre]
                            ): Map[Type[Pre], Type[Post]] = {
    val coercions: Seq[UniqueCoercion] = allArgs.flatMap(f => f.coercions.view.map(_._1)) :++ returnCoercion
    val coercionMap = getCoercionMap(coercions, calledOrigin) // Checks 1
    val coercedTypes = coercionMap.keySet
    val coercedResults = coercionMap.values.toSet
    // Checks 4
    if(coercedResults.size != coercedTypes.size){
      throw DisallowedQualifiedMethodCoercion(calledOrigin)
    }

    allArgs.foreach({ args =>
      val coercedArgs = args.coercions.map(_._2).toSet
      checkArgs(args.originalParams, coercedTypes, coercedResults, coercedArgs, calledOrigin) // Checks 2
    })
    // check return type if it not coerced (also 2)
    // But not 1: if we have an explicit coercion (which is checked)
    if(returnCoercion.isEmpty) {
      // anyReturnCoercion means we are fine with a coerced return type. But no coerced subtype can be present though
      val checkedSet = if(anyReturnCoercion) coercedTypes - returnType else coercedTypes
      returnType.collectFirst {
        case t: Type[Pre] if checkedSet.contains(t) => throw DisallowedQualifiedMethodCoercion(calledOrigin)
      }
      // If the returnType is not coerced, it cannot be contained in any coerced Results
      dispatch(returnType).collectFirst {
        case t: Type[Post] if coercedResults.contains(t) => throw DisallowedQualifiedMethodCoercion(calledOrigin)
      }
    }
    if(!checkedCallees.contains(original)) {
      // If the body of this functions calls the callee, we end up with recursion we do not want
      body.foreach(b => checkBody(b, callee.top, mutable.Set(original), calledOrigin)) // Checks 3
      checkedCallees.addOne(original)
    }
    coercionMap
  }

  // Instead of the regular procedure, we create an abstract procedure, which is the same, but with different types
  def createAbstractProcedureCopy(original: Procedure[Pre], typeCoerced: Map[Type[Pre], Type[Post]]): Procedure[Post] = {
    methodCopyTypes.having(typeCoerced) {
      globalDeclarations.declare({
        // Subtle, need to create variable scope, otherwise variables are already 'succeeded' in different copies.
        variables.scope({
          original.rewrite(body = None)
        })
      })
    }
  }

  // Same for functions
  def createAbstractFunctionCopy(original: Function[Pre], typeCoerced: Map[Type[Pre], Type[Post]]): Function[Post] = {
    methodCopyTypes.having(typeCoerced) {
      globalDeclarations.declare({
        // Subtle, need to create variable scope, otherwise variables are already 'succeeded' in different copies.
        variables.scope({
          original.rewrite(body = None)
        })
      })
    }
  }

  def rewriteProcedureInvocation(inv: ProcedureInvocation[Pre], returnCoercion: Option[UniqueCoercion], anyReturnPointer: Boolean = false): ProcedureInvocation[Post] = {
    val f = inv.ref.decl
    val allArgs: Seq[Args] = Seq(addArgs(f.args, inv.args),
      addArgs(f.outArgs, inv.outArgs))
    if(argsNoCoercions(allArgs) && returnCoercion.isEmpty) return inv.rewriteDefault()
    if(callee.top == f) throw DisallowedQualifiedMethodCoercion(inv.o)
    // Yields and givens are not supported for now (is possible)
    if(inv.givenMap.nonEmpty || inv.yields.nonEmpty) throw DisallowedQualifiedMethodCoercion(inv.o)

    val map = getCoercionMapAndCheck(allArgs, f.returnType, returnCoercion, anyReturnPointer, inv.o, f.body, f)
    val newProc: Procedure[Post] =
      abstractProcedure.getOrElseUpdate((f, map), createAbstractProcedureCopy(f, map))
    val newArgs = removeCoercions(inv.args)
    val newOutArgs = removeCoercions(inv.outArgs)
    inv.rewrite(ref = newProc.ref, args=newArgs, outArgs=newOutArgs)
  }

  // For AmbiguousSubscript / DerefPointer we do not care about how the return type is coerced
  // so if we encounter invocations we communicate that.
  def rewriteAnyPointerReturn(e: Expr[Pre]): Expr[Post] = e match {
    case inv: ProcedureInvocation[Pre] =>
      rewriteProcedureInvocation(inv, None, anyReturnPointer=true)
//    case inv: FunctionInvocation[Pre] =>
//      rewriteFunctionInvocation(inv, None, anyReturnPointer=true)
    case e => dispatch(e)
  }

  // If the coerce contains an invocation, maybe it is valid, otherwise not
  def rewriteCoerce(e: Expr[Pre], target: Type[Pre]): Expr[Post] = e match {
    case inv: ProcedureInvocation[Pre] =>
      val returnCoercion = Some(UniqueCoercion(target, inv.t))
      rewriteProcedureInvocation(inv, returnCoercion)
//    case inv: FunctionInvocation[Pre] =>
    //      rewriteFunctionInvocation(inv, None, anyReturnPointer=true)
    case am@AmbiguousMinus(pointer, _) => am.rewrite(left=rewriteCoerce(pointer, target))
    case am@AmbiguousPlus(pointer, _) => am.rewrite(left=rewriteCoerce(pointer, target))
    case e => throw DisallowedQualifiedCoercion(e.o, e.t, target)
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = e.o
    e match {
      case inv@FunctionInvocation(f, args, typeArgs, givenMap, yields) =>
        val allArgs: Seq[Args] = Seq(addArgs(f.decl.args, args))
        if(argsNoCoercions(allArgs)) return inv.rewriteDefault()
        if(callee.top == inv.ref.decl) throw DisallowedQualifiedMethodCoercion(inv.o)
        // Yields and givens are not supported
        if(givenMap.nonEmpty || yields.nonEmpty) throw DisallowedQualifiedMethodCoercion(inv.o)

        val map = getCoercionMapAndCheck(allArgs, f.decl.returnType, ???, ???, inv.o, f.decl.body, f.decl)
        // Make sure we only create one copy per coercion mapping
        val newFunc: Function[Post] =
          abstractFunction.getOrElseUpdate((f.decl, map), createAbstractFunctionCopy(f.decl, map))
        val newArgs = removeCoercions(args)
        inv.rewrite(ref = newFunc.ref, args=newArgs)
      case inv: ProcedureInvocation[Pre] =>
        rewriteProcedureInvocation(inv, None)
      // So this is awkward, but...
      // A lot of times we just coerced to 'pointer', as with subscripting. In this case we don't care if the return gets
      // coerced.
      case e@AmbiguousSubscript(p, _) => e.rewrite(collection=rewriteAnyPointerReturn(p))
      case e@DerefPointer(p) => e.rewrite(pointer=rewriteAnyPointerReturn(p))
      case e@FreePointer(p) => e.rewrite(pointer=rewriteAnyPointerReturn(p))
      case e@PointerBlockLength(p) => e.rewrite(pointer=rewriteAnyPointerReturn(p))
      case e@PointerLength(p) => e.rewrite(pointer=rewriteAnyPointerReturn(p))
      case e@PointerBlockOffset(p) => e.rewrite(pointer=rewriteAnyPointerReturn(p))
      case e@SharedMemSize(p) => e.rewrite(pointer=rewriteAnyPointerReturn(p))
      // We store the coercion for the return type
      case u: UniquePointerCoercion[Pre] => rewriteCoerce(u.e, u.t)
      // TODO: consider doing exactly the same for any other abstractFunction/abstractMethod
      case other => other.rewriteDefault()
    }
  }

  override def dispatch(s: Statement[Pre]): Statement[Post] = {
    implicit val o: Origin = s.o
    s match {
      case ev@Eval(e) =>
        ev.rewrite(rewriteAnyPointerReturn(e))
      case other => other.rewriteDefault()
    }
  }

  override def dispatch(d: Declaration[Pre]): Unit = d match {
    case f: AbstractFunction[Pre] => callee.having(f){
      checkedCallees.clear()
      allScopes.anySucceed(f, f.rewriteDefault())
    }
    case f: AbstractMethod[Pre] => callee.having(f){
      checkedCallees.clear()
      allScopes.anySucceed(f, f.rewriteDefault())
    }
    case other => allScopes.anySucceed(other, other.rewriteDefault())
  }
}
