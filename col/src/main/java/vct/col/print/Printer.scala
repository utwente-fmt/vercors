package vct.col.print

import scala.language.postfixOps

import hre.util.ScopedStack
import vct.col.ast.{ADTDeclaration, ADTFunction, ADTFunctionInvocation, ActionApply, ActionPerm, AddrOf, AmbiguousMember, AmbiguousResult, AmbiguousSubscript, AmbiguousThis, And, ApplicableContract, ArraySubscript, Assert, Assign, Assume, AxiomaticDataType, BagMemberCount, BitAnd, BitNot, BitOr, BitShl, BitShr, BitUShr, BitXor, Block, Branch, Break, CAnonymousFunctionDeclarator, CArrayDeclarator, CAtomic, CBool, CChar, CConst, CDeclaration, CDeclarationSpecifier, CDeclarationStatement, CDeclarator, CDouble, CFloat, CFunctionDefinition, CGlobalDeclaration, CGoto, CInit, CInline, CInt, CInvocation, CKernel, CLabeledStatement, CLocal, CLong, CName, CParam, CPointer, CPointerDeclarator, CPrimitiveType, CPure, CRestrict, CShort, CSigned, CSpecificationType, CStatic, CStructAccess, CStructDeref, CTypeQualifier, CTypeQualifierDeclarationSpecifier, CTypedFunctionDeclarator, CTypedef, CTypedefName, CUnsigned, CVoid, CVolatile, Case, Cast, CatchClause, Class, Concat, Cons, Constant, Continue, CurPerm, CurrentThreadId, Declaration, DefaultCase, Deref, DerefPointer, Div, Drop, Empty, EmptyProcess, Eq, Eval, Exhale, Exists, Exp, Expr, Field, FieldFlag, Final, FloorDiv, Fold, Forall, Fork, Function, FunctionInvocation, Goto, GpgpuAtomic, GpgpuCudaKernelInvocation, GpgpuGlobalBarrier, GpgpuLocalBarrier, Greater, GreaterEq, Havoc, Head, Held, IdleToken, Implies, Inhale, InlinePattern, InstanceFunction, InstanceFunctionInvocation, InstanceMethod, InstanceOf, InstancePredicate, InstancePredicateApply, IterVariable, JavaAbstract, JavaClass, JavaConstructor, JavaDeref, JavaFields, JavaFinal, JavaImport, JavaInline, JavaInterface, JavaInvocation, JavaLiteralArray, JavaLocal, JavaLocalDeclaration, JavaLocalDeclarationStatement, JavaMethod, JavaModifier, JavaName, JavaNamespace, JavaNative, JavaNewClass, JavaNewDefaultArray, JavaNewLiteralArray, JavaPrivate, JavaProtected, JavaPublic, JavaPure, JavaSharedInitialization, JavaStatic, JavaStrictFP, JavaSynchronized, JavaTClass, JavaTUnion, JavaTransient, JavaVolatile, Join, JoinToken, LabelDecl, Length, Less, LessEq, Let, LiteralSeq, LiteralSet, Local, LocalDecl, Lock, Loop, MapCons, MapDisjoint, MapEq, MapGet, MapItemSet, MapKeySet, MapMember, MapRemove, MapSize, MapValueSet, MethodInvocation, Minus, Mod, Model, ModelAction, ModelChoose, ModelCreate, ModelDeref, ModelDestroy, ModelDo, ModelField, ModelMergeFrom, ModelPerm, ModelProcess, ModelSplitInto, Mult, Neq, NewArray, NewObject, NoPerm, Node, Not, Notify, Null, Old, OptGet, OptGetOrElse, OptNone, OptSome, Or, ParAtomic, ParBarrier, ParBlock, ParBlockDecl, ParInvariant, ParInvariantDecl, ParRegion, Perm, PermPointer, PermPointerIndex, Permutation, Plus, PointerSubscript, PointsTo, PostAssignExpression, PreAssignExpression, Predicate, PredicateApply, Procedure, ProcedureInvocation, ProcessApply, ProcessChoice, ProcessPar, ProcessSelect, ProcessSeq, Product, Program, Range, ReadPerm, Refute, RemoveAt, Return, Scale, Scope, Select, SeqMember, SeqSubscript, SeqUpdate, SetMember, SignalsClause, SilverCurFieldPerm, SilverCurPredPerm, SilverDeref, SilverField, SilverFieldAssign, SilverFold, SilverIf, SilverLocalAssign, SilverNewRef, SilverPerm, SilverPredPerm, SilverPredicateAccess, SilverUnfold, SilverUnfolding, SilverWhile, SimplificationRule, Size, Slice, SpecIgnoreEnd, SpecIgnoreStart, Star, Starall, Statement, SubSet, SubSetEq, SubType, Sum, SuperType, Switch, Synchronized, TAny, TArray, TAxiomatic, TBag, TBool, TBoundedInt, TChar, TClass, TFloat, TFraction, TInt, TMap, TModel, TNotAValue, TNull, TOption, TPointer, TProcess, TRational, TRef, TResource, TSeq, TSet, TString, TTuple, TType, TVoid, TZFraction, Tail, Take, Then, Throw, TryCatchFinally, TupGet, Type, TypeOf, TypeValue, UMinus, UPlus, Unfold, Unfolding, Unlock, ValidArray, ValidMatrix, Values, Variable, Void, Wait, Wand, WandApply, WandCreate, WandQed, WandUse, With, WritePerm}
import vct.col.resolve.{RefJavaLocalDeclaration, Referrable}

import scala.collection.mutable

sealed trait Syntax
case object PVL extends Syntax
case object Silver extends Syntax
case object Java extends Syntax
case class C(gpgpuSyntax: GpgpuSyntax) extends Syntax

sealed trait GpgpuSyntax
case object Any
case object Cuda
case object OpenCL

case object InvalidPrinterStateMutation extends RuntimeException

sealed trait PrinterState {
  def <>(text: String): PrinterState
  def <+>(text: String): PrinterState

  def >> : PrinterState

  def << : PrinterState
  def >@ : PrinterState
  def <@ : PrinterState
  def <*@ : PrinterState
  def >*@ : PrinterState
  def <*> : PrinterState
  def <**> : PrinterState

  def invalid: Nothing = throw InvalidPrinterStateMutation
  def >|(f: PrinterState => PrinterState): PrinterState = f(this)
}

  case class InLine(out: Appendable, lastWasSpace: Boolean, inSpec: Boolean, indent: Int) extends PrinterState {
  override def <>(text: String): PrinterState = {
    out.append(text)
    InLine(out, text.last == ' ', inSpec, indent)
  }
  override def <+>(text: String): PrinterState = {
    if(!lastWasSpace) out.append(' ')
    out.append(text)
    InLine(out, text.last == ' ', inSpec, indent)
  }

  override def >> : PrinterState = Lines(out, 0, inSpec, indent) >>
  override def << : PrinterState = Lines(out, 0, inSpec, indent) <<
  override def >@ : PrinterState = LazyInLine(out, lastWasSpace, inSpec, wantSpec=true, indent)
  override def <@ : PrinterState = LazyInLine(out, lastWasSpace, inSpec, wantSpec=false, indent)
  override def <*@ : PrinterState = Lines(out, 0, inSpec, indent) <*@
  override def >*@ : PrinterState = Lines(out, 0, inSpec, indent) >*@
  override def <*> : PrinterState = Lines(out, 0, inSpec, indent) <*>
  override def <**> : PrinterState = Lines(out, 0, inSpec, indent) <**>
}

case class LazyInLine(out: Appendable, lastWasSpace: Boolean, inSpec: Boolean, wantSpec: Boolean, indent: Int) extends PrinterState {
  def commit: PrinterState = {
    if(inSpec && !wantSpec) InLine(out, lastWasSpace, wantSpec, indent) <+> "@*/"
    else if(!inSpec && wantSpec) InLine(out, lastWasSpace, wantSpec, indent) <+> "/*@ "
    else InLine(out, lastWasSpace, wantSpec, indent)
  }

  override def <>(text: String): PrinterState = commit <> text
  override def <+>(text: String): PrinterState = commit <+> text
  override def >> : PrinterState = LazyInLine(out, lastWasSpace, inSpec, wantSpec, indent+1)
  override def << : PrinterState = LazyInLine(out, lastWasSpace, inSpec, wantSpec, indent-1)
  override def >@ : PrinterState = LazyInLine(out, lastWasSpace, inSpec, wantSpec = true, indent)
  override def <@ : PrinterState = LazyInLine(out, lastWasSpace, inSpec, wantSpec = false, indent)
  override def <*@ : PrinterState = commit <*@
  override def >*@ : PrinterState = commit >*@
  override def <*> : PrinterState = commit <*>
  override def <**> : PrinterState = commit <**>
}

case class Lines(out: Appendable, newLines: Int, inSpec: Boolean, indent: Int) extends PrinterState {
  override def <>(text: String): PrinterState = InLine(out, newLines>0, inSpec, indent) <> text
  override def <+>(text: String): PrinterState = InLine(out, newLines>0, inSpec, indent) <+> text
  override def >> : PrinterState = Lines(out, newLines, inSpec, indent + 1)
  override def << : PrinterState = Lines(out, newLines, inSpec, indent - 1)
  override def >@ : PrinterState = InLine(out, newLines>0, inSpec, indent) >@
  override def <@ : PrinterState = InLine(out, newLines>0, inSpec, indent) <@
  override def <*@ : PrinterState = LazyLines(out, newLines, newLines, inSpec, wantSpec = true, indent)
  override def >*@ : PrinterState = LazyLines(out, newLines, newLines, inSpec, wantSpec = false, indent)
  override def <*> : PrinterState = LazyLines(out, newLines, 1, inSpec, inSpec, indent)
  override def <**> : PrinterState = LazyLines(out, newLines, 2, inSpec, inSpec, indent)
}

case class LazyLines(out: Appendable, newLines: Int, wantNewLines: Int, inSpec: Boolean, wantSpec: Boolean, indent: Int) extends PrinterState {
  def commit: Lines = {
    if(wantNewLines < newLines) {
      (newLines until wantNewLines-1).foreach(_ => {
        (0 until indent).foreach(_ => out.append("\t"))
        out.append('\n')
      })
      (0 until indent).foreach(_ => out.append("\t"))
    }

    if(inSpec != wantSpec) {
      if(inSpec && !wantSpec) out.append("@*/")
      else if(!inSpec && wantSpec) out.append("/*@")

      out.append('\n')
      (0 until indent).foreach(_ => out.append("\t"))
    }

    Lines(out, wantNewLines, wantSpec, indent)
  }

  override def <>(text: String): PrinterState = commit <> text
  override def <+>(text: String): PrinterState = commit <+> text
  override def >> : PrinterState = LazyLines(out, newLines, wantNewLines, inSpec, wantSpec, indent+1)
  override def << : PrinterState = LazyLines(out, newLines, wantNewLines, inSpec, wantSpec, indent-1)
  override def >@ : PrinterState = commit >@
  override def <@ : PrinterState = commit >@
  override def <*@ : PrinterState = LazyLines(out, newLines, wantNewLines, inSpec, wantSpec = true, indent)
  override def >*@ : PrinterState = LazyLines(out, newLines, wantNewLines, inSpec, wantSpec = false, indent)
  override def <*> : PrinterState = LazyLines(out, newLines, wantNewLines.max(1), inSpec, wantSpec, indent)
  override def <**> : PrinterState = LazyLines(out, newLines, wantNewLines.max(2), inSpec, wantSpec, indent)
}

case class Printer(out: Appendable,
                   syntax: Syntax = Java,
                   permissive: Boolean = true,
                   newlineText: String = "\n",
                   indentText: String = "    ",
) {
  var names: ScopedStack[mutable.Map[Referrable, String]] = ScopedStack()
  var usedNames: ScopedStack[mutable.Set[(String, Int)]] = ScopedStack()

  def unfmt(name: String): (String, Int) = {
    if(name.last.isDigit) {
      val overeenkomst = "^.*(0|[1-9][0-9]*)$".r.findFirstMatchIn(name).get
      (overeenkomst.group(0), Integer.parseInt(overeenkomst.group(1)))
    } else {
      (name, -1)
    }
  }

  def nextName(name: String): String = {
    var (baseName, idx) = unfmt(name)
    while(usedNames.exists(_.contains((baseName, idx)))) {
      idx += 1
    }
    usedNames.head += ((baseName, idx))

    if(idx == -1) baseName
    else s"$baseName$idx"
  }

  def name(decl: Declaration): String =
    name(Referrable.from(decl).head)(decl.o.preferredName)

  def name(decl: Referrable)(preferredName: String = decl.name): String =
    names.find(_.contains(decl))
      .getOrElse(names.head)
      .getOrElseUpdate(decl, nextName(preferredName))

  def intersperse[T](text: String)(f: T => PrinterState => PrinterState, xs: Seq[T]): PrinterState => PrinterState =
    (out: PrinterState) => xs.tail.foldLeft(out >| f(xs.head))(_ <> ", " >| f(_))

  def commas[T](f: T => PrinterState => PrinterState, xs: Seq[T]): PrinterState => PrinterState =
    intersperse(", ")(f, xs)

//  def printProgram(program: Program): PrinterState => PrinterState = (out: PrinterState) =>
//    program.declarations.foldLeft(out)(_ >| printDeclaration(_))
//
//  def printStatement(stat: Statement): PrinterState => PrinterState = (out: PrinterState) => stat match {
//    case CDeclarationStatement(decl) =>
//      out .<*> >| commas(printCDeclarationSpecifier, decl.specs) >|
//        commas(decl.ini)
//    case CLabeledStatement(label, statement) =>
//    case ref @ CGoto(label) =>
//    case GpgpuLocalBarrier(requires, ensures) =>
//    case GpgpuGlobalBarrier(requires, ensures) =>
//    case GpgpuAtomic(impl, before, after) =>
//    case JavaLocalDeclarationStatement(decl) =>
//    case SilverUnfold(access) =>
//    case SilverFold(access) =>
//    case SilverWhile(cond, invariant, body) =>
//    case SilverIf(cond, whenTrue, whenFalse) =>
//    case SilverNewRef(v, fields) =>
//    case SilverFieldAssign(obj, field, value) =>
//    case SilverLocalAssign(v, value) =>
//    case Eval(expr) =>
//    case LocalDecl(local) =>
//    case Return(result) =>
//    case Assign(target, value) =>
//    case Block(statements) =>
//    case Scope(locals, body) =>
//    case Branch(branches) =>
//    case Switch(expr, body) =>
//    case Loop(init, cond, update, invariant, body) =>
//    case TryCatchFinally(body, after, catches) =>
//    case Synchronized(obj, body) =>
//    case ParInvariant(decl, inv, content) =>
//    case ParAtomic(inv, content) =>
//    case ParBarrier(block, invs, requires, ensures, content) =>
//    case ParRegion(requires, ensures, blocks) =>
//    case Throw(e) =>
//    case DefaultCase() =>
//    case Case(pattern) =>
//    case Label(decl) =>
//    case Goto(lbl) =>
//    case Exhale(res) =>
//    case Assert(assn) =>
//    case Refute(assn) =>
//    case Inhale(res) =>
//    case Assume(assn) =>
//    case SpecIgnoreStart() =>
//    case SpecIgnoreEnd() =>
//    case Wait(obj) =>
//    case Notify(obj) =>
//    case Fork(runnable) =>
//    case Join(runnable) =>
//    case Lock(obj) =>
//    case Unlock(obj) =>
//    case Fold(pred) =>
//    case Unfold(pred) =>
//    case WandCreate(statements) =>
//    case WandQed(wand) =>
//    case WandApply(wand) =>
//    case WandUse(pred) =>
//    case ModelCreate(target, model, process) =>
//    case ModelDestroy(model) =>
//    case ModelSplitInto(model, leftPerm, left, rightPerm, right) =>
//    case ModelMergeFrom(model, leftPerm, left, rightPerm, right) =>
//    case ModelChoose(model, perm, choiceProcess, choice) =>
//    case ModelDo(model, perm, after, action) =>
//    case Havoc(loc) =>
//    case Break(label) =>
//    case Continue(label) =>
//  }
//
//  def printExpr(e: Expr): PrinterState => PrinterState = (out: PrinterState) => e match {
//    case CLocal(name) =>
//    case CInvocation(applicable, args) =>
//    case CStructAccess(struct, field) =>
//    case CStructDeref(struct, field) =>
//    case GpgpuCudaKernelInvocation(kernel, blocks, threads, args) =>
//    case JavaLocal(name) =>
//    case JavaDeref(obj, field) =>
//    case JavaLiteralArray(exprs) =>
//    case JavaInvocation(obj, typeParams, method, arguments) =>
//    case JavaNewClass(args, typeArgs, name) =>
//    case JavaNewLiteralArray(baseType, dims, initializer) =>
//    case JavaNewDefaultArray(baseType, specifiedDims, moreDims) =>
//    case SilverDeref(obj, field) =>
//    case SilverPerm(obj, field, perm) =>
//    case SilverPredPerm(access) =>
//    case SilverUnfolding(access, body) =>
//    case SilverCurFieldPerm(obj, field) =>
//    case SilverCurPredPerm(ref, args) =>
//    case MapSize(map) =>
//    case Sum(bindings, condition, main) =>
//    case Product(bindings, condition, main) =>
//    case Length(arr) =>
//    case Size(obj) =>
//    case Empty(obj) =>
//    case BagMemberCount(x, xs) =>
//    case value: Constant.BooleanValue =>
//    case MapEq(left, right) =>
//    case MapDisjoint(left, right) =>
//    case Forall(bindings, triggers, body) =>
//    case Exists(bindings, triggers, body) =>
//    case ValidArray(arr, len) =>
//    case ValidMatrix(mat, w, h) =>
//    case SetMember(x, xs) =>
//    case SeqMember(x, xs) =>
//    case MapMember(x, xs) =>
//    case Permutation(xs, ys) =>
//    case Held(obj) =>
//    case IdleToken(thread) =>
//    case JoinToken(thread) =>
//    case Starall(bindings, triggers, body) =>
//    case Star(left, right) =>
//    case Wand(left, right) =>
//    case Scale(scale, res) =>
//    case Perm(loc, perm) =>
//    case PointsTo(loc, perm, value) =>
//    case PermPointer(p, len, perm) =>
//    case PermPointerIndex(p, idx, perm) =>
//    case ModelPerm(loc, perm) =>
//    case ActionPerm(loc, perm) =>
//    case NoPerm() =>
//    case ReadPerm() =>
//    case WritePerm() =>
//    case EmptyProcess() =>
//    case ActionApply(action, args) =>
//    case ProcessApply(process, args) =>
//    case ProcessSeq(left, right) =>
//    case ProcessChoice(left, right) =>
//    case ProcessPar(left, right) =>
//    case ProcessSelect(cond, whenTrue, whenFalse) =>
//    case value: Constant.IntegerValue =>
//    case LiteralSeq(element, values) =>
//    case LiteralSet(element, values) =>
//    case Void() =>
//    case AmbiguousThis() =>
//    case AmbiguousResult() =>
//    case CurrentThreadId() =>
//    case Null() =>
//    case Range(from, to) =>
//    case Values(arr, from, to) =>
//    case OptSome(e) =>
//    case OptNone() =>
//    case MapCons(map, k, v) =>
//    case MapKeySet(map) =>
//    case MapValueSet(map) =>
//    case MapItemSet(map) =>
//    case MapRemove(map, k) =>
//    case Let(binding, value, main) =>
//    case InlinePattern(inner) =>
//    case Local(ref) =>
//    case Deref(obj, ref) =>
//    case ModelDeref(obj, ref) =>
//    case DerefPointer(pointer) =>
//    case AddrOf(e) =>
//    case PredicateApply(ref, args) =>
//    case InstancePredicateApply(obj, ref, args) =>
//    case ADTFunctionInvocation(ref, args) =>
//    case ProcedureInvocation(ref, args, outArgs) =>
//    case FunctionInvocation(ref, args) =>
//    case MethodInvocation(obj, ref, args, outArgs) =>
//    case InstanceFunctionInvocation(obj, ref, args) =>
//    case UPlus(arg) =>
//    case UMinus(arg) =>
//    case BitNot(arg) =>
//    case Not(arg) =>
//    case Exp(left, right) =>
//    case Plus(left, right) =>
//    case Minus(left, right) =>
//    case Mult(left, right) =>
//    case Div(left, right) =>
//    case Mod(left, right) =>
//    case FloorDiv(left, right) =>
//    case BitAnd(left, right) =>
//    case BitOr(left, right) =>
//    case BitXor(left, right) =>
//    case BitShl(left, right) =>
//    case BitShr(left, right) =>
//    case BitUShr(left, right) =>
//    case And(left, right) =>
//    case Or(left, right) =>
//    case Implies(left, right) =>
//    case Eq(left, right) =>
//    case Neq(left, right) =>
//    case Greater(left, right) =>
//    case Less(left, right) =>
//    case GreaterEq(left, right) =>
//    case LessEq(left, right) =>
//    case SubSet(left, right) =>
//    case SubSetEq(left, right) =>
//    case Unfolding(pred, body) =>
//    case CurPerm(loc) =>
//    case Select(condition, whenTrue, whenFalse) =>
//    case NewObject(cls) =>
//    case NewArray(element, dims) =>
//    case Old(expr, at) =>
//    case AmbiguousSubscript(collection, index) =>
//    case SeqSubscript(seq, index) =>
//    case ArraySubscript(arr, index) =>
//    case PointerSubscript(pointer, index) =>
//    case Cons(x, xs) =>
//    case Head(xs) =>
//    case Tail(xs) =>
//    case Drop(xs, count) =>
//    case Take(xs, count) =>
//    case Slice(xs, from, to) =>
//    case SeqUpdate(xs, i, x) =>
//    case Concat(xs, ys) =>
//    case RemoveAt(xs, i) =>
//    case AmbiguousMember(x, xs) =>
//    case OptGet(opt) =>
//    case OptGetOrElse(opt, alt) =>
//    case MapGet(map, k) =>
//    case TupGet(tup, index) =>
//    case TypeValue(value) =>
//    case TypeOf(expr) =>
//    case InstanceOf(value, typeValue) =>
//    case Cast(value, typeValue) =>
//    case SubType(left, right) =>
//    case SuperType(left, right) =>
//    case PreAssignExpression(target, value) =>
//    case PostAssignExpression(target, value) =>
//    case With(pre, value) =>
//    case Then(value, post) =>
//  }
//
//  def printType(t: Type): PrinterState => PrinterState = (out: PrinterState) => t match {
//    case CPrimitiveType(specifiers) =>
//    case JavaTUnion(types) =>
//    case JavaTClass(names) =>
//    case TVoid() =>
//    case TBool() =>
//    case TFloat() =>
//    case TChar() =>
//    case TString() =>
//    case TRef() =>
//    case TArray(element) =>
//    case TPointer(element) =>
//    case TProcess() =>
//    case TModel(model) =>
//    case TAxiomatic(adt, args) =>
//    case TOption(element) =>
//    case TTuple(elements) =>
//    case TSeq(element) =>
//    case TSet(element) =>
//    case TBag(element) =>
//    case TMap(key, value) =>
//    case TType(t) =>
//    case TNotAValue() =>
//    case TAny() =>
//    case TNull() =>
//    case TResource() =>
//    case TInt() =>
//    case TBoundedInt(gte, lt) =>
//    case TRational() =>
//    case TFraction() =>
//    case TZFraction() =>
//    case TClass(cls) =>
//  }
//
//  def printDeclaration(decl: Declaration): PrinterState => PrinterState = (out: PrinterState) => decl match {
//    case CDeclaration(contract, kernelInvariant, specs, inits) =>
//    case CParam(specifiers, declarator) =>
//    case JavaLocalDeclaration(modifiers, t, decls) =>
//    case CFunctionDefinition(specs, declarator, body) =>
//    case CGlobalDeclaration(decl) =>
//    case JavaNamespace(pkg, imports, declarations) =>
//    case JavaClass(name, modifiers, typeParams, ext, imp, decls) =>
//    case JavaInterface(name, modifiers, typeParams, ext, decls) =>
//    case field: SilverField =>
//    case rule: SimplificationRule =>
//    case dataType: AxiomaticDataType =>
//    case function: Function =>
//    case procedure: Procedure =>
//    case predicate: Predicate =>
//    case clazz: Class =>
//    case model: Model =>
//    case JavaSharedInitialization(isStatic, initialization) =>
//    case JavaFields(modifiers, t, decls) =>
//    case JavaConstructor(modifiers, name, parameters, typeParameters, signals, body, contract) =>
//    case JavaMethod(modifiers, returnType, dims, name, parameters, typeParameters, signals, body, contract) =>
//    case function: InstanceFunction =>
//    case method: InstanceMethod =>
//    case predicate: InstancePredicate =>
//    case field: Field =>
//    case variable: Variable =>
//    case decl: LabelDecl =>
//    case decl: ParBlockDecl =>
//    case decl: ParInvariantDecl =>
//    case declaration: ADTDeclaration =>
//    case function: ADTFunction =>
//    case process: ModelProcess =>
//    case action: ModelAction =>
//    case field: ModelField =>
//  }
//
//  def printApplicableContract(node: ApplicableContract): PrinterState => PrinterState = (out: PrinterState) =>
//    ???
//
//  def printParBlock(parBlock: ParBlock): PrinterState => PrinterState = (out: PrinterState) =>
//    ???
//
//  def printCatchClause(catchClause: CatchClause): PrinterState => PrinterState = (out: PrinterState) =>
//    ???
//
//  def printSignalsClause(node: SignalsClause): PrinterState => PrinterState = (out: PrinterState) =>
//    ???
//
//  def printFieldFlag(fieldFlag: FieldFlag): PrinterState => PrinterState = (out: PrinterState) => fieldFlag match {
//    case value: Final =>
//  }
//
//  def printIterVariable(iterVariable: IterVariable): PrinterState => PrinterState = (out: PrinterState) =>
//    ???
//
//  def printSilverPredicateAccess(silverPredAcc: SilverPredicateAccess): PrinterState => PrinterState = (out: PrinterState) =>
//    ???
//
//  def printCDeclarator(node: CDeclarator): PrinterState => PrinterState = (out: PrinterState) => node match {
//    case CPointerDeclarator(pointers, inner) =>
//    case CArrayDeclarator(qualifiers, size, inner) =>
//    case CTypedFunctionDeclarator(params, varargs, inner) =>
//    case CAnonymousFunctionDeclarator(params, inner) =>
//    case CName(name) =>
//  }
//
//  def printCDeclarationSpecifier(cDeclSpec: CDeclarationSpecifier): PrinterState => PrinterState = (out: PrinterState) => cDeclSpec match {
//    case CPure() =>
//    case CInline() =>
//    case CTypedef() =>
//    case CStatic() =>
//    case CVoid() =>
//    case CChar() =>
//    case CShort() =>
//    case CInt() =>
//    case CLong() =>
//    case CFloat() =>
//    case CDouble() =>
//    case CSigned() =>
//    case CUnsigned() =>
//    case CBool() =>
//    case CTypedefName(name) =>
//    case CSpecificationType(t) =>
//    case CTypeQualifierDeclarationSpecifier(typeQual) =>
//    case CKernel() =>
//  }
//
//  def printCTypeQualifier(node: CTypeQualifier): PrinterState => PrinterState = (out: PrinterState) => node match {
//    case CConst() =>
//    case CRestrict() =>
//    case CVolatile() =>
//    case CAtomic() =>
//  }
//
//  def printCPointer(node: CPointer): PrinterState => PrinterState = (out: PrinterState) =>
//    ???
//
//  def printCInit(node: CInit): PrinterState => PrinterState = (out: PrinterState) =>
//    ???
//
//  def printJavaModifier(node: JavaModifier): PrinterState => PrinterState = (out: PrinterState) => node match {
//    case JavaPublic() =>
//    case JavaProtected() =>
//    case JavaPrivate() =>
//    case JavaStatic() =>
//    case JavaAbstract() =>
//    case JavaFinal() =>
//    case JavaStrictFP() =>
//    case JavaNative() =>
//    case JavaSynchronized() =>
//    case JavaTransient() =>
//    case JavaVolatile() =>
//    case JavaPure() =>
//    case JavaInline() =>
//  }
//
//  def printJavaImport(node: JavaImport): PrinterState => PrinterState = (out: PrinterState) =>
//    ???
//
//  def printJavaName(node: JavaName): PrinterState => PrinterState = (out: PrinterState) =>
//    ???
//
//  def print(node: Node): PrinterState => PrinterState = (out: PrinterState) => node match {
//    case program: Program => printProgram(program)
//    case stat: Statement => printStatement(stat)
//    case e: Expr => printExpr(e)
//    case t: Type => printType(t)
//    case decl: Declaration => printDeclaration(decl)
//    case node: ApplicableContract => printApplicableContract(node)
//    case parBlock: ParBlock => printParBlock(parBlock)
//    case catchClause: CatchClause => printCatchClause(catchClause)
//    case node: SignalsClause => printSignalsClause(node)
//    case fieldFlag: FieldFlag => printFieldFlag(fieldFlag)
//    case iterVariable: IterVariable => printIterVariable(iterVariable)
//    case silverPredAcc: SilverPredicateAccess => printSilverPredicateAccess(silverPredAcc)
//    case node: CDeclarator => printCDeclarator(node)
//    case cDeclSpec: CDeclarationSpecifier => printCDeclarationSpecifier(cDeclSpec)
//    case node: CTypeQualifier => printCTypeQualifier(node)
//    case node: CPointer => printCPointer(node)
//    case node: CInit => printCInit(node)
//    case node: JavaModifier => printJavaModifier(node)
//    case node: JavaImport => printJavaImport(node)
//    case node: JavaName => printJavaName(node)
//  }
}
