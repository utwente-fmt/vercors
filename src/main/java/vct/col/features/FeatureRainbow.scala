package vct.col.features

import vct.col.ast.`type`.{ASTReserved, ClassType, PrimitiveSort, PrimitiveType, TypeExpression, TypeVariable}
import vct.col.ast.stmt
import vct.col.ast.stmt.composite.{BlockStatement, ForEachLoop, IfStatement, LoopStatement, ParallelBarrier, ParallelBlock, ParallelInvariant, ParallelRegion}
import vct.col.ast.stmt.decl.{ASTClass, ASTDeclaration, ASTFlags, ASTSpecial, Contract, DeclarationStatement, Method, NameSpace, ProgramUnit, VariableDeclaration}
import vct.col.ast.expr.{Binder, BindingExpression, MethodInvokation, NameExpression, NameExpressionKind, OperatorExpression, StandardOperator}
import vct.col.ast.expr
import vct.col.ast.expr.constant.{ConstantExpression, StructValue}
import vct.col.ast.util.{AbstractVisitor, RecursiveVisitor}
import vct.col.ast.generic.{ASTNode, BeforeAfterAnnotations}
import vct.col.ast.langspecific.c.{OMPFor, OMPForSimd, OMPParallel, OMPParallelFor, OMPSection, OMPSections}
import vct.col.ast.stmt.decl.ASTClass.ClassKind
import vct.col.ast.stmt.terminal.AssignmentStatement
import vct.col.rewrite.PVLEncoder
import vct.parsers.rewrite.InferADTTypes

import scala.collection.JavaConverters._
import scala.collection.mutable

class RainbowVisitor(source: ProgramUnit) extends RecursiveVisitor(source, true) {
  val features: mutable.Set[Feature] = mutable.Set()

  source.asScala.foreach(visitTopLevelDecl)

  def visitTopLevelDecl(decl: ASTNode): Unit = decl match {
    case _: ASTClass =>
    case ns: NameSpace =>
      ns.asScala.foreach(visitTopLevelDecl)
    case _: DeclarationStatement =>
      features += TopLevelFields
    case _ =>
      features += TopLevelDeclarations
  }

  override def visit(v: StructValue): Unit = {
    super.visit(v)
  }

  override def visit(c: ASTClass): Unit = {
    super.visit(c)
    if(c.super_classes.nonEmpty || c.implemented_classes.nonEmpty) {
      features += Inheritance
      features += NotJavaEncoded
    }
    if(c.kind != ClassKind.Record && c.methods().asScala.nonEmpty)
      features += This
    if(c.name.startsWith("Atomic"))
      features += JavaAtomic
    if(c.staticFields().asScala.nonEmpty)
      features += StaticFields
    if(c.kind == ClassKind.Kernel)
      features += KernelClass
    if(c.fields().asScala.nonEmpty && c.kind != ClassKind.Record)
      features += NotJavaEncoded
    if(c.asScala.collectFirst {
      case method: Method if method.kind == Method.Kind.Constructor => ()
    }.isEmpty) {
      features += ClassWithoutConstructor
    }
  }

  private def isPure(m: Method): Boolean =
    Set(Method.Kind.Predicate, Method.Kind.Pure).contains(m.kind) ||
      m.annotations().asScala.exists(_.isReserved(ASTReserved.Pure))

  private def isInline(m: Method): Boolean =
    (m.isValidFlag(ASTFlags.INLINE) && m.getFlag(ASTFlags.INLINE)) ||
      m.annotations().asScala.exists(_.isReserved(ASTReserved.Inline))

  override def visit(m: Method): Unit = {
    var forbidRecursion = false

    if(m.annotated())
      if (m.annotations().size == 1 && m.isSynchronized) {
        features += Synchronized
      } else {
        features += MethodAnnotations
      }
    if(m.name == "csl_invariant")
      features += JavaAtomic
    if(m.name == PVLEncoder.INV && !getParentNode.asInstanceOf[ASTClass].methods().asScala.exists(_.name == PVLEncoder.HELD))
      features += PVLSugar
    if(m.name == "run")
      features += PVLSugar
    if(m.getContract != null) {
      if(m.getContract.yields.nonEmpty || m.getContract.`given`.nonEmpty)
        features += GivenYields
      if(m.getContract.invariant != Contract.default_true)
        features += ContextEverywhere
      if(m.getContract.pre_condition.isConstant(false)) {
        features += SpecIgnore
        forbidRecursion = true
      }
    }
    if(isPure(m) && isInline(m))
      features += InlinePredicate
    if(isPure(m) && m.name == "lock_invariant")
      features += LockInvariant
    if(isPure(m) && m.getBody.isInstanceOf[BlockStatement])
      features += PureImperativeMethods
    if(m.kind == Method.Kind.Constructor)
      features += Constructors
    if(!m.getReturnType.isPrimitive(PrimitiveSort.Void) && !isPure(m))
      features += NonVoidMethods

    if(!forbidRecursion) {
      super.visit(m)
    }
  }

  override def visit(ct: ClassType): Unit = {
    super.visit(ct)
//    if(ct.definition == null)
//      features += NotJavaResolved
    if(ct.names == Seq("String"))
      features += StringClass
  }

  override def visit(c: Contract): Unit = {
    if(c.invariant != Contract.default_true)
      features += ContextEverywhere
  }

  override def visit(special: ASTSpecial): Unit = {
    super.visit(special)
    special.kind match {
      case ASTSpecial.Kind.SpecIgnoreStart | ASTSpecial.Kind.SpecIgnoreEnd =>
        features += SpecIgnore
      case ASTSpecial.Kind.Expression =>
        features += ExpressionStatement
      case ASTSpecial.Kind.ActionHeader =>
        features += ActionHeader
      case ASTSpecial.Kind.Send | ASTSpecial.Kind.Recv =>
        features += ParallelBlocks
      case ASTSpecial.Kind.Fork | ASTSpecial.Kind.Join |
           ASTSpecial.Kind.Lock | ASTSpecial.Kind.Unlock |
           ASTSpecial.Kind.Wait | ASTSpecial.Kind.Notify =>
        features += PVLSugar
      case ASTSpecial.Kind.Open | ASTSpecial.Kind.Close =>
        features += NotJavaEncoded
      case ASTSpecial.Kind.Break =>
        features += Break
        if (special.args.length == 0) {
          features += ImplicitLabels
        }
      case ASTSpecial.Kind.Continue =>
        features += Continue
        if (special.args.length == 0) {
          features += ImplicitLabels
        }
      case ASTSpecial.Kind.Goto => features += Goto
      case ASTSpecial.Kind.Throw => features += Exceptions
      case ASTSpecial.Kind.With | ASTSpecial.Kind.Then | ASTSpecial.Kind.Label =>
        features += ImproperlySortedBeforeAfter
      case _ =>
    }
  }

  override def visit(decl: VariableDeclaration): Unit = {
    super.visit(decl)
    features += MultiDecls
  }

  override def visit(t: TypeVariable): Unit = {
    super.visit(t)
    if(t.name == InferADTTypes.typeVariableName)
      features += UnresolvedTypeInference
  }

  def visitBeforeAfter(node: BeforeAfterAnnotations): Unit = {
    if(node.get_after() != null && node.get_after().asScala.nonEmpty) {
      features += BeforeAfter
    }

    if(node.get_before() != null && node.get_before().asScala.nonEmpty) {
      features += BeforeAfter
    }
  }

  override def visit(loop: LoopStatement): Unit = {
    super.visit(loop)
    visitBeforeAfter(loop)

    if(loop.getContract != null &&
      (loop.getContract.pre_condition != Contract.default_true ||
      loop.getContract.post_condition != Contract.default_true)) {
      features += OpenMP
    }
  }

  override def visit(op: OperatorExpression): Unit = {
    super.visit(op)
    visitBeforeAfter(op)
    op.operator match {
      case StandardOperator.Subscript =>
        features += Arrays
        if(op.second.isa(StandardOperator.RangeSeq)) {
          features += SubscriptRange
        }
        op.second match {
          case _: NameExpression | _: ConstantExpression =>
          case _ => features += ComplexSubscript
        }
        if(op.second.isReserved(ASTReserved.Any)) {
          features += AnySubscript
        }
        if(op.first.getType.isPrimitive(PrimitiveSort.Map)) {
          features += ADTOperator
        }
      case StandardOperator.EQ =>
        /* TODO (Bob): Why is a.getType null here for sys__result? ADding null check to work around it
                (But this is definitely a bug - anywhere I looked sys__result always has a proper type! But in feature
                rainbow scanner it seems to disappear...? AbstractTypeChecker always adds a type to _every_ NameExpression!
         */
        if(op.args.exists(a => a.getType != null && a.getType.isPrimitive(PrimitiveSort.Map))) {
          features += ADTOperator
        }
      case StandardOperator.Size if op.first.getType.isPrimitive(PrimitiveSort.Map) =>
        features += ADTOperator
      case StandardOperator.Instance | StandardOperator.TypeOf =>
        features += Inheritance
      case StandardOperator.RemoveAt =>
        features += ADTFunctions
      case StandardOperator.AddrOf =>
        features += AddrOf
      case StandardOperator.Held | StandardOperator.PVLidleToken | StandardOperator.PVLjoinToken =>
        features += PVLSugar
      case StandardOperator.ValidArray | StandardOperator.ValidMatrix | StandardOperator.NewArray |
           StandardOperator.Values | StandardOperator.Drop =>
        features += Arrays
      case StandardOperator.PostIncr | StandardOperator.PostDecr | StandardOperator.PreIncr | StandardOperator.PreDecr |
           StandardOperator.Assign | StandardOperator.PrependSingle | StandardOperator.AppendSingle | StandardOperator.Empty =>
        features += NotStandardized
      case StandardOperator.ValidPointer | StandardOperator.ValidPointerIndex =>
        features += ValidPointer
      case StandardOperator.Member =>
        features += MemberOf
      case _ =>
    }
  }

  private var bindingExpressionDepth = 0
  private var havePattern: Seq[Boolean] = Seq(false)

  override def visit(binding: BindingExpression): Unit = {
    bindingExpressionDepth += 1
    havePattern :+= false
    if(bindingExpressionDepth >= 2) {
      features += NestedQuantifiers
    }
    super.visit(binding)
    if(binding.binder == Binder.SetComp)
      features += ADTFunctions
    if(binding.binder == Binder.Sum)
      features += Summation
    if((binding.triggers == null || binding.triggers.isEmpty) && !havePattern.last)
      features += QuantifierWithoutTriggers
    bindingExpressionDepth -= 1
    havePattern = havePattern.init
  }

  override def visit(assign: AssignmentStatement): Unit = {
    super.visit(assign)
    assign.location match {
      case name: NameExpression if name.kind == NameExpressionKind.Argument =>
        features += ArgumentAssignment
      case _ =>
    }
  }

  override def visit(invok: MethodInvokation): Unit = {
    super.visit(invok)
    visitBeforeAfter(invok)
    if(invok.getDefinition != null && invok.getDefinition.kind == Method.Kind.Predicate && !getParentNode.isa(StandardOperator.Scale))
      features += UnscaledPredicateApplication
    if(invok.`object` == null && invok.dispatch == null)
      features += NotStandardized
  }

  override def visit(deref: expr.Dereference): Unit = {
    super.visit(deref)
    features += Dereference
  }

  override def visit(name: NameExpression): Unit = {
    super.visit(name)
    if(name.kind == NameExpressionKind.Reserved) name.reserved match {
      case ASTReserved.Null => features += Null
      case ASTReserved.This =>
        features += This
      case ASTReserved.CurrentThread => features += CurrentThread
      case ASTReserved.Super => features += NotJavaEncoded
      case _ =>
    }
    if(name.kind == NameExpressionKind.Field)
      features += NotStandardized
  }

  override def visit(par: OMPParallel): Unit = { super.visit(par); features += OpenMP }
  override def visit(par: OMPParallelFor): Unit = { super.visit(par); features += OpenMP }
  override def visit(sections: OMPSections): Unit = { super.visit(sections); features += OpenMP }
  override def visit(section: OMPSection): Unit = { super.visit(section); features += OpenMP }
  override def visit(par: OMPForSimd): Unit = { super.visit(par); features += OpenMP }
  override def visit(fr: OMPFor): Unit = { super.visit(fr); features += OpenMP }

  override def visit(s: stmt.composite.ParallelAtomic): Unit = { super.visit(s); features += ParallelAtomic }
  override def visit(s: ParallelBarrier): Unit = { super.visit(s); features += ParallelBlocks }
  override def visit(s: ParallelBlock): Unit = { super.visit(s); features += ParallelBlocks }
  override def visit(s: ParallelInvariant): Unit = { super.visit(s); features += ParallelBlocks }
  override def visit(s: ParallelRegion): Unit = { super.visit(s); features += ParallelBlocks }
  override def visit(s: ForEachLoop): Unit = { super.visit(s); features += ParallelBlocks }

  override def visit(t: PrimitiveType): Unit = {
    super.visit(t)
    t.sort match {
      case PrimitiveSort.Pointer => features += Pointers
      case _ =>
    }
  }

  override def visit(t: TypeExpression): Unit = {
    super.visit(t)
    features += TypeExpressions
  }

  var ifDepth = 0

  override def visit(fi: IfStatement): Unit = {
    ifDepth += 1
    super.visit(fi)
    if(fi.getCount == 2 && fi.getGuard(0).isReserved(ASTReserved.Any))
      features += NondetCondition
    ifDepth -= 1
  }

  override def visit(block: BlockStatement): Unit = {
    if(block.getStatements.lastIndexWhere(_.isInstanceOf[DeclarationStatement])
        > block.getStatements.indexWhere(!_.isInstanceOf[DeclarationStatement]))
      features += ScatteredDeclarations

    var specIgnoreDepth = 0
    block.asScala.foreach {
      case s: ASTSpecial if s.kind == ASTSpecial.Kind.SpecIgnoreStart =>
        specIgnoreDepth += 1
      case s: ASTSpecial if s.kind == ASTSpecial.Kind.SpecIgnoreEnd =>
        specIgnoreDepth -= 1
      case other =>
        if(specIgnoreDepth == 0) {
          other.accept(this)
        }
    }
  }


  override def visit(decl: DeclarationStatement): Unit = {
    super.visit(decl)
    if(ifDepth > 0) {
      features += DeclarationsInIf
    }
  }

  override def visit(block: stmt.composite.VectorBlock): Unit = {
    super.visit(block)
    features += VectorBlock
  }

  override def visit(pat: expr.InlineQuantifierPattern): Unit = {
    super.visit(pat)
    features += InlineQuantifierPattern
    havePattern = havePattern.init :+ true
  }

  override def visit(lemma: stmt.composite.Lemma): Unit = {
    super.visit(lemma)
    features += Lemma
  }

  override def visit(switch: vct.col.ast.stmt.composite.Switch): Unit = {
    super.visit(switch)
    features += Switch
  }

  override def visit(returnStatement: vct.col.ast.stmt.terminal.ReturnStatement): Unit = {
    super.visit(returnStatement)
    features += Return
  }

  override def visit(tryCatch: vct.col.ast.stmt.composite.TryCatchBlock): Unit = {
    super.visit(tryCatch)
    features += Exceptions
    features += Inheritance
    if (tryCatch.after != null) {
      features += Finally
    }
  }

  override def visit(signals: vct.col.ast.stmt.decl.SignalsClause): Unit = {
    super.visit(signals)
    features += Exceptions
  }

  override def visit(synchronized: vct.col.ast.stmt.composite.Synchronized): Unit = {
    super.visit(synchronized)
    features += Synchronized
  }
}

object Feature {
  def scan(source: ProgramUnit): Set[Feature] = {
    val scanner = new RainbowVisitor(source)
    source.accept(scanner)
    scanner.features.toSet
  }

  val ALL: Set[Feature] = Set(
    MethodAnnotations,
    TypeExpressions,
    TopLevelDeclarations,
    TopLevelFields,
    SpecIgnore,
    MultiDecls,
    UnresolvedTypeInference,
    ExpressionStatement,
    ActionHeader,
    ImproperlySortedBeforeAfter,
    SubscriptRange,
    Dereference,
    Inheritance,
    Null,
    This,
    JavaAtomic,
    CurrentThread,
    ValidPointer,
    ArgumentAssignment,
    BeforeAfter,
    ADTFunctions,
    ADTOperator,
    GivenYields,
    StaticFields,
    InlinePredicate,
    KernelClass,
    AddrOf,
    OpenMP,
    ParallelBlocks,
    ParallelAtomic,
    Pointers,
    ContextEverywhere,
    PureImperativeMethods,
    PVLSugar,
    NondetCondition,
    ScatteredDeclarations,
    Arrays,
    ComplexSubscript,
    AnySubscript,
    UnscaledPredicateApplication,
    NotStandardized,
    Constructors,
    VectorBlock,
    NonVoidMethods,
    NestedQuantifiers,
    DeclarationsInIf,
    InlineQuantifierPattern,
    QuantifierWithoutTriggers,
    Summation,
    Lemma,
    NotJavaEncoded,
    Switch,
    ImplicitLabels,
    Break, // TODO (Bob): Just an idea, but: maybe we also want to be able to put whole ast nodes as features? Would save the next 6 declarations (but might be brittle and fraught with implicit assumptions, so explicit might be better. Dunno)
    Continue, // TODO (Bob): TBH the above idea gets better once you get language specific ast nodes and col specific ast nodes...
    Return,
    Goto,
    Exceptions,
    Finally,
    ExcVar,
    Synchronized,
    ClassWithoutConstructor,
    LockInvariant,
    StringClass,
    MemberOf,

    NotFlattened,
    BeforeSilverDomains,
    NullAsOptionValue,
    NotOptimized,
    DeclarationsNotLifted,
    UnusedExtern,
    ParallelLocalAssignmentNotChecked,
    NotJavaResolved,

    NeedsSatCheck,
    NeedsAxiomCheck,
    NeedsDefinedCheck,
    NeedsHistoryCheck,
  )
  val DEFAULT_INTRODUCE: Set[Feature] = Set(
    // I think we'll need those (histories need them boxed in set_field, get_field)
    Dereference,
    Null,
    // (sometimes implicit) this value, probably fair to introduce by default (e.g. invokations)
    This,
    // Declarations that are not at the top of a block; we probably introduce those.
    ScatteredDeclarations,
    // For RewriteArrayRef; kinda ugly that we rewrite Susbcript -> OptGet Subscript Deref -> OptGet loc Deref
    Arrays,
    // Anything other than a name or constant as a subscript is "complicated"
    ComplexSubscript,
    // Supposedly you can't have a predicate application without a scale in front in silicon...
    UnscaledPredicateApplication,
    // Nice to have, should be done somewhere at the end.
    NonVoidMethods,
    // Passes should be able to introduce complex expressions
    NotFlattened,
    // Whole bunch of primitive types
    BeforeSilverDomains,
    // Sometimes stuff gets quantified, so you'd have to know the shape of parent expressions
    NestedQuantifiers,
    // Knowledge about parents etc.
    DeclarationsInIf,
    // Very useful to not repeat yourself when generating quantifiers
    InlineQuantifierPattern,
  )
  val NO_POLY_INTRODUCE: Set[Feature] = DEFAULT_INTRODUCE -- Set(
    This,
  )
  val EXPR_ONLY_INTRODUCE: Set[Feature] = NO_POLY_INTRODUCE -- Set(
    Arrays,
    NonVoidMethods,
    DeclarationsInIf,
    ScatteredDeclarations,
  )
  val DEFAULT_PERMIT: Set[Feature] = Set(
    MethodAnnotations,
    // Many passes just do .isPrimitive(x)
    // TypeExpressions,
    // currently largely unsupported (but we should), so most passes only put stuff in classes
    // TopLevelDeclarations,
    // TopLevelFields,
    // this is stateful and hard to deal with
    // SpecIgnore,
    MultiDecls,
    UnresolvedTypeInference,
    ExpressionStatement,
    ActionHeader,
    ImproperlySortedBeforeAfter,
    SubscriptRange,
    Dereference,
    Inheritance,
    Null,
    This,
    JavaAtomic,
    CurrentThread,
    ValidPointer,
    // This is OK in our frontends, but silver does not accept it, so it's probably best to disallow it in COL by
    // default
    // ArgumentAssignment,
    BeforeAfter,
    ADTFunctions, ADTOperator,
    GivenYields,
    StaticFields,
    InlinePredicate,
    KernelClass,
    AddrOf,
    OpenMP,
    ParallelBlocks,
    ParallelAtomic,
    Pointers,
    ContextEverywhere,
    PureImperativeMethods,
    // Front-end stuff
    // PVLSugar,
    NondetCondition,
    ScatteredDeclarations,
    Arrays,
    ComplexSubscript,
    AnySubscript,
    UnscaledPredicateApplication,
    // Incorrect method invokations are corrected here
    // NotStandardized,
    Constructors,
    VectorBlock,
    NonVoidMethods,
    NotFlattened,
    BeforeSilverDomains,
    NestedQuantifiers,
    DeclarationsInIf,
    InlineQuantifierPattern,
    QuantifierWithoutTriggers,
    Lemma,
    Summation,
    ClassWithoutConstructor,
    LockInvariant,
    StringClass,
    NotOptimized,
    // NotJavaEncoded,
    // NotJavaResolved,
    ParallelLocalAssignmentNotChecked,
    DeclarationsNotLifted,
    UnusedExtern,
    // Incomplete typing etc.
    // NullAsOptionValue,

    // (Bob) I think most passes ignore this anyway?
    Goto,
    Break,
    Continue,
    Switch,
    Return,
    ExcVar,
    ImplicitLabels,
    Exceptions,
    Finally,
    Synchronized,
    MemberOf,
  )
  val EXPR_ONLY_PERMIT: Set[Feature] = DEFAULT_PERMIT ++ Set(
    TopLevelDeclarations,
    TopLevelFields,
  )
  val OPTION_GATES: Set[Feature] = Set(
    NeedsSatCheck,
    NeedsAxiomCheck,
    NeedsDefinedCheck,
    NeedsHistoryCheck,
  )
}

sealed trait Feature
sealed trait ScannableFeature extends Feature
sealed trait GateFeature extends Feature

case object MethodAnnotations extends ScannableFeature
case object TypeExpressions extends ScannableFeature
case object TopLevelDeclarations extends ScannableFeature
case object TopLevelFields extends ScannableFeature
case object SpecIgnore extends ScannableFeature
case object MultiDecls extends ScannableFeature
case object UnresolvedTypeInference extends ScannableFeature
case object ExpressionStatement extends ScannableFeature // no pass
case object ActionHeader extends ScannableFeature // no pass
case object ImproperlySortedBeforeAfter extends ScannableFeature
case object SubscriptRange extends ScannableFeature // no pass
case object Dereference extends ScannableFeature
case object Inheritance extends ScannableFeature
case object Null extends ScannableFeature
case object This extends ScannableFeature
case object JavaAtomic extends ScannableFeature
case object CurrentThread extends ScannableFeature
case object ValidPointer extends ScannableFeature
case object ArgumentAssignment extends ScannableFeature
case object BeforeAfter extends ScannableFeature
case object ADTFunctions extends ScannableFeature
case object ADTOperator extends ScannableFeature
case object GivenYields extends ScannableFeature
case object StaticFields extends ScannableFeature
case object InlinePredicate extends ScannableFeature
case object KernelClass extends ScannableFeature
case object AddrOf extends ScannableFeature
case object OpenMP extends ScannableFeature
case object ParallelBlocks extends ScannableFeature
case object ParallelAtomic extends ScannableFeature
case object Pointers extends ScannableFeature
case object ContextEverywhere extends ScannableFeature
case object PureImperativeMethods extends ScannableFeature
case object PVLSugar extends ScannableFeature
case object NondetCondition extends ScannableFeature // no pass
case object ScatteredDeclarations extends ScannableFeature
case object Arrays extends ScannableFeature
case object ComplexSubscript extends ScannableFeature // no pass
case object AnySubscript extends ScannableFeature
case object UnscaledPredicateApplication extends ScannableFeature
case object NotStandardized extends ScannableFeature
case object Constructors extends ScannableFeature
case object VectorBlock extends ScannableFeature
case object NonVoidMethods extends ScannableFeature
case object NestedQuantifiers extends ScannableFeature
case object DeclarationsInIf extends ScannableFeature
case object InlineQuantifierPattern extends ScannableFeature
case object QuantifierWithoutTriggers extends ScannableFeature
case object Summation extends ScannableFeature
case object Lemma extends ScannableFeature
case object NotJavaEncoded extends ScannableFeature
case object Switch extends ScannableFeature
case object ImplicitLabels extends ScannableFeature
case object Break extends ScannableFeature
case object Continue extends ScannableFeature
case object Return extends ScannableFeature
case object Goto extends ScannableFeature
case object Exceptions extends ScannableFeature
case object Finally extends ScannableFeature
case object ExcVar extends ScannableFeature
case object Synchronized extends ScannableFeature
case object ClassWithoutConstructor extends ScannableFeature
case object LockInvariant extends ScannableFeature
case object NotJavaResolved extends ScannableFeature
case object StringClass extends ScannableFeature
case object MemberOf extends ScannableFeature

case object NotFlattened extends GateFeature
case object BeforeSilverDomains extends GateFeature
case object NullAsOptionValue extends GateFeature
case object NotOptimized extends GateFeature
case object DeclarationsNotLifted extends GateFeature
case object UnusedExtern extends GateFeature
case object ParallelLocalAssignmentNotChecked extends GateFeature

case object NeedsSatCheck extends GateFeature
case object NeedsAxiomCheck extends GateFeature
case object NeedsDefinedCheck extends GateFeature
case object NeedsHistoryCheck extends GateFeature