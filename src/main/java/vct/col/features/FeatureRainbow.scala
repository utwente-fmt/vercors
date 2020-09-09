package vct.col.features

import vct.col.ast.`type`.{ASTReserved, PrimitiveSort, PrimitiveType, TypeExpression, TypeVariable}
import vct.col.ast.stmt
import vct.col.ast.stmt.composite.{BlockStatement, ForEachLoop, IfStatement, LoopStatement, ParallelBarrier, ParallelBlock, ParallelInvariant, ParallelRegion}
import vct.col.ast.stmt.decl.{ASTClass, ASTFlags, ASTSpecial, Contract, DeclarationStatement, Method, ProgramUnit, VariableDeclaration}
import vct.col.ast.expr.{Binder, BindingExpression, MethodInvokation, NameExpression, NameExpressionKind, OperatorExpression, StandardOperator}
import vct.col.ast.expr
import vct.col.ast.expr.constant.ConstantExpression
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

  source.asScala.foreach {
    case _: ASTClass =>
    case _ => features += TopLevelDeclarations
  }

  override def visit(c: ASTClass): Unit = {
    super.visit(c)
    if(c.super_classes.nonEmpty || c.implemented_classes.nonEmpty)
      features += Inheritance
    if(c.kind != ClassKind.Record && c.methods().asScala.nonEmpty)
      features += This
    if(c.name.startsWith("Atomic"))
      features += JavaAtomic
    if(c.staticFields().asScala.nonEmpty)
      features += StaticFields
    if(c.kind == ClassKind.Kernel)
      features += KernelClass
  }

  override def visit(m: Method): Unit = {
    super.visit(m)
    if(m.annotated())
      features += MethodAnnotations
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
    }
    if(Set(Method.Kind.Predicate, Method.Kind.Pure).contains(m.kind) && m.isValidFlag(ASTFlags.INLINE) && m.getFlag(ASTFlags.INLINE))
      features += InlinePredicate
    if(m.kind == Method.Kind.Pure && m.getBody.isInstanceOf[BlockStatement])
      features += PureImperativeMethods
    if(m.kind == Method.Kind.Constructor)
      features += Constructors
    if(!m.getReturnType.isPrimitive(PrimitiveSort.Void))
      features += NonVoidMethods
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
      case ASTSpecial.Kind.Given | ASTSpecial.Kind.Yields | ASTSpecial.Kind.Requires | ASTSpecial.Kind.Ensures |
           ASTSpecial.Kind.RequiresAndEnsures | ASTSpecial.Kind.Invariant | ASTSpecial.Kind.Modifies | ASTSpecial.Kind.Accessible =>
        features += ContractStatement
      case ASTSpecial.Kind.Send | ASTSpecial.Kind.Recv =>
        features += ParallelBlocks
      case ASTSpecial.Kind.Fork | ASTSpecial.Kind.Join |
           ASTSpecial.Kind.Lock | ASTSpecial.Kind.Unlock |
           ASTSpecial.Kind.Wait | ASTSpecial.Kind.Notify =>
        features += PVLSugar
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
    if(node.get_after() != null && node.get_after().asScala.exists(
      after => after.isSpecial(ASTSpecial.Kind.Label) || after.isSpecial(ASTSpecial.Kind.With))) {
      features += ImproperlySortedBeforeAfter
    }

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
      case StandardOperator.Instance | StandardOperator.TypeOf =>
        features += Inheritance
      case StandardOperator.RemoveAt =>
        features += ADTOperators
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
      case _ =>
    }
  }

  var bindingExpressionDepth = 0

  override def visit(binding: BindingExpression): Unit = {
    bindingExpressionDepth += 1
    if(bindingExpressionDepth >= 2) {
      features += NestedQuantifiers
    }
    super.visit(binding)
    if(binding.binder == Binder.SetComp)
      features += ADTOperators
    if(binding.triggers == null || binding.triggers.isEmpty)
      features += QuantifierWithoutTriggers
    bindingExpressionDepth -= 1
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
    if(invok.getDefinition.kind == Method.Kind.Predicate && !getParentNode.isa(StandardOperator.Scale))
      features += UnscaledPredicateApplication
  }

  override def visit(deref: expr.Dereference): Unit = {
    super.visit(deref)
    features += Dereference
  }

  override def visit(name: NameExpression): Unit = {
    super.visit(name)
    if(name.kind == NameExpressionKind.Reserved) name.reserved match {
      case ASTReserved.Null => features += Null
      case ASTReserved.This => features += This
      case ASTReserved.CurrentThread => features += CurrentThread
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
    super.visit(block)
    if(block.getStatements.lastIndexWhere(_.isInstanceOf[DeclarationStatement])
        > block.getStatements.indexWhere(!_.isInstanceOf[DeclarationStatement]))
      features += ScatteredDeclarations
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
  }
}

object Feature {
  val ALL: Set[Feature] = Set(
    MethodAnnotations,
    TypeExpressions,
    TopLevelDeclarations,
    SpecIgnore,
    MultiDecls,
    UnresolvedTypeInference,
    ExpressionStatement,
    ActionHeader,
    ImproperlySortedBeforeAfter,
    ContractStatement,
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
    ADTOperators,
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

    NotFlattened,
    BeforeSilverDomains,
    NullAsOptionValue,
  )
  val DEFAULT_INTRODUCE: Set[Feature] = Set(
    // node annotations are mostly used by the parser and resolved early on
    // MethodAnnotations,

    // type expressions are used for const, long, extern and such
    // TypeExpressions,

    // currently largely unsupported, so most passes only put stuff in classes
    // TopLevelDeclarations,

    // why would we add ignored specifications?
    // SpecIgnore,

    // A multi-decl like "int a,b;" is equivalent to "int a; int b" so introducing one is unlikely
    // MultiDecls,

    // Nice to have in the frontend for anonymous sequences and such, but programatically we specify the type
    // UnresolvedTypeInference,

    // Archaic; though maybe nice to support again when we have a more strongly typed AST
    // ExpressionStatement,

    // Archaic (?)
    // ActionHeader,

    // We'll probably manage to put stuff in the right place
    // ImproperlySortedBeforeAfter,

    // Feature from the old parser, where e.g. loop_invariants are loose statements, later attached to a loop contract
    // ContractStatement,

    // Some syntax sugar we don't use in code
    // SubscriptRange,

    // I think we'll need those (histories need them boxed in set_field, get_field)
    Dereference,

    // TODO
    // Inheritance,

    // Hmm, this is here to indicate null-array-values. TODO
    Null,

    // (sometimes implicit) this value, probably fair to introduce by default (e.g. invokations)
    This,

    // If you use a specific name in the frontend, rewrite some stuff.
    // JavaAtomic,

    // Currently only used in the frontend, so let's say we promise not to introduce it for now.
    // CurrentThread,

    // Syntactic sugar we don't use explicitly.
    // ValidPointer,

    // This is OK in our frontends, but silver does not accept it, so it's probably best to disallow it in COL by
    // default
    // ArgumentAssignment,

    // There is no point in introducing ghost state if we can just add regular arguments.
    // BeforeAfter,

    // I think unused programatically for now, but that may very well change.
    // ADTOperators,

    // Pretty much the same argument as BeforeAfter
    // GivenYields,

    // I think introducing global state is fine
    StaticFields,

    // Introducing a definition that is meant to be inlined is strange
    // InlinePredicate,

    // Front-end feature
    // KernelClass,

    // Pointers are reduced to arrays easily and we don't really use pointer features internally
    // AddrOf,

    // Front-end feature, mostly reduced to ParBlock
    // OpenMP,

    // OpenMP makes new parallel blocks, I think that's about it, so let's keep that explicit.
    // ParallelBlocks,
    // ParallelAtomic,

    // Same as AddrOf
    // Pointers,

    // Supported by our silver translation
    ContextEverywhere,

    // Front-end feature
    // PureImperativeMethods,

    // Front-end stuff
    // PVLSugar,

    // if(*): non-deterministic condition. Not used elsewhere
    // NondetCondition,

    // Declarations that are not at the top of a block; we probably introduce those.
    ScatteredDeclarations,

    // For RewriteArrayRef; kinda ugly that we rewrite Susbcript -> OptGet Subscript Deref -> OptGet loc Deref
    Arrays,

    // Anything other than a name or constant as a subscript is "complicated"
    ComplexSubscript,

    // Don't need the sugar
    // AnySusbcript,

    // Supposedly you can't have a predicate application without a scale in front in silicon...
    UnscaledPredicateApplication,

    // Minor rewrites
    // NotStandardized,

    // Useful for class generation and whatnot
    Constructors,

    // Simple reduction
    // VectorBlock,

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

    // Our passes should always specify correct triggers in new quantified statements
    // QuantifierWithoutTriggers,
  )
  val DEFAULT_PERMIT: Set[Feature] = Set(
    // transfered by post_visit in AbstractRewriter automatically
    MethodAnnotations,

    // type expressions are used for const, long, extern and such
    TypeExpressions,

    // currently largely unsupported, so most passes only put stuff in classes
    // TopLevelDeclarations,

    // this is stateful and hard to deal with
    // SpecIgnore,

    // Scoping is dealt with generally, so should be fine.
    MultiDecls,

    // Weird types are generally fine
    UnresolvedTypeInference,

    // Shouldn't hurt
    ExpressionStatement,

    // Shouldn't hurt
    ActionHeader,

    // Shouldn't hurt, except for lifting passes
    ImproperlySortedBeforeAfter,

    // Proabbly important that contracts are complete
    // ContractStatement,

    // Shouldn't hurt
    SubscriptRange,

    // I think we'll need those (histories need them boxed in set_field, get_field)
    Dereference,

    // TODO
    Inheritance,

    // Hmm, this is here to indicate null-array-values. TODO
    Null,

    // (sometimes implicit) this value, probably fair to introduce by default (e.g. invokations)
    This,

    // If you use a specific name in the frontend, rewrite some stuff.
    JavaAtomic,

    // Shouldn't hurt
    CurrentThread,

    // Shouldn't hurt
    ValidPointer,

    // This is OK in our frontends, but silver does not accept it, so it's probably best to disallow it in COL by
    // default
    // ArgumentAssignment,

    // Shouldn't hurt
    BeforeAfter,

    // Shouldn't hurt
    ADTOperators,

    // Pretty much the same argument as BeforeAfter
    GivenYields,

    // I think introducing global state is fine
    StaticFields,

    // Inlined and un-inlined predicates should be fine
    InlinePredicate,

    // Most passes check that they're dealing with a Plain class, for example
    KernelClass,

    // Shouldn't hurt
    AddrOf,

    // Big feature, so has to
    OpenMP,

    // Big feature, so has to
    ParallelBlocks,
    ParallelAtomic,

    // Shouldn't hurt
    Pointers,

    // Supported by our silver translation
    ContextEverywhere,

    // I think some transformations would make it invalid
    // PureImperativeMethods,

    // Front-end stuff
    PVLSugar,

    // if(*): non-deterministic condition. Not used elsewhere
    NondetCondition,

    // Declarations that are not at the top of a block; we probably introduce those.
    ScatteredDeclarations,

    // For RewriteArrayRef; kinda ugly that we rewrite Susbcript -> OptGet Subscript Deref -> OptGet loc Deref
    Arrays,

    // Anything other than a name or constant as a subscript is "complicated"
    ComplexSubscript,

    AnySubscript,

    // Supposedly you can't have a predicate application without a scale in front in silicon...
    UnscaledPredicateApplication,

    // Minor rewrites
    NotStandardized,

    // Useful for class generation and whatnot
    Constructors,

    // Simple reduction
    VectorBlock,

    // Nice to have, should be done somewhere at the end.
    NonVoidMethods,

    // Complex expressions should be no problem.
    NotFlattened,

    // Reasoning over the simple tyeps is much easier than the converted types
    BeforeSilverDomains,

    // Easy to reason about
    NestedQuantifiers,

    // Nothing special
    DeclarationsInIf,

    InlineQuantifierPattern,
    QuantifierWithoutTriggers,
  )
}

sealed trait Feature
sealed trait ScannableFeature extends Feature
sealed trait GateFeature extends Feature

case object MethodAnnotations extends ScannableFeature // no pass
case object TypeExpressions extends ScannableFeature // no pass
case object TopLevelDeclarations extends ScannableFeature // no pass
case object SpecIgnore extends ScannableFeature // no pass
case object MultiDecls extends ScannableFeature
case object UnresolvedTypeInference extends ScannableFeature
case object ExpressionStatement extends ScannableFeature // no pass
case object ActionHeader extends ScannableFeature // no pass
case object ImproperlySortedBeforeAfter extends ScannableFeature // no pass
case object ContractStatement extends ScannableFeature // no pass
case object SubscriptRange extends ScannableFeature // no pass
case object Dereference extends ScannableFeature
case object Inheritance extends ScannableFeature
case object Null extends ScannableFeature // no pass yet
case object This extends ScannableFeature
case object JavaAtomic extends ScannableFeature
case object CurrentThread extends ScannableFeature
case object ValidPointer extends ScannableFeature
case object ArgumentAssignment extends ScannableFeature
case object BeforeAfter extends ScannableFeature
case object ADTOperators extends ScannableFeature
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

case object NotFlattened extends GateFeature
case object BeforeSilverDomains extends GateFeature
case object NullAsOptionValue extends GateFeature