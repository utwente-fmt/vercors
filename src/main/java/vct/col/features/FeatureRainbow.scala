package vct.col.features

import vct.col.ast.`type`.{ASTReserved, PrimitiveSort, PrimitiveType, TypeVariable}
import vct.col.ast.stmt.composite.{BlockStatement, ForEachLoop, IfStatement, LoopStatement, ParallelAtomic, ParallelBarrier, ParallelBlock, ParallelInvariant, ParallelRegion}
import vct.col.ast.stmt.decl.{ASTClass, ASTFlags, ASTSpecial, Contract, DeclarationStatement, Method, ProgramUnit, VariableDeclaration}
import vct.col.ast.expr.{Binder, BindingExpression, MethodInvokation, NameExpression, NameExpressionKind, OperatorExpression, StandardOperator}
import vct.col.ast.expr
import vct.col.ast.expr.constant.ConstantExpression
import vct.col.ast.util.{AbstractVisitor, RecursiveVisitor}
import vct.col.ast.generic.BeforeAfterAnnotations
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
    if(m.annotated())
      features += MethodAnnotations
    if(m.name == "csl_invariant")
      features += JavaAtomic
    if(m.name == PVLEncoder.INV && !m.getParent.asInstanceOf[ASTClass].methods().asScala.exists(_.name == PVLEncoder.HELD))
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
  }

  override def visit(special: ASTSpecial): Unit = special.kind match {
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

  override def visit(decl: VariableDeclaration): Unit =
    features += MultiDecls

  override def visit(t: TypeVariable): Unit =
    if(t.name == InferADTTypes.typeVariableName)
      features += UnresolvedTypeInference

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

  override def visit(loop: LoopStatement): Unit =
    visitBeforeAfter(loop)

  override def visit(op: OperatorExpression): Unit = {
    visitBeforeAfter(op)
    op.operator match {
      case StandardOperator.Subscript =>
        if(op.first.isa(StandardOperator.RangeSeq)) {
          features += SubscriptRange
        }
        op.first match {
          case _: NameExpression | _: ConstantExpression =>
          case _ => features += ComplexSubscript
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
           StandardOperator.Subscript | StandardOperator.Values | StandardOperator.Drop =>
        features += Arrays
      case StandardOperator.PostIncr | StandardOperator.PostDecr | StandardOperator.PreIncr | StandardOperator.PreDecr |
           StandardOperator.Assign | StandardOperator.PrependSingle | StandardOperator.AppendSingle | StandardOperator.Empty =>
        features += NotStandardized
      case _ =>
    }
  }

  override def visit(binding: BindingExpression): Unit = {
    if(binding.binder == Binder.SetComp)
      features += ADTOperators
  }

  override def visit(assign: AssignmentStatement): Unit =
    assign.location match {
      case name: NameExpression if name.kind == NameExpressionKind.Argument =>
        features += ArgumentAssignment
    }

  override def visit(invok: MethodInvokation): Unit = {
    visitBeforeAfter(invok)
    if(invok.getDefinition.kind == Method.Kind.Predicate && !invok.getParent.isa(StandardOperator.Scale))
      features += UnscaledPredicateApplication
  }

  override def visit(deref: expr.Dereference): Unit =
    features += Dereference

  override def visit(name: NameExpression): Unit = {
    if(name.kind == NameExpressionKind.Reserved) name.reserved match {
      case ASTReserved.Null => features += Null
      case ASTReserved.This => features += This
      case ASTReserved.CurrentThread => features += CurrentThread
      case _ =>
    }
    if(name.kind == NameExpressionKind.Field)
      features += NotStandardized
  }

  override def visit(par: OMPParallel): Unit = features += OpenMP
  override def visit(par: OMPParallelFor): Unit = features += OpenMP
  override def visit(sections: OMPSections): Unit = features += OpenMP
  override def visit(section: OMPSection): Unit = features += OpenMP
  override def visit(par: OMPForSimd): Unit = features += OpenMP
  override def visit(fr: OMPFor): Unit = features += OpenMP

  override def visit(s: ParallelAtomic): Unit = features += ParallelBlocks
  override def visit(s: ParallelBarrier): Unit = features += ParallelBlocks
  override def visit(s: ParallelBlock): Unit = features += ParallelBlocks
  override def visit(s: ParallelInvariant): Unit = features += ParallelBlocks
  override def visit(s: ParallelRegion): Unit = features += ParallelBlocks
  override def visit(s: ForEachLoop): Unit = features += ParallelBlocks

  override def visit(t: PrimitiveType): Unit = t.sort match {
    case PrimitiveSort.Pointer => features += Pointers
    case _ =>
  }

  override def visit(fi: IfStatement): Unit =
    if(fi.getCount == 2 && fi.getGuard(0).isReserved(ASTReserved.Any))
      features += NondetCondition

  override def visit(block: BlockStatement): Unit =
    if(block.getStatements.lastIndexWhere(_.isInstanceOf[DeclarationStatement])
        > block.getStatements.indexWhere(!_.isInstanceOf[DeclarationStatement]))
      features += ScatteredDeclarations
}

sealed trait Feature

case object MethodAnnotations extends Feature
case object TypeExpressions extends Feature
case object TopLevelDeclarations extends Feature
case object SpecIgnore extends Feature
case object MultiDecls extends Feature
case object UnresolvedTypeInference extends Feature
case object ExpressionStatement extends Feature
case object ActionHeader extends Feature
case object ImproperlySortedBeforeAfter extends Feature
case object ContractStatement extends Feature
case object SubscriptRange extends Feature
case object Dereference extends Feature
case object Inheritance extends Feature
case object Null extends Feature
case object This extends Feature
case object JavaAtomic extends Feature
case object CurrentThread extends Feature
case object ValidPointer extends Feature
case object ArgumentAssignment extends Feature
case object BeforeAfter extends Feature
case object ADTOperators extends Feature
case object GivenYields extends Feature
case object StaticFields extends Feature
case object InlinePredicate extends Feature
case object KernelClass extends Feature
case object AddrOf extends Feature
case object OpenMP extends Feature
case object ParallelBlocks extends Feature
case object Pointers extends Feature
case object ContextEverywhere extends Feature
case object PureImperativeMethods extends Feature
case object PVLSugar extends Feature
case object NondetCondition extends Feature
case object ScatteredDeclarations extends Feature
case object Arrays extends Feature
case object ComplexSubscript extends Feature
case object UnscaledPredicateApplication extends Feature
case object NotStandardized extends Feature