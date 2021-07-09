package vct.col.features

import hre.ast.MessageOrigin
import hre.lang.System.{LogLevel, Output, getLogLevelOutputWriter}
import vct.col.ast.`type`.{ASTReserved, ClassType, PrimitiveSort, PrimitiveType, TypeExpression, TypeOperator, TypeVariable}
import vct.col.ast.stmt
import vct.col.ast.stmt.composite.{BlockStatement, CatchClause, ForEachLoop, IfStatement, LoopStatement, ParallelBarrier, ParallelBlock, ParallelInvariant, ParallelRegion, TryCatchBlock}
import vct.col.ast.stmt.decl.{ASTClass, ASTDeclaration, ASTFlags, ASTSpecial, AxiomaticDataType, Contract, DeclarationStatement, Method, NameSpace, ProgramUnit, VariableDeclaration}
import vct.col.ast.expr.{Binder, BindingExpression, KernelInvocation, MethodInvokation, NameExpression, NameExpressionKind, OperatorExpression, StandardOperator}
import vct.col.ast.expr
import vct.col.ast.expr.constant.{ConstantExpression, StructValue}
import vct.col.ast.util.{AbstractVisitor, RecursiveVisitor, SequenceUtils}
import vct.col.ast.generic.{ASTNode, BeforeAfterAnnotations}
import vct.col.ast.langspecific.c.{OMPFor, OMPForSimd, OMPParallel, OMPParallelFor, OMPSection, OMPSections}
import vct.col.ast.stmt.decl.ASTClass.ClassKind
import vct.col.ast.stmt.terminal.{AssignmentStatement, ReturnStatement}
import vct.col.rewrite.{AddTypeADT, IntroExcVar, PVLEncoder}
import vct.parsers.rewrite.InferADTTypes

import scala.jdk.CollectionConverters._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class RainbowVisitor(source: ProgramUnit) extends RecursiveVisitor(source, true) {
  val features: mutable.Set[Feature] = mutable.Set()
  val blames: mutable.Map[Feature, ArrayBuffer[ASTNode]] = mutable.Map()

  source.asScala.foreach(visitTopLevelDecl)

  if(source.asScala.collectFirst {
    case ax: AxiomaticDataType => ax.name == AddTypeADT.ADT_NAME
  }.isEmpty) {
    addFeature(NoTypeADT, {
      val block = new BlockStatement
      block.setOrigin(new MessageOrigin("(no origin)"))
      block
    })
  }

  def addFeature(feature: Feature, blame: ASTNode): Unit = {
    features += feature
    blames.getOrElseUpdate(feature, ArrayBuffer()) += blame
  }

  private def printBlameNodeExample(node: ASTNode): Unit = {
    vct.col.ast.util.Configuration.getDiagSyntax.print(getLogLevelOutputWriter(LogLevel.Info), node)
    node.getOrigin.report("", "Originating here ^")
    Output("")
    Output("")
  }

  def logBlameExamples(feature: Feature): Unit = {
    val featureBlames = blames(feature).take(3)

    if(featureBlames.isEmpty) {
      throw new IllegalArgumentException()
    } else if(featureBlames.size == 1) {
      Output("Example: %s", featureBlames.head.getClass().getSimpleName)
      printBlameNodeExample(featureBlames.head)
    } else {
      featureBlames.zipWithIndex.foreach { case (node, i) =>
        Output("== Example %d: %s ==", Int.box(i + 1), node.getClass().getSimpleName)
        printBlameNodeExample(node)
      }
    }
  }

  def visitTopLevelDecl(decl: ASTNode): Unit = decl match {
    case ns: NameSpace =>
      ns.asScala.foreach(visitTopLevelDecl)
    case _: DeclarationStatement =>
      addFeature(TopLevelFields, decl)
    case m: Method =>
      if(!isPure(m) && m.getBody != null) {
        addFeature(TopLevelImplementedMethod, decl)
      }
      addFeature(TopLevelMethod, decl)
    case _ =>
  }

  override def visit(v: StructValue): Unit = {
    super.visit(v)
  }

  override def visit(c: ASTClass): Unit = {
    super.visit(c)
    if(c.kind != ClassKind.Record && c.methods().asScala.nonEmpty)
      addFeature(This, c)
    if(c.staticFields().asScala.nonEmpty)
      addFeature(StaticFields, c)
    if(c.kind == ClassKind.Kernel)
      addFeature(KernelClass, c)
    if(c.methods().asScala.exists(m => isPure(m) && m.name == "lock_invariant")) {
      c.methods().asScala.collectFirst {
        case m: Method if m.kind == Method.Kind.Constructor => m
      } match {
        case Some(con) =>
          con.getBody match {
            case null =>
            case block: BlockStatement =>
              val last = block.asScala.filterNot(_.isSpecial(ASTSpecial.Kind.Label)).lastOption match {
                case None => addFeature(NoLockInvariantProof, c)
                case Some(last) =>
                  if(!last.isSpecial(ASTSpecial.Kind.Exhale) ||
                    last.asInstanceOf[ASTSpecial].args(0) == MethodInvokation(NameExpression(null, ASTReserved.This, NameExpressionKind.Reserved), null, "lock_invariant", Seq())) {
                    addFeature(NoLockInvariantProof, c)
                  }
              }
            case _ => addFeature(NoLockInvariantProof, c)
          }
        case None =>
      }
    }
  }

  private def isPure(m: Method): Boolean =
    Set(Method.Kind.Predicate, Method.Kind.Pure).contains(m.kind) ||
      m.annotations().asScala.exists(_.isReserved(ASTReserved.Pure))

  private def isInline(node: ASTNode): Boolean =
    (node.isValidFlag(ASTFlags.INLINE) && node.getFlag(ASTFlags.INLINE)) ||
      node.annotations().asScala.exists(_.isReserved(ASTReserved.Inline))

  private def isFinal(node: ASTNode): Boolean =
    isInline(node) ||
      (node.isValidFlag(ASTFlags.FINAL) && node.getFlag(ASTFlags.FINAL))

  private def isStatic(node: ASTNode): Boolean =
    node.isValidFlag(ASTFlags.STATIC) && node.getFlag(ASTFlags.STATIC)

  var lastMethodStatements: Set[ASTNode] = Set()

  override def visit(m: Method): Unit = {
    var forbidRecursion = false

    if(m.annotated())
      if (m.annotations().size == 1 && m.isSynchronized) {
        addFeature(Synchronized, m)
      } else if(m.annotations().size > 0) {
        addFeature(MethodAnnotations, m)
      }
    /* See CSLEncoder -> MethodInvokation
    if(m.name == "csl_invariant")
      addFeature(JavaAtomic, m)
     */
    if(m.name == PVLEncoder.INV && getParentNode != null && !getParentNode.asInstanceOf[ASTClass].methods().asScala.exists(_.name == PVLEncoder.HELD))
      addFeature(PVLSugar, m)
    if(m.name == "run" && getParentNode != null && !getParentNode.asInstanceOf[ASTClass].methods().asScala.exists(_.name == "forkOperator"))
      addFeature(PVLSugar, m)
    if(m.canThrowSpec) {
      addFeature(Exceptions, m)
    }
    if(m.getContract != null) {
      if(m.getContract.yields.nonEmpty || m.getContract.`given`.nonEmpty)
        addFeature(GivenYields, m)
      if(m.getContract.invariant != Contract.default_true)
        addFeature(ContextEverywhere, m)
      if(m.getContract.pre_condition.isConstant(false)) {
        addFeature(SpecIgnore, m)
        forbidRecursion = true
      }
    }
    if(IntroExcVar.usesExceptionalControlFlow(m) && m.getBody != null) {
      if(m.getBody.asInstanceOf[BlockStatement].isEmpty ||
        !m.getBody.asInstanceOf[BlockStatement].get(0).isInstanceOf[DeclarationStatement] ||
        m.getBody.asInstanceOf[BlockStatement].get(0).asInstanceOf[DeclarationStatement].name != IntroExcVar.excVar) {
        if(m.getArgs.isEmpty || m.getArgument(0) != IntroExcVar.excVar) {
          addFeature(NoExcVar, m)
        }
      }
    }

    if(isPure(m)) {
      if(isInline(m)) {
        if (m.getReturnType.isPrimitive(PrimitiveSort.Resource)) {
          addFeature(InlinePredicate, m)
        } else if (!m.getReturnType.isPrimitive(PrimitiveSort.Process)) {
          addFeature(InlineFunction, m)
        }
      }
      if(m.getBody.isInstanceOf[BlockStatement])
        addFeature(PureImperativeMethods, m)
    }

    if(m.kind == Method.Kind.Pure && m.getReturnType.isPrimitive(PrimitiveSort.Resource))
      addFeature(NotStandardized, m)

    if(m.kind == Method.Kind.Constructor)
      addFeature(Constructors, m)
    if(!m.getReturnType.isPrimitive(PrimitiveSort.Void) && !isPure(m))
      addFeature(NonVoidMethods, m)
    if(getParentNode.isInstanceOf[ASTClass] && !isFinal(m) && !isFinal(getParentNode) && !isStatic(m) && !isPure(m /* TODO: should consider #532 */))
      addFeature(NotJavaEncoded, m)
    if(m.isValidFlag(ASTFlags.EXTERN) && m.getFlag(ASTFlags.EXTERN))
      addFeature(Extern, m)

    if(!forbidRecursion) {
      m.getReturnType.accept(this)
      m.getArgs.foreach(_.accept(this))
      m.signals.foreach(_.accept(this))
      if(m.getContract != null) m.getContract.accept(this)
      if(m.getBody != null) {
        lastMethodStatements = scanLastStatements(m.getBody)
        m.getBody.accept(this)
      }
    }
  }

  def scanLastStatements(statement: ASTNode): Set[ASTNode] = statement match {
    case block: BlockStatement =>
      block.asScala.filterNot(_.isSpecial(ASTSpecial.Kind.Label)).lastOption.map(scanLastStatements).getOrElse(Set())
    case `try`: TryCatchBlock =>
      if(`try`.after != null) {
        scanLastStatements(`try`.after)
      } else {
        `try`.catches.map(c => scanLastStatements(c.block)).foldLeft(Set.empty[ASTNode])(_ ++ _)
      }
    case other =>
      Set(other)
  }

  override def visit(ct: ClassType): Unit = {
    super.visit(ct)
//    if(ct.definition == null)
//      addFeature(NotJavaResolved)
    if(ct.names == Seq("String"))
      addFeature(StringClass, ct)
  }

  override def visit(special: ASTSpecial): Unit = {
    super.visit(special)
    special.kind match {
      case ASTSpecial.Kind.SpecIgnoreStart | ASTSpecial.Kind.SpecIgnoreEnd =>
        addFeature(SpecIgnore, special)
      case ASTSpecial.Kind.Expression =>
        addFeature(ExpressionStatement, special)
      case ASTSpecial.Kind.ActionHeader =>
        addFeature(ActionHeader, special)
      case ASTSpecial.Kind.Send | ASTSpecial.Kind.Recv =>
        addFeature(ParallelBlocks, special)
      case ASTSpecial.Kind.Fork | ASTSpecial.Kind.Join |
           ASTSpecial.Kind.Lock | ASTSpecial.Kind.Unlock |
           ASTSpecial.Kind.Wait | ASTSpecial.Kind.Notify =>
        addFeature(PVLSugar, special)
      case ASTSpecial.Kind.Open | ASTSpecial.Kind.Close =>
        addFeature(NotJavaEncoded, special)
      case ASTSpecial.Kind.Break =>
        addFeature(Break, special)
        if (special.args.length == 0) {
          addFeature(ImplicitLabels, special)
        }
      case ASTSpecial.Kind.Continue =>
        addFeature(Continue, special)
        if (special.args.length == 0) {
          addFeature(ImplicitLabels, special)
        }
      case ASTSpecial.Kind.Goto => addFeature(Goto, special)
      case ASTSpecial.Kind.Throw => addFeature(Exceptions, special)
      case _ =>
    }
  }

  override def visit(decl: VariableDeclaration): Unit = {
    super.visit(decl)
    addFeature(MultiDecls, decl)
  }

  override def visit(t: TypeVariable): Unit = {
    super.visit(t)
    if(t.name == InferADTTypes.typeVariableName)
      addFeature(UnresolvedTypeInference, t)
  }

  def visitBeforeAfter[T <: ASTNode with BeforeAfterAnnotations](node: T): Unit = {
    if(node.get_after() != null && node.get_after().asScala.nonEmpty) {
      addFeature(BeforeAfter, node)
      node.get_after().asScala.foreach {
        case special: ASTSpecial => special.kind match {
          case ASTSpecial.Kind.With | ASTSpecial.Kind.Then | ASTSpecial.Kind.Label =>
            addFeature(ImproperlySortedBeforeAfter, node)
          case _ =>
        }
        case _ =>
      }
    }

    if(node.get_before() != null && node.get_before().asScala.nonEmpty) {
      addFeature(BeforeAfter, node)
      node.get_before().asScala.foreach {
        case special: ASTSpecial => special.kind match {
          case ASTSpecial.Kind.With | ASTSpecial.Kind.Then | ASTSpecial.Kind.Label =>
            addFeature(ImproperlySortedBeforeAfter, node)
          case _ =>
        }
        case _ =>
      }
    }
  }

  override def visit(loop: LoopStatement): Unit = {
    super.visit(loop)
    visitBeforeAfter(loop)

    if(loop.getContract != null &&
      (loop.getContract.pre_condition != Contract.default_true ||
      loop.getContract.post_condition != Contract.default_true)) {
      addFeature(OpenMP, loop)
    }
  }

  override def visit(op: OperatorExpression): Unit = {
    super.visit(op)
    visitBeforeAfter(op)
    op.operator match {
      case StandardOperator.Subscript =>
        if(op.second.isa(StandardOperator.RangeSeq)) {
          addFeature(SubscriptRange, op)
        }
        op.second match {
          case _: NameExpression | _: ConstantExpression =>
          case _ => addFeature(ComplexSubscript, op)
        }
        if(op.second.isReserved(ASTReserved.Any)) {
          addFeature(AnySubscript, op)
        }
        if(op.first.getType.isPrimitive(PrimitiveSort.Map)) {
          addFeature(ADTOperator, op)
        }
      case StandardOperator.EQ =>
        /* TODO (Bob): Why is a.getType null here for sys__result? ADding null check to work around it
                (But this is definitely a bug - anywhere I looked sys__result always has a proper type! But in feature
                rainbow scanner it seems to disappear...? AbstractTypeChecker always adds a type to _every_ NameExpression!
         */
        if(op.args.exists(a => a.getType != null && a.getType.isPrimitive(PrimitiveSort.Map))) {
          addFeature(ADTOperator, op)
        }
      case StandardOperator.Size if op.first.getType.isPrimitive(PrimitiveSort.Map) =>
        addFeature(ADTOperator, op)
      case StandardOperator.Instance | StandardOperator.TypeOf =>
        addFeature(Inheritance, op)
      case StandardOperator.RemoveAt =>
        addFeature(ADTFunctions, op)
      case StandardOperator.AddrOf =>
        addFeature(AddrOf, op)
      case StandardOperator.Held | StandardOperator.PVLidleToken | StandardOperator.PVLjoinToken =>
        addFeature(PVLSugar, op)
      case StandardOperator.ValidArray | StandardOperator.ValidMatrix | StandardOperator.NewArray |
           StandardOperator.Values =>
        addFeature(ArrayOps, op)
      case StandardOperator.Drop =>
        val tInfo = SequenceUtils.getInfo(op.first)
        if(tInfo != null && tInfo.getSequenceSort == PrimitiveSort.Array) {
          addFeature(ArrayOps, op)
        }
      case StandardOperator.PrependSingle | StandardOperator.AppendSingle =>
        addFeature(ADTOperator, op)
      case StandardOperator.Empty =>
        addFeature(NotStandardized, op)
      case StandardOperator.ValidPointer | StandardOperator.ValidPointerIndex =>
        addFeature(ValidPointer, op)
      case StandardOperator.Member =>
        if(op.second.isa(StandardOperator.RangeSeq)) {
          addFeature(MemberOfRange, op)
        }
      case StandardOperator.PostDecr | StandardOperator.PostIncr | StandardOperator.PreDecr | StandardOperator.PreIncr |
           StandardOperator.MulAssign | StandardOperator.FloorDivAssign | StandardOperator.RemAssign |
           StandardOperator.AddAssign | StandardOperator.SubAssign | StandardOperator.ShlAssign |
           StandardOperator.ShrAssign | StandardOperator.SShrAssign | StandardOperator.AndAssign |
           StandardOperator.XorAssign | StandardOperator.OrAssign | StandardOperator.Assign =>
        op.first match {
          case NameExpression(_, _, NameExpressionKind.Argument) =>
            addFeature(ArgumentAssignment, op)
          case _ =>
        }
      case StandardOperator.SeqPermutation =>
        addFeature(ADTOperator, op)
      case StandardOperator.LT | StandardOperator.LTE =>
        if(op.first.getType.isPrimitive(PrimitiveSort.Set) || op.first.getType.isPrimitive(PrimitiveSort.Bag)) {
          addFeature(ADTOperator, op)
        }
      case _ =>
    }
  }

  private var bindingExpressionDepth = 0
  private var havePattern: Boolean = false

  override def visit(binding: BindingExpression): Unit = {
    bindingExpressionDepth += 1
    havePattern = false
    if(bindingExpressionDepth >= 2) {
      addFeature(NestedQuantifiers, binding)
    }
    super.visit(binding)
    if(binding.binder == Binder.SetComp)
      addFeature(ADTFunctions, binding)
    if(binding.binder == Binder.Sum)
      addFeature(Summation, binding)
    if((binding.triggers == null || binding.triggers.isEmpty) && !havePattern)
      addFeature(QuantifierWithoutTriggers, binding)
    bindingExpressionDepth -= 1
  }

  override def visit(assign: AssignmentStatement): Unit = {
    super.visit(assign)
    assign.location match {
      case name: NameExpression if name.kind == NameExpressionKind.Argument =>
        val unsortedWithThen = getAncestor(1).isSpecial(ASTSpecial.Kind.With) || getAncestor(1).isSpecial(ASTSpecial.Kind.Then)
        val sortedWithThen = getAncestor(1).isInstanceOf[MethodInvokation]
        if(!unsortedWithThen && !sortedWithThen)
          addFeature(ArgumentAssignment, assign)
      case _ =>
    }
  }

  override def visit(invok: MethodInvokation): Unit = {
    super.visit(invok)
    visitBeforeAfter(invok)
    if(invok.getDefinition != null && invok.getDefinition.kind == Method.Kind.Predicate &&
      (getParentNode == null || !getParentNode.isa(StandardOperator.Scale)))
      addFeature(UnscaledPredicateApplication, invok)
    if(invok.`object` == null && invok.dispatch == null) {
      if(!features.contains(TopLevelMethod)) {
        // PB: ugly stateful hack: we parse invokation with no object and no dispatch, which must be resolved to a
        // static or instance call. When we allow top level declarations in the chain, no-object no-dispatch invokations
        // instead refer to top level declarations.
        addFeature(NotStandardized, invok)
      }
    }
    if(invok.getDefinition != null && invok.getDefinition.kind == Method.Kind.Constructor && invok.getDefinition.getParent == null) {
      addFeature(ImplicitConstructorInvokation, invok)
    }
    if(invok.getDefinition != null && invok.getDefinition.kind == Method.Kind.Plain) {
      invok.getDefinition.getParent match {
        case cls: ASTClass if cls.name.startsWith("Atomic") =>
          addFeature(JavaAtomic, invok)
        case _ =>
      }
    }
  }

  override def visit(deref: expr.Dereference): Unit = {
    super.visit(deref)
    addFeature(Dereference, deref)
  }

  override def visit(name: NameExpression): Unit = {
    super.visit(name)
    if(name.kind == NameExpressionKind.Reserved) name.reserved match {
      case ASTReserved.Null => addFeature(Null, name)
      case ASTReserved.This =>
        addFeature(This, name)
      case ASTReserved.CurrentThread => addFeature(CurrentThread, name)
      case ASTReserved.Super =>
        // PB: I put this here because I incorrectly assumed that JavaEncode simplifies away super, but instead it
        // introduces it it seems like... I don't think super is dealt with anywhere yet.
        addFeature(NotJavaEncoded, name)
      case _ =>
    }
    if(name.kind == NameExpressionKind.Field)
      addFeature(NotStandardized, name)
  }

  override def visit(par: OMPParallel): Unit = { super.visit(par); addFeature(OpenMP, par) }
  override def visit(par: OMPParallelFor): Unit = { super.visit(par); addFeature(OpenMP, par) }
  override def visit(sections: OMPSections): Unit = { super.visit(sections); addFeature(OpenMP, sections) }
  override def visit(section: OMPSection): Unit = { super.visit(section); addFeature(OpenMP, section) }
  override def visit(par: OMPForSimd): Unit = { super.visit(par); addFeature(OpenMP, par) }
  override def visit(fr: OMPFor): Unit = { super.visit(fr); addFeature(OpenMP, fr) }

  override def visit(s: stmt.composite.ParallelAtomic): Unit = {
    super.visit(s)
    visitBeforeAfter(s)
    addFeature(ParallelAtomic, s)
  }
  override def visit(s: ParallelBarrier): Unit = { super.visit(s); addFeature(ParallelBlocks, s) }
  override def visit(s: ParallelBlock): Unit = { super.visit(s); addFeature(ParallelBlocks, s) }
  override def visit(s: ParallelInvariant): Unit = { super.visit(s); addFeature(ParallelBlocks, s) }
  override def visit(s: ParallelRegion): Unit = {
    super.visit(s)
    addFeature(ParallelBlocks, s)

    if(s.contract != null && s.contract.invariant != Contract.default_true)
      addFeature(ContextEverywhere, s)
  }
  override def visit(s: ForEachLoop): Unit = { super.visit(s); addFeature(ParallelBlocks, s) }

  override def visit(t: PrimitiveType): Unit = {
    super.visit(t)
    t.sort match {
      case PrimitiveSort.Pointer => addFeature(Pointers, t)
      case _ =>
    }
  }

  override def visit(t: TypeExpression): Unit = {
    super.visit(t)
    addFeature(TypeExpressions, t)
    if(t.operator == TypeOperator.Extern)
      addFeature(Extern, t)
  }

  var ifDepth = 0

  override def visit(fi: IfStatement): Unit = {
    ifDepth += 1
    super.visit(fi)
    if(fi.getCount == 2 && fi.getGuard(0).isReserved(ASTReserved.Any))
      addFeature(NondetCondition, fi)
    ifDepth -= 1
  }

  override def visit(block: BlockStatement): Unit = {
    val lastDeclaration = block.getStatements.lastIndexWhere(_.isInstanceOf[DeclarationStatement])
    val firstStatement = block.getStatements.indexWhere(!_.isInstanceOf[DeclarationStatement])
    if(firstStatement != -1 && lastDeclaration > firstStatement)
      addFeature(ScatteredDeclarations, block)

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
    if(decl.isValidFlag(ASTFlags.STATIC) && decl.isStatic && getParentNode != null && getParentNode.isInstanceOf[ASTClass])
      addFeature(NotJavaEncoded, decl)
    if(ifDepth > 0 && !getParentNode.isInstanceOf[BindingExpression])
      addFeature(DeclarationsInIf, getParentNode)
  }

  override def visit(block: stmt.composite.VectorBlock): Unit = {
    super.visit(block)
    addFeature(VectorBlock, block)
  }

  override def visit(pat: expr.InlineQuantifierPattern): Unit = {
    super.visit(pat)
    addFeature(InlineQuantifierPattern, pat)
    havePattern = true
  }

  override def visit(lemma: stmt.composite.Lemma): Unit = {
    super.visit(lemma)
    addFeature(Lemma, lemma)
  }

  override def visit(switch: vct.col.ast.stmt.composite.Switch): Unit = {
    super.visit(switch)
    addFeature(Switch, switch)
  }

  override def visit(returnStatement: vct.col.ast.stmt.terminal.ReturnStatement): Unit = {
    super.visit(returnStatement)
    if(!lastMethodStatements.contains(returnStatement)) {
      addFeature(ExceptionalReturn, returnStatement)
    }
  }

  override def visit(tryCatch: vct.col.ast.stmt.composite.TryCatchBlock): Unit = {
    super.visit(tryCatch)
    addFeature(Exceptions, tryCatch)
    if (tryCatch.after != null) {
      addFeature(Finally, tryCatch)
    }
  }

  override def visit(signals: vct.col.ast.stmt.decl.SignalsClause): Unit = {
    super.visit(signals)
    addFeature(Exceptions, signals)
  }

  override def visit(synchronized: vct.col.ast.stmt.composite.Synchronized): Unit = {
    super.visit(synchronized)
    addFeature(Synchronized, synchronized)
  }

  override def visit(ki: KernelInvocation): Unit = {
    super.visit(ki)
    addFeature(KernelInvocations, ki)
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
    TopLevelImplementedMethod,
    TopLevelMethod,
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
    InlineFunction,
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
    ArrayOps,
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
    ExceptionalReturn,
    Goto,
    Exceptions,
    Finally,
    NoExcVar,
    Synchronized,
    ImplicitConstructorInvokation,
    NoLockInvariantProof,
    StringClass,
    MemberOfRange,
    NoTypeADT,
    Extern,
    KernelInvocations,

    NotFlattened,
    BeforeSilverDomains,
    NullAsOptionValue,
    NotOptimized,
    DeclarationsNotLifted,
    UnusedExtern,
    ParallelLocalAssignmentNotChecked,
    NotJavaResolved,
    InvariantsPropagatedHere,

    NeedsSatCheck,
    NeedsAxiomCheck,
    NeedsDefinedCheck,
    NeedsHistoryCheck,
  )
  val DEFAULT_INTRODUCE: Set[Feature] = Set(
    // (sometimes implicit) this value, probably fair to introduce by default (e.g. invokations)
    This,
    // Declarations that are not at the top of a block; we probably introduce those.
    ScatteredDeclarations,
    // For RewriteArrayRef; kinda ugly that we rewrite Susbcript -> OptGet Subscript Deref -> OptGet loc Deref
    ArrayOps,
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

    // Anything other than a name or constant as a subscript is "complicated"
    ComplexSubscript,
    // I think we'll need those (histories need them boxed in set_field, get_field)
    Dereference,
    Null,
  )
  val NO_POLY_INTRODUCE: Set[Feature] = DEFAULT_INTRODUCE -- Set(
    This,
  )
  val EXPR_ONLY_INTRODUCE: Set[Feature] = NO_POLY_INTRODUCE -- Set(
    ArrayOps,
    NonVoidMethods,
    DeclarationsInIf,
    ScatteredDeclarations,
    ExceptionalReturn,
  )
  val DEFAULT_PERMIT: Set[Feature] = Set(
    MethodAnnotations,
    // Many passes just do .isPrimitive(x)
    // TypeExpressions,
    // currently largely unsupported (but we should), so most passes only put stuff in classes
    // TopLevelDeclarations,
    // TopLevelFields,
    TopLevelMethod,
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
    InlineFunction,
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
    ArrayOps,
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
    ImplicitConstructorInvokation,
    NoLockInvariantProof,
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
    ExceptionalReturn,
    NoExcVar,
    ImplicitLabels,
    Exceptions,
    Finally,
    Synchronized,
    MemberOfRange,
    NoTypeADT,
    InvariantsPropagatedHere,
    Extern,
    KernelInvocations,
  )
  val EXPR_ONLY_PERMIT: Set[Feature] = DEFAULT_PERMIT ++ Set(
    TopLevelImplementedMethod,
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
case object TopLevelImplementedMethod extends ScannableFeature
case object TopLevelMethod extends ScannableFeature
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
case object InlineFunction extends ScannableFeature
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
case object ArrayOps extends ScannableFeature
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
case object ExceptionalReturn extends ScannableFeature
case object Goto extends ScannableFeature
case object Exceptions extends ScannableFeature
case object Finally extends ScannableFeature
case object NoExcVar extends ScannableFeature
case object Synchronized extends ScannableFeature
case object ImplicitConstructorInvokation extends ScannableFeature
case object NoLockInvariantProof extends ScannableFeature
case object NotJavaResolved extends ScannableFeature
case object StringClass extends ScannableFeature
case object MemberOfRange extends ScannableFeature
case object NoTypeADT extends ScannableFeature
case object Extern extends ScannableFeature
case object KernelInvocations extends ScannableFeature

case object NotFlattened extends GateFeature
case object BeforeSilverDomains extends GateFeature
case object NullAsOptionValue extends GateFeature
case object NotOptimized extends GateFeature
case object DeclarationsNotLifted extends GateFeature
case object UnusedExtern extends GateFeature
case object ParallelLocalAssignmentNotChecked extends GateFeature
case object InvariantsPropagatedHere extends GateFeature

case object NeedsSatCheck extends GateFeature
case object NeedsAxiomCheck extends GateFeature
case object NeedsDefinedCheck extends GateFeature
case object NeedsHistoryCheck extends GateFeature