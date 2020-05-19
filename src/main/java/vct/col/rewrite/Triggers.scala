package vct.col.rewrite

import vct.col.ast.expr.{Binder, BindingExpression, MethodInvokation, NameExpression, OperatorExpression}
import vct.col.ast.expr.StandardOperator.{EQ, GT, GTE, LT, LTE, Member, NEQ, Scale, Size, Subscript}
import vct.col.ast.generic.ASTNode
import vct.col.ast.expr
import vct.col.ast.expr.constant.ConstantExpression
import vct.col.ast.stmt.decl.{DeclarationStatement, Method, ProgramUnit}

case class Triggers(override val source: ProgramUnit) extends AbstractRewriter(source) {
  sealed abstract class NodeStructure(val origin: ASTNode) {

  }

  sealed abstract class SupportedLeaf(origin: ASTNode) extends NodeStructure(origin)
  class Name(name: String, origin: ASTNode) extends SupportedLeaf(origin) {
    override def equals(obj: Any): Boolean = obj match {
      case other: Name => other.name == name
      case _ => false
    }
  }

//  class OtherSupportedLeaf(origin: ASTNode) extends SupportedLeaf(origin)

  class UnsupportedLeaf(origin: ASTNode) extends NodeStructure(origin) {
    override def equals(obj: Any): Boolean = obj match {
      case other: UnsupportedLeaf => other.origin == origin
      case _ => false
    }
  }

  sealed abstract class SupportedComposite(val args: Seq[NodeStructure], origin: ASTNode) extends NodeStructure(origin)
  class SupportedCompositeIgnoreOnChild(arg: NodeStructure, origin: ASTNode) extends SupportedComposite(Seq(arg), origin)
  class OtherSupportedComposite(args: Seq[NodeStructure], origin: ASTNode) extends SupportedComposite(args, origin)

  sealed abstract class UnsupportedComposite(val args: Seq[NodeStructure], origin: ASTNode) extends NodeStructure(origin)
  class RelationalComposite(args: Seq[NodeStructure], origin: ASTNode) extends UnsupportedComposite(args, origin)
  class OtherComposite(args: Seq[NodeStructure], origin: ASTNode) extends UnsupportedComposite(args, origin)



  def getNodeStructure(node: ASTNode): NodeStructure = node match {
    case invok: MethodInvokation if Set(Method.Kind.Predicate, Method.Kind.Pure).contains(invok.getDefinition.kind) =>
      invok.method match {
        case "alen" =>
          new SupportedCompositeIgnoreOnChild(getNodeStructure(invok.getArg(0)), invok)
        case _ =>
          new OtherSupportedComposite(invok.getArgs.map(getNodeStructure), invok)
      }
    case exp: OperatorExpression => exp.operator match {
      case LT | LTE | GT | GTE | EQ | NEQ =>
        new RelationalComposite(exp.args.map(getNodeStructure), exp)
      case Size =>
        new SupportedCompositeIgnoreOnChild(getNodeStructure(exp.args.head), exp)
      case Subscript | Scale | Member =>
        new OtherSupportedComposite(exp.args.map(getNodeStructure), exp)
      case _ =>
        new OtherComposite(exp.args.map(getNodeStructure), exp)
    }
    case deref: expr.Dereference =>
      new SupportedCompositeIgnoreOnChild(getNodeStructure(deref.obj), deref)
    case name: NameExpression =>
      new Name(name.getName, name)
    case binder: BindingExpression =>
      new OtherComposite(Seq(binder.select, binder.main).map(getNodeStructure), binder)
    case _: ConstantExpression =>
      new UnsupportedLeaf(node)
    case x =>
      Warning("The following node was encountered, but we cannot decide whether that is allowed in a trigger. We may " +
        "discard valid triggers and come to an incorrect conclusion.", x)
      new UnsupportedLeaf(node)
  }

  def tryComputeTrigger(decls: Array[DeclarationStatement], cond: ASTNode, body: ASTNode): BindingExpression = {
    val struct = getNodeStructure(body)
    val patterns = struct.getPatterns
  }

  override def visit(expr: BindingExpression): Unit = {
    expr.binder match {
      case Binder.Forall if expr.triggers == null =>
        result = tryComputeTrigger(expr.getDeclarations, rewrite(expr.select), rewrite(expr.main))
      case _ =>
        super.visit(expr)
    }
  }
}
