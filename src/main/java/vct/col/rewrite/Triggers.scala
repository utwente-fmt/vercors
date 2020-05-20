package vct.col.rewrite

import vct.col.ast.expr.{Binder, BindingExpression, MethodInvokation, NameExpression, OperatorExpression, StandardOperator}
import vct.col.ast.expr.StandardOperator.{EQ, GT, GTE, LT, LTE, Member, NEQ, Scale, Size, Subscript}
import vct.col.ast.generic.ASTNode
import vct.col.ast.expr
import vct.col.ast.expr.constant.ConstantExpression
import vct.col.ast.stmt.decl.{DeclarationStatement, Method, ProgramUnit}

case class Triggers(override val source: ProgramUnit) extends AbstractRewriter(source) {
  sealed abstract class Trigger(val origin: ASTNode) {

  }

  case class Name(x: String)(origin: ASTNode) extends Trigger(origin)
  case class Invokation(name: String, args: Seq[Trigger])(origin: ASTNode) extends Trigger(origin)
  case class Expression(op: StandardOperator, args: Seq[Trigger])(origin: ASTNode) extends Trigger(origin)
  case class Deref(obj: Trigger, field: String)(origin: ASTNode) extends Trigger(origin)

  def getTriggers(node: ASTNode): (Set[Trigger], Boolean) = node match {
    case invok: MethodInvokation if Set(Method.Kind.Predicate, Method.Kind.Pure).contains(invok.getDefinition.kind) =>
      invok.method match {
        case "alen" =>
          val arg = invok.getArg(0)
          val (childPatterns, allowChild) = getTriggers(arg)
          childPatterns.filter(_.origin == arg) match {
            case Seq() if allowChild => (Invokation("alen", ))
          }
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
        "discard valid triggers and come to an incorrect conclusion.")
      Warning("%s", x)
      new UnsupportedLeaf(node)
  }

  def tryComputeTrigger(decls: Array[DeclarationStatement], cond: ASTNode, body: ASTNode): BindingExpression = {
    val patterns = getTriggers(body)
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
