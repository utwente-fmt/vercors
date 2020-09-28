package vct.col.rewrite;

import vct.col.ast.expr.NameExpression;
import vct.col.ast.stmt.decl.ASTFlags;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.expr.OperatorExpression;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.type.ASTReserved;
import vct.col.ast.util.AbstractRewriter;

import java.util.HashMap;
import java.util.Stack;

public class InlinePredicatesRewriter extends AbstractRewriter {

  int count = 0;
  Stack<String> inlinedScalars = new Stack<>();
 
  public InlinePredicatesRewriter(ProgramUnit source) {
    super(source);
  }

  public ASTNode inlineCall(MethodInvokation e, Method def) {
    int N=def.getArity();
    HashMap<NameExpression,ASTNode> map=new HashMap<NameExpression, ASTNode>();
    Substitution sigma=new Substitution(source(),map);
    map.put(create.reserved_name(ASTReserved.This), rewrite(e.object()));
    for(int i=0;i<N;i++){
      map.put(create.unresolved_name(def.getArgument(i)),rewrite(e.getArg(i)));
    }
    ASTNode body=rewrite(def.getBody());
    InlineMarking marker=new InlineMarking(source(),e.getOrigin());
    body.accept(marker);
    return sigma.rewrite(body);
  }

  @Override
  public void visit(MethodInvokation e){
    if (inline(e)){
      result = inlineCall(e, e.getDefinition());
    } else if (!inlinedScalars.empty() && e.getDefinition().getKind().equals(Method.Kind.Predicate)) {
      // Because the previous if branch was false, we know this is a predicate that is not inline
      // Hence, we have to add a scale that scales the predicate according to any earlier encountered scales
      super.visit(e);
      result = create.expression(StandardOperator.Scale,
              getCombinedScalar(null),
              result
              );
    } else {
      super.visit(e);
    }
  }

  protected boolean inline(ASTNode node) {
    if (node instanceof Method) {
      Method def = (Method) node;
      if (def.isValidFlag(ASTFlags.INLINE)){
        return (def.kind==Method.Kind.Predicate || def.kind==Method.Kind.Pure) && def.getFlag(ASTFlags.INLINE);
      } else {
        return false;
      }
    } else if (node instanceof MethodInvokation) {
      MethodInvokation invokation = (MethodInvokation) node;
      return inline(invokation.getDefinition());
    } else if (node instanceof OperatorExpression) {
      OperatorExpression operatorExpression = (OperatorExpression) node;
      if (operatorExpression.operator() == StandardOperator.Scale) {
        return inline(operatorExpression.arg(1));
      }
    }

    return false;
  }

  @Override
  public void visit(Method m){
    if (inline(m)){
      result=null;
    } else {
      super.visit(m);
    }
  }

  /**
   * Returns all the aggregated scalars so far in one multiplication.
   *
   * Given a startNode, it will yield the following AST:
   *
   * (((startNode * scalar1) * scalar2) * ...
   *
   * If no startNode is given, only the scalars appear in the tree.
   *
   * The reason startNode is a possible argument is because of a bug in Viper where if startNode
   * is not at the bottom of a multiplication tree, some type error is triggered in Viper. Ideally,
   * this method would have no arguments, and would just return the multiplication tree of the scalars.
   *
   * @param startNode Node to start the multiplication with. If not available, can be null.
   * @return AST corresponding to: (((startNode * scalar1) * scalar2) * ...
   */
  private ASTNode getCombinedScalar(ASTNode startNode) {
    if (inlinedScalars.empty()) {
      Abort("Cannot combine scalars when stack is empty");
    }

    ASTNode result;
    int startIndex;

    if (startNode == null) {
      result = create.local_name(inlinedScalars.get(0));
      startIndex = 1;
    } else {
      result = startNode;
      startIndex = 0;
    }

    for (int i = startIndex; i < inlinedScalars.size(); i++) {
      result = create.expression(
              StandardOperator.Mult,
              result,
              create.local_name(inlinedScalars.get(i)));
    }

    return result;
  }

  @Override
  public void visit(OperatorExpression e){
    switch(e.operator()){
      case Scale:
        if (inline(e)) {
          String scaleAmountName = "inlineScalar" + count++;

          inlinedScalars.push(scaleAmountName);
          super.visit(e);
          OperatorExpression scaleExpr = (OperatorExpression) result;
          inlinedScalars.pop();

          result = create.let_expr(
                  create.field_decl(scaleAmountName, rewrite(e.arg(0).getType()), scaleExpr.arg(0)),
                  scaleExpr.arg(1)
          );
        } else {
          super.visit(e);
        }
        break;
      case Perm:
        super.visit(e);
        OperatorExpression newPerm = (OperatorExpression) result;
        if (!inlinedScalars.empty()) {
          ASTNode permissionAmount = getCombinedScalar(e.arg(1));
          result = create.expression(StandardOperator.Perm, newPerm.arg(0), permissionAmount);
        }
        break;
      default:
        super.visit(e);
        break;
    }
  }

  @Override
  public void visit(ASTSpecial e) {
    if (e.kind == ASTSpecial.Kind.Fold || e.kind == ASTSpecial.Kind.Unfold) {
      Warning("Folding/unfolding an inline predicate is allowed but not encouraged. See https://github.com/utwente-fmt/vercors/wiki/Resources-and-Predicates#inline-predicates for more info.");
    }
    super.visit(e);
  }
}
