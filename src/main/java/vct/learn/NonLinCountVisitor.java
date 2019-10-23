package vct.learn;

import java.util.concurrent.atomic.AtomicBoolean;

import vct.col.ast.expr.BindingExpression;
import vct.col.ast.expr.OperatorExpression;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.util.RecursiveVisitor;
import vct.col.util.ASTUtils;

public class NonLinCountVisitor extends TypesCountVisitor {
  
  public NonLinCountVisitor(ProgramUnit pu) {
    super(pu);
  }
  
  @Override
  public void visit(BindingExpression be) {
    super.visit(be);
    switch (be.binder) {
    case Forall:
    case Star: {
      boolean nonlinear = false;
      for (DeclarationStatement d : be.getDeclarations()) {
        if (!nonlinear) {
          nonlinear |= nonLinear(be.select, d.name());
          nonlinear |= nonLinear(be.main, d.name());
        }
      }
      if (nonlinear) {
        specialCounter.count(be.getClass().getName(), be.binder.toString(), "non-linear");
      }
      break;
    }
    default:
      break;
    }
  }
  
  private boolean nonLinear(ASTNode node, String name) {
    final AtomicBoolean res = new AtomicBoolean(false);
    RecursiveVisitor<Boolean> scanner = new RecursiveVisitor<Boolean>((ProgramUnit) null) {
      public void visit(OperatorExpression e) {
        switch (e.operator()) {
        case Mult:
        case Div:
        case Mod:
          if (ASTUtils.find_name(e, name)) {
            res.set(true);
          } else {
            super.visit(e);
          }
          break;
        default:
          super.visit(e);
        }
      }
    };
    node.accept(scanner);
    return res.get();
  }
  
}
