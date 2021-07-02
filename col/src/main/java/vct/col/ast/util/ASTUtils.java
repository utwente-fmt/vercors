package vct.col.ast.util;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import vct.col.ast.expr.*;
import vct.col.ast.expr.constant.BooleanValue;
import vct.col.ast.expr.constant.ConstantExpression;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.composite.LoopStatement;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.stmt.decl.ProgramUnit;

public class ASTUtils {

  public static Iterable<ASTNode> conjuncts(ASTNode e,StandardOperator op,StandardOperator ... ops){
    ArrayList<ASTNode> res=new ArrayList<ASTNode>();
    EnumSet<StandardOperator> allops=EnumSet.of(op,ops);
    scan_ops(res,allops,e);
    return res;
  }

  private static void scan_ops(ArrayList<ASTNode> res, EnumSet<StandardOperator> ops,ASTNode n) {
    if (n instanceof OperatorExpression){
      OperatorExpression e=(OperatorExpression)n;
      if (ops.contains(e.operator())) {
        int N=e.operator().arity();
        for (int i=0;i<N;i++){
          scan_ops(res,ops,e.arg(i));
        }
      } else {
        res.add(e);
      }
    } else {
      if (n instanceof ConstantExpression){
        ConstantExpression ce=(ConstantExpression)n;
        if (ce.value() instanceof BooleanValue && ((BooleanValue)ce.value()).value()){
          // skip true.
          return;
        }
      }
      res.add(n); 
    }
  }

  public static boolean find_name(ASTNode clause,final String name) {
    final AtomicBoolean res=new AtomicBoolean(false);
    RecursiveVisitor<Boolean>scanner=new RecursiveVisitor<Boolean>((ProgramUnit)null){
      public void visit(NameExpression e){
        if (e.getName().equals(name)){
          res.set(true);
        }
      }
    };
    clause.accept(scanner);
    return res.get();
  }

  public static Iterable<ASTNode> reverse(Iterable<ASTNode> conjuncts) {
    ArrayList<ASTNode> res=new ArrayList<ASTNode>();
    for(ASTNode n:conjuncts){
      res.add(0,n);
    }
    return res;
  }

  public static void collectNames(HashSet<NameExpression> names, ASTNode tree) {
    RecursiveVisitor<Boolean> scanner = new RecursiveVisitor<Boolean>((ProgramUnit) null) {
      public void visit(NameExpression n) {
        names.add(n);
      }
    };
    tree.accept(scanner);
  }
  
  public static int countOccurences(String name, ASTNode node) {
    AtomicInteger res = new AtomicInteger(0);
    RecursiveVisitor<Boolean>scanner=new RecursiveVisitor<Boolean>((ProgramUnit)null){
      public void visit(NameExpression e) {
        if(e.getName().equals(name)){ {
          res.incrementAndGet();
        }
        }
      }
    };
    node.accept(scanner);
    return res.get();
  }
  
  public static ASTNode replace(ASTNode a, ASTNode b, ASTNode tree) {
    AbstractRewriter rw = new AbstractRewriter((ProgramUnit)null) {
      public void visit(Dereference d) {
        if(d.equals(a)) {
          result = b;
        } else  {
          super.visit(d);
        }
      }

      public void visit(NameExpression e) {
        if(e.equals(a)) {
          result = b;
        } else  {
          super.visit(e);
        }
      }
      public void visit(OperatorExpression e ) {
        if(e.equals(a)) {
          result = b;
        } else  {
          super.visit(e);
        }
      }
      public void visit(MethodInvokation e ) {
        if(e.equals(a)) {
          result = b;
        } else  {
          super.visit(e);
        }
      }
      @Override
      public void visit(LoopStatement s) {
        if(s.equals(a)) {
          result = b;
        } else  {
          super.visit(s);
        }
      }
      @Override
      public void visit(BlockStatement s) {
        if(s.equals(a)) {
          result = b;
        } else  {
          super.visit(s);
        }
      }
    };
    return rw.rewrite(tree);
  }
  
  public static boolean coversAllNames(Iterable<DeclarationStatement> declarations, ASTNode tree) {
    boolean res = true;
    for(DeclarationStatement declaration: declarations) {
      res &= find_name(tree, declaration.name());
    }
    return res;
  }
}
