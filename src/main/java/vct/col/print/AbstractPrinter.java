// -*- tab-width:2 ; indent-tabs-mode:nil -*-
package vct.col.print;

import hre.ast.MessageOrigin;
import hre.ast.Origin;
import hre.ast.TrackingOutput;
import hre.lang.HREError;
import vct.col.ast.expr.OperatorExpression;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.expr.constant.ConstantExpression;
import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.expr.NameExpression;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.Hole;
import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.type.ASTReserved;
import vct.col.ast.type.PrimitiveType;
import vct.col.ast.type.TypeExpression;
import vct.col.ast.util.AbstractVisitor;
import vct.col.syntax.Syntax;
import static hre.lang.System.*;

/**
 * This class contains the generic code for pretty printing expressions
 * based on a given syntax.
 * 
 * This class will use the precedences supplied by the syntax to minimize the
 * number of parenthesis.
 * 
 * @author sccblom
 *
 */
public class AbstractPrinter extends AbstractVisitor<Object> {

  protected Syntax syntax;
  protected int current_precedence;
  protected TrackingOutput out;
  protected boolean in_expr;
  protected int expr_level;

  // use expression mode until exit from current visit
  public void setExpr(){
    if (!in_expr){
      in_expr = true;
      expr_level=1;
    }
  }
  // use inline mode in next accept call
  public void nextExpr(){
    if (!in_expr){
      in_expr = true;
      expr_level=0;
    }
  }
  
  private static final Origin missing=new MessageOrigin("unknown location");
  
  public void pre_visit(ASTNode node){
    super.pre_visit(node);
    if (in_expr) {
      expr_level++;
    }
    Origin o=node.getOrigin();
    if (o==null){
      //throw new Error("found "+node.getClass()+" without origin");
      o=missing;
    }
    out.enter(node.getOrigin());
  }

  public void post_visit(ASTNode node){
    out.leave(node.getOrigin());
    if (in_expr) {
      expr_level--;
      in_expr=(expr_level>0);
    }
    super.post_visit(node);
  }

  public AbstractPrinter(Syntax syntax,TrackingOutput out){
    this.syntax=syntax;
    this.out=out;
    current_precedence=0;
  }

  @Override
  public void visit(TypeExpression t){
    switch (t.operator()) {
    case Extern:
      out.printf("extern ");
      t.firstType().apply(this);
      break;
    case Static:
      out.printf("static ");
      t.firstType().apply(this);
      break;
    case Const:
      out.printf("const ");
      t.firstType().apply(this);
      break;
    case Kernel:
      out.printf("__kernel ");
      t.firstType().apply(this);
      break;
    case Global:
      out.printf("__global ");
      t.firstType().apply(this);
      break;
    case Local:
      out.printf("__local ");
      t.firstType().apply(this);
      break;
    case Unsigned:
      out.printf("unsigned ");
      t.firstType().apply(this);
      break;
    case PointerTo:
      t.firstType().apply(this);
      out.printf("*");
      break;
    default:
      throw new HREError("Missing case: %s", t.operator());
    }
  }
  
  public void visit(Hole hole){
    out.printf("[.]");  
  }
  
  public void visit(PrimitiveType t){
    String s=syntax.getPrimitiveType(t.sort);
    if (s==null) throw new Error("unsupported primitive type: "+t.sort);
    out.printf(s);
  }

  public void visit(NameExpression e){
    ASTReserved word=e.reserved();
    if (word==null){
      out.print(e.getName());
    } else {
      String s=syntax.getSyntax(word);
      if (s==null) {
        throw Failure("reserved word %s not part of langauge",word);
      }
      out.print(s);
    }
    if (!in_expr) {
      out.lnprintf(";");
    }
  }

  public void visit(MethodInvokation e){
    //boolean statement=!in_expr;
    setExpr();
    if (e.object!=null) {
      // TODO: manage precedence properly.
      e.object.accept(this);
      out.printf(".");
    }
    out.printf("%s",e.method);
    if (e.dispatch!=null){
      out.printf("@");
      e.dispatch.accept(this);
    }
    out.printf("(");
    int N=e.getArity();
    if(N>0){
      int precedence=current_precedence;
      current_precedence=0;
      e.getArg(0).accept(this);
      for(int i=1;i<N;i++){
        out.print(",");
        current_precedence=0;
        e.getArg(i).accept(this);
      }
      current_precedence=precedence;
    }
    out.print(")");
  }

  public void visit(OperatorExpression e){
    StandardOperator op=e.operator();
    String op_syntax[]=syntax.getSyntax(op);
    if (op_syntax==null){
      throw new Error("no syntax defined for "+op+" operation");
    }
    int N=op.arity();
    ASTNode args[]=e.argsJava().toArray(new ASTNode[0]);
    setExpr();    
    if (N<0){
      out.print(op_syntax[0]);
      if(args.length>0){
        args[0].accept(this);
      }
      for(int i=1;i<args.length;i++){
        out.print(op_syntax[1]);
        args[i].accept(this);
      }      
      out.print(op_syntax[2]);
    } else {
      if (op_syntax[0]!=null && op_syntax[0].length()>0) out.printf("%s ",op_syntax[0]);
      for(int i=0;i<N;i++){
        if (i>0) out.printf(" %s ",op_syntax[i]);
        args[i].accept(this);
      }
      if (op_syntax[0]!=null && op_syntax[N].length()>0) out.printf(" %s",op_syntax[N]);
    }
  }

  public void visit(ConstantExpression ce){
    if (!in_expr) Abort("constant %s outside of expression for %s",ce,ce.getOrigin());
    out.print(ce.toString());
  }
  
  public void visit(ASTSpecial s){
    switch(s.kind){
    case Comment:
      String lines[]=s.args[0].toString().split("\n");
      for(int i=0;i<lines.length;i++){
        out.println(lines[i]);
      }
      break;
    case Pragma:
      out.printf("@pragma(\"%s\")%n", s.args[0]);
      break;
    case Modifies:
      out.println("modifies ...");
      break;
    case Accessible:
      out.println("accessible ...");
      break;
    default:
      if (s.args.length==0){
        out.printf("%s;%n",s.kind);
      } else {
        setExpr();
        out.printf("%s ",s.kind);
        if (s.args[0]==null) {
          out.print("null");
        } else {
          s.args[0].accept(this);
        }
        for (int i=1; i<s.args.length;i++){
          out.printf(", ");
          if (s.args[i]==null) {
            out.print("null");
          } else {
            s.args[i].accept(this);
          }
        }
        out.println(";");
      }
      break;
    }
  }
}

