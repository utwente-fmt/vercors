package vct.parsers.rewrite;

import java.util.HashMap;

import vct.col.ast.stmt.decl.ASTClass;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.stmt.decl.ASTSpecial.Kind;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.type.ClassType;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.expr.NameExpression;
import vct.col.ast.expr.OperatorExpression;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.type.Type;
import vct.col.ast.util.AbstractRewriter;
import vct.col.ast.syntax.JavaDialect;
import vct.col.ast.syntax.JavaSyntax;

/**
 * Rewrite a Java AST, produced by parsing, to conform to the COL AST standard.  
 */
public class JavaPostProcessor extends AbstractRewriter {

  private int wildcard_count=0;
  
  public JavaPostProcessor(ProgramUnit source) {
    super(source);
  }

  @Override
  public void visit(ASTSpecial s){
    switch(s.kind){
    case ActionHeader:
      Fail("cannot create block around action",s.getOrigin());
      break;
    case Expression:
      result=rewrite(s.args[0]);
      break;
    default:
      super.visit(s);
      break;
    }
  }
  
  @Override
  public void visit(ClassType t){
    String name[]=t.getNameFull();
    if (name.length==1){
      switch(name[0]){
      case "String":
        result=create.primitive_type(PrimitiveSort.String);
        return;
      case "set":
        result=create.primitive_type(PrimitiveSort.Set,rewrite(t.argsJava()));
        return;
      case "loc":
        result=create.primitive_type(PrimitiveSort.Location,rewrite(t.argsJava()));
        return;
      case "bag":
        result=create.primitive_type(PrimitiveSort.Bag,rewrite(t.argsJava()));
        return;
      default:
        super.visit(t);
        return;
      }
    } else {
      super.visit(t);
    }
  }
  
  @Override
  public void visit(ASTClass c){
    super.visit(c);
    ASTClass decl=(ASTClass)result;
    int N=0;
    for(Method m:decl.dynamicMethods()){
      if (m.kind==Method.Kind.Constructor) N++;
    }
    if (N==0 && c.kind!=ASTClass.ClassKind.Interface) create.addZeroConstructor(decl);
  }

  @Override
  public void visit(Method m){
    if (m.getReturnType().isPrimitive(PrimitiveSort.Resource)){
      result=create.predicate(m.getName(), rewrite(m.getBody()), rewrite(m.getArgs()));
    } else {
      super.visit(m);
    }
  }
  
  @Override
  public void visit(MethodInvokation e){
    if (e.object()==null){
      JavaSyntax syntax=JavaSyntax.getJava(JavaDialect.JavaVerCors);
      StandardOperator op=syntax.parseFunction(e.method());
      if (op!=null){
        result=create.expression(op,rewrite(e.getArgs()));
        return;
      }
    }
    MethodInvokation res = create.invokation(rewrite(e.object()), e.dispatch(), e.method(), rewrite(e.getArgs()));
    res.set_before(rewrite(e.get_before()));
    res.set_after(rewrite(e.get_after()));
    result = res;
  }
  
  @Override
  public void visit(BlockStatement b){
    if (b.size()>0 && b.get(0).isSpecial(Kind.ActionHeader)){
      ASTSpecial decl=(ASTSpecial)b.get(0);
      ASTNode history=rewrite(decl.args[0]);
      ASTNode fraction=rewrite(decl.args[1]);
      ASTNode process=rewrite(decl.args[2]);
      ASTNode action=rewrite(decl.args[3]);
      HashMap<String,ASTNode> map=new HashMap<String,ASTNode>();
      for(int i=4;i<decl.args.length;i+=2){
        String field=decl.args[i].toString();
        ASTNode frac=rewrite(decl.args[i+1]);
        map.put(field,frac);
      }
      BlockStatement block=create.block();
      int N=b.size();
      for(int i=1;i<N;i++){
        block.add(rewrite(b.get(i)));
      }
      result=create.action_block(history,fraction,process, action,map,block);
    } else {
      super.visit(b);
    }
  }
}
