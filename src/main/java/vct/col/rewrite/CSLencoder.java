package vct.col.rewrite;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.concurrent.atomic.AtomicInteger;

import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.expr.NameExpression;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.decl.*;
import vct.col.ast.type.ASTReserved;
import vct.col.ast.util.AbstractRewriter;
import vct.logging.ErrorMapping;

public class CSLencoder extends AbstractRewriter {
 
  public CSLencoder(ProgramUnit source, ErrorMapping errorMapping) {
    super(source);
  }
  
  @Override
  public void visit(ASTClass cl){
    String name=cl.getName();
    if (name.startsWith("Atomic")){
      ASTClass res=create.ast_class(name, ASTClass.ClassKind.Plain,null, null,null);
      for(DeclarationStatement decl:cl.dynamicFields()){
        res.add_dynamic(rewrite(decl));
      }
      for(Method m:cl.dynamicMethods()){
        if (m.kind==Method.Kind.Constructor){
          res.add_dynamic(rewrite(m));
        }
      }
      result=res;
    } else {
      super.visit(cl);
    }
  }

  @Override
  public void visit(Method m){
    // TODO PB: disabling this is unsound, but this logic cannot be combined with JavaEncoder: it was already disabled accidentally.
    super.visit(m);
  }
  
  private AtomicInteger count=new AtomicInteger();
  
  @Override
  public void visit(MethodInvokation e){
    Method m=e.getDefinition();
    boolean replace=false;
    ASTNode decl=m.getParent();
    if(decl instanceof ASTClass){
      ASTClass cl=(ASTClass)decl;
      String name=cl.getName();
      if (name.startsWith("Atomic")){
        replace=true;
      }
    }
    if(replace){
      switch(m.getKind()){
      case Constructor:
        super.visit(e);
        return;
      case Plain:
        break;
      default:
        Fail("Atomic classes can only use constructors and plain methods!");
        return;
      }
      int no=count.incrementAndGet();
      String result_name="csl_result_"+no;
      String return_label="csl_return_"+no;
      BlockStatement block=create.block();
      if (!m.getReturnType().isVoid()){
        currentBlock.add(create.field_decl(result_name,rewrite(m.getReturnType())));
      }
      ArrayList<ASTNode> subjects=new ArrayList<ASTNode>();
      for(ASTNode s:e.get_before()){
        if (s.isSpecial(ASTSpecial.Kind.CSLSubject)){
          subjects.add(((ASTSpecial)s).args[0]);
        } else {
          block.add(rewrite(s));
        }
      }
      if (subjects.size()==0){
        //Warning("no explicit subjects for atomic method call.");
        subjects.add(create.reserved_name(ASTReserved.This));
      }
      InlineMethod inline=new InlineMethod(source());
      inline.inline(block,result_name,return_label,m,e.object(),e.getArgs(),e.getOrigin());
      block.add(create.special(ASTSpecial.Kind.Label,create.label(return_label)));
      Hashtable<NameExpression, ASTNode> map=new Hashtable<NameExpression, ASTNode>();
      Substitution sigma=new Substitution(source(),map);
      map.put(create.reserved_name(ASTReserved.Result),create.local_name(result_name));
      for(ASTNode s:e.get_after()){
        block.add(sigma.rewrite(rewrite(s)));
      }
      currentBlock.add(create.csl_atomic(block,subjects.toArray(new ASTNode[0])));
      if (m.getReturnType().isVoid()){
        result=null;
      } else {
        result=create.local_name(result_name);
      }
    } else {
      super.visit(e);
    }
    
  }
  
  
}
