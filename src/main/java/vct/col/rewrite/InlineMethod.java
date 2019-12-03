package vct.col.rewrite;

import hre.ast.BranchOrigin;
import hre.ast.Origin;

import java.util.Hashtable;
import java.util.concurrent.atomic.AtomicInteger;

import vct.col.ast.expr.NameExpression;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.stmt.decl.ASTSpecial.Kind;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.stmt.terminal.ReturnStatement;
import vct.col.ast.type.ASTReserved;
import vct.col.util.OriginWrapper;

public class InlineMethod extends Substitution {

  private String return_name;
  private String return_label;
  private String prefix;
  
  public InlineMethod(ProgramUnit source) {
    super(source, new Hashtable<NameExpression, ASTNode>());
  }

  private static AtomicInteger count=new AtomicInteger();
  
  public void inline(BlockStatement block, String return_name, String return_label, Method m, ASTNode object, ASTNode[] args, Origin source) {
    create.enter();
    BranchOrigin branch=new BranchOrigin("inlined code at "+source,source);
    ASTNode tmp;
    create.setOrigin(m.getOrigin());
    map.clear();
    NameExpression n=create.reserved_name(ASTReserved.This);
    if (n==null) Debug("1");
    if (object==null) Debug("2");
    map.put(n,object);
    prefix="inline_"+count.incrementAndGet()+"_";
    DeclarationStatement decls[]=m.getArgs();
    for(int i=0;i<decls.length;i++){
      tmp=create.field_decl(prefix+decls[i].name(), copy_rw.rewrite(decls[i].getType()));
      OriginWrapper.wrap(null, tmp,branch);
      block.add(tmp);
      tmp=create.assignment(create.local_name(prefix+decls[i].name()), copy_rw.rewrite(args[i]));
      OriginWrapper.wrap(null, tmp,branch);
      block.add(tmp);      
      map.put(create.local_name(decls[i].name()),create.local_name(prefix+decls[i].name()));
    }
    this.return_name=return_name;
    this.return_label=return_label;
    BlockStatement body=(BlockStatement)m.getBody();
    for(ASTNode s:body){
      if (s.isSpecial(Kind.Refute)) continue;
      tmp=rewrite(s);
      OriginWrapper.wrap(null, tmp,branch);
      block.add(tmp);            
    }
    create.leave();
  }
  
  @Override
  public void visit(DeclarationStatement decl){
    //TODO: handle scoping properly!
    String name = decl.name();
    String new_name = prefix + name;
    NameExpression n = create.local_name(decl.name());
    map.put(n, create.local_name(new_name));
    result = create.field_decl(new_name, rewrite(decl.getType()), rewrite(decl.initJava()));
  }

  @Override
  public void visit(ReturnStatement r){
    if (r.getExpression()!=null){
      result=create.block(
          create.assignment(create.local_name(return_name),rewrite(r.getExpression())),
          create.special(ASTSpecial.Kind.Goto,create.label(return_label))
      );
    } else {
      result=create.special(ASTSpecial.Kind.Goto,create.label(return_label));
    }
  }

}
