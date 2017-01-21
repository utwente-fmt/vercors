// -*- tab-width:2 ; indent-tabs-mode:nil -*-
package vct.boogie;

import java.util.HashMap;

import hre.ast.TrackingOutput;
import vct.col.ast.*;
import vct.col.print.AbstractPrinter;
import vct.col.syntax.Syntax;
import vct.col.util.ASTUtils;

/**
 * This class contains a pretty printer for the common part of Boogie and Chalice.
 * 
 * @author Stefan Blom
 */
public abstract class AbstractBoogiePrinter extends AbstractPrinter {
  
  private boolean boogie;
  
  protected boolean in_clause=false;
  protected ASTNode post_condition=null;
  
  public AbstractBoogiePrinter(Syntax syntax,TrackingOutput out,boolean boogie){
    super(syntax,out);
    this.boogie=boogie;
  }
  public void visit(MethodInvokation e){
    String tag;
    if (e.labels()==1){
      tag=e.getLabel(0).getName();
    } else {
      tag="_";
    }
    boolean statement=!in_expr;
    if(!in_expr) {
      out.printf("call ");
    }
    setExpr();
    DeclarationStatement types[]=e.getDefinition().getArgs();
    ASTNode args[]=e.getArgs();
    String next="";
    for(int i=0;i<args.length;i++){
      if (i<types.length&&types[i].isValidFlag(ASTFlags.OUT_ARG)&&types[i].getFlag(ASTFlags.OUT_ARG)) {
        out.printf("%s",next);
        args[i].accept(this);
        next=",";
      }
    }
    for(int i=args.length;i<types.length;i++){
      if (i<types.length&&types[i].isValidFlag(ASTFlags.OUT_ARG)&&types[i].getFlag(ASTFlags.OUT_ARG)) {
        out.printf("%s%s_%s",next,tag,types[i].getName());
        next=",";
      }
    }
    Contract c=e.getDefinition().getContract();
    if (c!=null){
      HashMap<String,ASTNode> yield_map=new HashMap<String,ASTNode>();
      BlockStatement block=e.get_after();
      if (block!=null){
        int N=block.getLength();
        for(int i=0;i<N;i++){
          ASTNode s=block.getStatement(i);
          if (s instanceof AssignmentStatement){
            AssignmentStatement a=(AssignmentStatement)s;
            ASTNode loc=a.location();
            ASTNode val=a.expression();
            if (val instanceof NameExpression && loc instanceof NameExpression){
              yield_map.put(((NameExpression)val).getName(),loc);
            }
          }
        }
      }
      for(int i=0;i<c.yields.length;i++){
        String name=c.yields[i].getName();
        ASTNode loc=yield_map.get(name);
        if (loc==null){
          out.printf("%s%s_%s",next,tag,name);
        } else {
          out.print(next);
          nextExpr();
          loc.accept(this);
        }
        next=",";        
      }
    }
    if (next.equals(",")) {
      out.printf(" := ");
    }
    if (e.object!=null && !boogie){
      e.object.accept(this);
      out.printf(".");
    }
    out.printf("%s(",e.method);
    next="";
    HashMap<String,ASTNode> map=new HashMap<String,ASTNode>();
    for(int i=0;i<args.length;i++){
      if (args[i].labels()>0) {
        if (i>=types.length || types[i].getInit()!=null){
          map.put(args[i].getLabel(0).getName(),args[i]);
          continue;
        }
      }
      if (i<types.length&&types[i].isValidFlag(ASTFlags.OUT_ARG)&&types[i].getFlag(ASTFlags.OUT_ARG)) continue;
      out.printf("%s",next);
      args[i].accept(this);
      next=",";
    }
    for(int i=args.length;i<types.length;i++){
      if (types[i].isValidFlag(ASTFlags.OUT_ARG)&&types[i].getFlag(ASTFlags.OUT_ARG)) continue;
      if (types[i].getInit()==null){
        Fail("Missing argument without default at %s",e.getOrigin());
      }
      out.printf("%s",next);
      types[i].getInit().accept(this);
      next=",";
    }
    if (c!=null){
      BlockStatement before=e.get_before();
      if (before !=null){
        int N=before.getLength();
        for(int i=0;i<N;i++){
          ASTNode item=before.getStatement(i);
          if (item instanceof AssignmentStatement){
            AssignmentStatement arg=(AssignmentStatement)item;
            map.put(((NameExpression)arg.location()).getName(),arg.expression());
          }
        }
      }
      for(int i=0;i<c.given.length;i++){
        ASTNode arg=map.get(c.given[i].getName());
        if (arg==null) Fail("parameter %s is not given at %s",c.given[i].getName(),e.getOrigin());
        out.printf("%s",next);
        arg.accept(this);
        next=",";
      }
    }
    out.printf(")");
    if(statement) {
      out.lnprintf(";");
      if(e.get_after()!=null){
        BlockStatement after=e.get_after();
        int N=after.getLength();
        for(int i=0;i<N;i++){
          ASTNode item=after.getStatement(i);
          if (item instanceof AssignmentStatement){
            AssignmentStatement hint=(AssignmentStatement)item;
            ASTNode loc=hint.location();
            if (!(loc instanceof NameExpression)){
              loc.accept(this);
              out.lnprintf(" := %s_%s;",tag,((NameExpression)hint.expression()).getName());
            }
          }
        }        
      }
    }
  }
  public void visit(AssignmentStatement s){
    if (in_expr) throw new Error("assignment is a statement in chalice");
    nextExpr();
    s.location().accept(this);
    out.printf(" := ");
    nextExpr();
    s.expression().accept(this);
    out.lnprintf(";");
  }
 
  public void visit(BlockStatement s){
    out.lnprintf("{");
    out.incrIndent();
    int N=s.getLength();
    for(int i=0;i<N;i++) s.getStatement(i).accept(this);
    out.decrIndent();
    out.lnprintf("}");
  }
  
  public void visit(Contract contract){
    visit(contract,false);
  }
  
  public void visit(Contract contract,boolean function){
    in_clause=true;
    out.incrIndent();
    
    if (contract.pre_condition.getOrigin()==null) {
      throw new Error("pre condition has no origin");
    }
    for (ASTNode clause:ASTUtils.conjuncts(contract.pre_condition,StandardOperator.And)){
      out.printf("requires ");
      nextExpr();
      current_precedence=0;
      clause.accept(this);
      out.lnprintf(";");
    }
    if(!function){
      if (contract.post_condition.getOrigin()==null) {
        throw new Error("post condition has no origin");
      }
      for (ASTNode clause:ASTUtils.conjuncts(contract.post_condition,StandardOperator.And)){
        out.printf("ensures ");
        nextExpr();
        current_precedence=0;
        clause.accept(this);
        out.lnprintf(";");
      }
    }
    if (contract.modifies!=null){
      out.printf("modifies ");
      nextExpr();
      contract.modifies[0].accept(this);
      for(int i=1;i<contract.modifies.length;i++){
        out.printf(", ");
        nextExpr();
        contract.modifies[i].accept(this);
      }
      out.lnprintf(";");
    }
    out.decrIndent();
    in_clause=false;
  }
  
  public void visit(PrimitiveType t){
    switch (t.sort){
    case Long:
    case Integer:
      out.printf("int");
      break;
    case Void:
      out.printf("void");
      break;
    case Boolean:
      out.printf("bool");
      break;
    default:
      Fail("Primitive type %s is not supported, please use an encoding.",t.sort);
    }
  }

  public void visit(DeclarationStatement s){
    out.printf("var %s : ",s.getName());
    nextExpr();
    s.getType().accept(this);
    out.lnprintf(";");
  }
  
  public void visit(IfStatement s){
    int N=s.getCount();
    for(int i=0;i<N;i++){
      ASTNode g=s.getGuard(i);
      ASTNode gs=s.getStatement(i);
      if(i==0) {
        out.printf("if(");
        nextExpr();
        g.accept(this);
        out.lnprintf(")");
      } else if (i==N-1 && g==IfStatement.elseGuard()) {
        out.lnprintf("else");
      } else {
        out.printf("else if(");
        nextExpr();
        g.accept(this);
        out.lnprintf(")");
      }
      out.incrIndent();
      gs.accept(this);
      out.decrIndent();
    }
  }

  public void visit(LoopStatement s){
    ASTNode init_block=s.getInitBlock();
    ASTNode entry_guard=s.getEntryGuard();
    ASTNode exit_guard=s.getExitGuard();
    ASTNode body=s.getBody();
    if (exit_guard!=null) throw new Error("cannot generate for exit condition yet");
    if (init_block!=null){
      init_block.accept(this);
    }
    if (s.get_before()!=null && s.get_before().size()>0){
      s.get_before().accept(this);
    }
    if (entry_guard!=null) {
      out.printf("while(");
      nextExpr();
      entry_guard.accept(this);
      out.lnprintf(")");
    } else {
      out.lnprintf("while(true)");
    }
    out.incrIndent();
    for(ASTNode inv:s.getInvariants()){
      in_clause=true;
      out.printf("invariant ");
      nextExpr();
      inv.accept(this);
      out.lnprintf(";");
      in_clause=false;
    }
    out.decrIndent();
    if (body instanceof BlockStatement) {
      body.accept(this);
    } else {
      out.lnprintf("{");
      out.incrIndent();
      body.accept(this);
      out.decrIndent();      
      out.lnprintf("}");
    }
    if (s.get_after()!=null && s.get_after().size() > 0) {
      s.get_after().accept(this);
    }
  }
  public void visit(ASTSpecial e){
    String keyword=null;
    switch(e.kind){
      case Assume:
        if (keyword==null) keyword="assume";
      case Assert:
      {
        if (keyword==null) keyword="assert";
        if (in_expr) Fail("%s is a statement",keyword);
        in_clause=true;
        out.printf("%s ",keyword);
        current_precedence=0;
        setExpr();
        ASTNode prop=e.getArg(0);
        prop.accept(this);
        out.lnprintf(";");
        in_clause=false;
        break;
      }
      default:{
        super.visit(e);
      }
    }
  }

  public void visit(Dereference e){
    if (boogie){
      if (e.object() instanceof ClassType) {
        // TODO: check if really OK.
        out.printf("%s", e.field());
      } else {
        Fail("dereferences are illegal in Boogie programs");
      }
    } else {
      e.object().accept(this);
      out.printf(".%s", e.field());
    }
  }
  public void visit(ReturnStatement s){
    if (s.getExpression()!=null) {
      out.printf("__result := ");
      nextExpr();
      s.getExpression().accept(this);
      out.lnprintf(";");
    }
    if (s.get_after()!=null){
      for(ASTNode n:s.get_after()){
        n.accept(this);
      }
    }
    if (post_condition!=null){
      out.printf("assert ");
      nextExpr();
      in_clause=true;
      post_condition.accept(this);
      out.lnprintf(";");
      in_clause=false;
    }
    out.lnprintf("assume false; // return;");   
  }
  @Override
  public void visit(BindingExpression e){
    String connect=null;
    out.printf("(");
    switch(e.binder){
    case FORALL:
      out.printf("forall ");
      connect=")==>(";
      break;
    case EXISTS:
      out.printf("exists ");
      connect=")&&(";
      break;
    default:
      Abort("missing case: %s",e.binder);
    }
    int N=e.getDeclCount();
    if (N!=1) {
      Abort("multi variable quatification is future work");
    }
    DeclarationStatement decl=e.getDeclaration(0);
    out.printf("%s : ",decl.getName());
    decl.getType().accept(this);
    out.printf("::((");
    if (e.select!=null){
      e.select.accept(this);
      out.printf(connect);
    }
    e.main.accept(this);
    out.printf(")))");
  }

}

