package vct.col.rewrite;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Hashtable;

import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.expr.NameExpression;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.terminal.AssignmentStatement;
import vct.col.ast.stmt.decl.*;
import vct.col.ast.util.AbstractRewriter;
import vct.col.ast.util.ContractBuilder;

public class GhostLifter extends AbstractRewriter {

  public GhostLifter(ProgramUnit source) {
    super(source);
  }

  @Override
  public void visit(Method m){
    ContractBuilder cb=new ContractBuilder();
    ArrayList<DeclarationStatement> args=new ArrayList<DeclarationStatement>();
    Contract c=m.getContract();
    for(DeclarationStatement arg:m.getArgs()){
      args.add(rewrite(arg));
    }
    Hashtable<String, ASTNode> yielded=new Hashtable<String, ASTNode>();
    if (c!=null){
      for(DeclarationStatement arg:c.given){
        args.add(rewrite(arg));
      }
      cb.requires(rewrite(c.pre_condition));
      for(DeclarationStatement arg:c.yields){
        ASTNode init=rewrite(arg.initJava());
        if (init!=null){
          yielded.put(arg.name(), init);
        }
        arg=create.field_decl(arg.name(), rewrite(arg.getType()));
        arg.setFlag(ASTFlags.OUT_ARG, true);
        args.add(arg);
      }
      cb.ensures(rewrite(c.post_condition));
    }
    ASTNode body=rewrite(m.getBody());
    if (body instanceof BlockStatement){
      BlockStatement block=(BlockStatement)body;
      for(String name:yielded.keySet()){
        block.prepend(create.assignment(create.local_name(name),yielded.get(name)));
      }
    }
    result=create.method_kind(
        m.kind,
        rewrite(m.getReturnType()),
        cb.getContract(), 
        m.name(), 
        args.toArray(new DeclarationStatement[0]), 
        m.usesVarArgs(), 
        body
    );
  }
  
  private int count=0;
  
  @Override
  public void visit(MethodInvokation m){
    ArrayList<ASTNode> args=new ArrayList<ASTNode>();
    BlockStatement before=create.block();
    BlockStatement after=create.block();
    for(ASTNode n:m.getArgs()){
      args.add(rewrite(n));
    }
    HashMap<String,ASTNode> arg_map=new HashMap<String, ASTNode>();
    if (m.get_before()!=null) for(ASTNode n:m.get_before()){
      if (n instanceof AssignmentStatement){
        AssignmentStatement s=(AssignmentStatement)n;
        if (s.location() instanceof NameExpression){
          NameExpression name=(NameExpression)s.location();
          //TODO: make kind checking work
          //if (name.getKind()==NameExpressionKind.Label){
            if (arg_map.containsKey(name.getName())){
              Fail("%s is assigned twice",name.getName());
            }
            arg_map.put(name.getName(), rewrite(s.expression()));
            continue;
          //}
        }
      }
      before.add(rewrite(n));
    }
    if (m.get_after()!=null) for(ASTNode n:m.get_after()){
      if (n instanceof AssignmentStatement){
        AssignmentStatement s=(AssignmentStatement)n;
        if (s.expression() instanceof NameExpression){
          NameExpression name=(NameExpression)s.expression();
          //TODO: make kind checking work
          //if (name.getKind()==NameExpressionKind.Label){
            if (arg_map.containsKey(name.getName())){
              Fail("%s is assigned twice",name.getName());
            }
            arg_map.put(name.getName(), rewrite(s.location()));
            continue;
          //}
        }
      }
      after.add(rewrite(n));
    }
    Method def=m.getDefinition();
    Contract c=def.getContract();
    if (c!=null){
      if(c.given!=null) for(DeclarationStatement decl:c.given){
        ASTNode arg = arg_map.get(decl.name());
        if (arg==null){
          arg=rewrite(decl.initJava());
        }
        if (arg==null){
          Debug("key set %s",arg_map.keySet());
          Fail("argument %s is missing", decl.name());
        }
        args.add(arg);
      }
      if (c.yields!=null) for(DeclarationStatement decl:c.yields){
        ASTNode arg=arg_map.get(decl.name());
        if (arg==null){
          count++;
          String name="dummy_yields_"+count;
          currentBlock.prepend(create.field_decl(name,decl.getType()));
          arg=create.local_name(name);
        }
        args.add(arg);      
      }
      //TODO: check for unused arguments.
    }
    MethodInvokation res=create.invokation(rewrite(m.object()), m.dispatch(), m.method(), args.toArray(new ASTNode[0]));
    if (m.get_before()!=null) res.set_before(before);
    if (m.get_after()!=null) res.set_after(after);
    result=res;
  }
}
