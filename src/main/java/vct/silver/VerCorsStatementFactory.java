package vct.silver;

import java.util.ArrayList;
import java.util.List;

import hre.ast.MessageOrigin;
import hre.ast.Origin;
import hre.lang.HREError;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.ASTSpecial.Kind;
import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.composite.Constraining;
import vct.col.ast.expr.NameExpression;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.type.Type;
import vct.col.util.ASTFactory;
import viper.api.StatementFactory;

public class VerCorsStatementFactory implements
    StatementFactory<Origin, Type, ASTNode, ASTNode > {

  public VerCorsStatementFactory(ASTFactory<?> create){
    this.create=create;
  }
  
  private ASTFactory<?> create;

  @Override
  public ASTNode assert_(Origin o, ASTNode expr) {
    enter(o);
    ASTNode res=create.special(ASTSpecial.Kind.Assert, expr);
    leave();
    return res;
  }

  @Override
  public ASTNode refute(Origin o, ASTNode expr) {
    enter(o);
    ASTNode res=create.special(ASTSpecial.Kind.Refute, expr);
    leave();
    return res;
  }

  @Override
  public ASTNode assignment(Origin o, ASTNode loc, ASTNode val) {
    enter(o);
    ASTNode res=create.assignment(loc, val);
    leave();
    return res;
  }

  @Override
  public ASTNode block(Origin o, java.util.List<ASTNode> stats) {
    enter(o);
    BlockStatement res=create.block();
    for(ASTNode S : stats){
      res.add(S);
    }
    leave();
    return res;
  }

  @Override
  public ASTNode exhale(Origin o, ASTNode expr) {
    enter(o);
    ASTNode res=create.special(Kind.Exhale,expr);
    leave();
    return res; 
  }
  
  private void enter(Origin o){
    create.enter();
    if (o==null){
      hre.lang.System.Warning("missing origin");
      o=new MessageOrigin("unknown origin");
    }
    create.setOrigin(o);    
  }


  @Override
  public ASTNode fold(Origin o, ASTNode expr) {
    enter(o);
    ASTNode res=create.special(Kind.Fold,expr);
    leave();
    return res;
  }

  @Override
  public ASTNode goto_(Origin o, String l) {
    enter(o);
    ASTNode res=create.special(Kind.Goto,create.label(l));
    leave();
    return res;

  }

  @Override
  public ASTNode if_then_else(Origin o, ASTNode c, ASTNode s1, ASTNode s2) {
    enter(o);
    ASTNode res=create.ifthenelse(c,s1,s2);
    leave();
    return res;
  }

  @Override
  public ASTNode inhale(Origin o, ASTNode expr) {
    enter(o);
    ASTNode res=create.special(Kind.Inhale,expr);
    leave();
    return res;
  }

  @Override
  public ASTNode label(Origin o, String l, java.util.List<ASTNode> invs) {
    enter(o);
    ASTNode res=create.special(Kind.Label,create.label(l));
    leave();
    return res;
  }

  private void leave(){
    create.leave();
  }

  @Override
  public ASTNode method_call(Origin o, String name,
      java.util.List<ASTNode> args, java.util.List<ASTNode> outs,
      java.util.List<viper.api.Triple<Origin,String,Type>> pars,
      java.util.List<viper.api.Triple<Origin,String,Type>> rets) {
    enter(o);
    ArrayList<ASTNode> list=new ArrayList<ASTNode>();
    list.addAll(args);
    list.addAll(outs);
    ASTNode res=create.invokation(null,null,name,list);
    leave();
    return res;
  }

  @Override
  public ASTNode new_object(Origin o, ASTNode var,
      java.util.List<String> names, java.util.List<Type> types) {
    enter(o);
    ArrayList<ASTNode> lst=new ArrayList<ASTNode>();
    for(String name:names){
      lst.add(create.dereference(create.class_type("Ref"),name));
    }
    ASTNode res=create.assignment(var,create.expression(StandardOperator.NewSilver,lst));
    leave();
    return res;
  }

  @Override
  public ASTNode unfold(Origin o, ASTNode expr) {
    enter(o);
    ASTNode res=create.special(ASTSpecial.Kind.Unfold,expr);
    leave();
    return res;
  }

  @Override
  public ASTNode while_loop(Origin o, ASTNode cond,
      java.util.List<ASTNode> invs,
      java.util.List<viper.api.Triple<Origin,String,Type>> locals, ASTNode body) {
    enter(o);
    BlockStatement block=create.block();
    for(viper.api.Triple<Origin,String,Type> e:locals){
      block.add(create.field_decl(e.v1,e.v2,e.v3));
    }
    block.append(body);
    ASTNode loop=create.while_loop(cond, block, invs.toArray(new ASTNode[invs.size()]));
    leave();
    return loop;
  }

  private BlockStatement checkBlock(ASTNode body) {
    if (body instanceof BlockStatement){
      return (BlockStatement)body;
    } else {
      throw new HREError("not a block statement");
    }
  }

}
