package vct.col.rewrite;

import vct.col.ast.stmt.decl.ASTClass;
import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.decl.Contract;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.util.AbstractRewriter;
import vct.col.ast.util.ContractBuilder;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.expr.OperatorExpression;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.stmt.decl.ASTSpecial.Kind;
import vct.col.util.FeatureScanner;

/**
 * This class encodes the built-in operations of PVL.
 * 
 * This encoding must be run before and pass that renames methods
 * and/or adds argument is run. E.g. JavaEncoder.
 * 
 * @author Stefan Blom
 *
 */
public class PVLEncoder extends AbstractRewriter {

  public static final String INV="lock_invariant";
  public static final String HELD="lock_held";

  public PVLEncoder(ProgramUnit source) {
    super(source);
  }
  
  public void visit(ASTSpecial s){
    switch(s.kind){
    case Fork:
      result=create.invokation(rewrite(s.args[0]),null,"forkOperator");
      break;
    case Join:
      result=create.invokation(rewrite(s.args[0]),null,"joinOperator");
      break;      
    case Lock:
      currentBlock.add(create.special(ASTSpecial.Kind.Inhale,create.invokation(rewrite(s.args[0]),null,INV)));
      currentBlock.add(create.special(Kind.Unfold,create.invokation(rewrite(s.args[0]),null,INV)));
      currentBlock.add(create.special(ASTSpecial.Kind.Inhale,create.invokation(rewrite(s.args[0]),null,HELD)));
      result=null;
      break;
    case Unlock:
      currentBlock.add(create.special(ASTSpecial.Kind.Exhale,create.invokation(rewrite(s.args[0]),null,HELD)));
      currentBlock.add(create.special(Kind.Fold,create.invokation(rewrite(s.args[0]),null,INV)));
      currentBlock.add(create.special(ASTSpecial.Kind.Exhale,create.invokation(rewrite(s.args[0]),null,INV)));
      result=null;
      break;
    case Wait:
      currentBlock.add(create.special(Kind.Fold,create.invokation(rewrite(s.args[0]),null,INV)));
      currentBlock.add(create.special(ASTSpecial.Kind.Exhale,create.invokation(rewrite(s.args[0]),null,INV)));
      currentBlock.add(create.special(ASTSpecial.Kind.Assert,create.invokation(rewrite(s.args[0]),null,HELD)));
      currentBlock.add(create.special(ASTSpecial.Kind.Inhale,create.invokation(rewrite(s.args[0]),null,INV)));
      currentBlock.add(create.special(Kind.Unfold,create.invokation(rewrite(s.args[0]),null,INV)));
      result=null;
      break;
    case Notify:
      currentBlock.add(create.special(ASTSpecial.Kind.Assert,create.invokation(rewrite(s.args[0]),null,HELD)));
      result=null;
      break;
    default:
      super.visit(s);
    }
  }

  @Override
  public void visit(OperatorExpression e){
    switch (e.operator()) {
    case PVLidleToken:
      result=create.invokation(rewrite(e.arg(0)),null,"idleToken");
      break;
    case PVLjoinToken:
      result=create.invokation(rewrite(e.arg(0)),null,"joinToken");
      break;
    case Held:
      result = create.invokation(rewrite(e.arg(0)), null, HELD);
      break;
    default:
      super.visit(e);
    }
  }
  
  
  @Override
  public void visit(Method m){
    if(m.name().equals(INV)){
      currentTargetClass.add_dynamic(create.predicate(HELD,null));
    }
    if (m.name().equals("run")) {
      Contract c=m.getContract();
      FeatureScanner features=new FeatureScanner();
      c.post_condition.accept(features);
      if(features.usesOperator(StandardOperator.Old)){
        Fail("The post-condition of a run method is not allowed to use the \\old operator.");
      }
      ContractBuilder cb=new ContractBuilder();
      cb.requires(rewrite(c.pre_condition));
      cb.requires(create.invokation(create.diz(),null,"idleToken"));
      cb.ensures(create.invokation(create.diz(),null,"joinToken"));
      Method fork=create.method_decl(
          create.primitive_type(PrimitiveSort.Void),
          cb.getContract(),
          "forkOperator",
          new DeclarationStatement[0],
          null
      );
      currentTargetClass.add_dynamic(fork);
      cb=new ContractBuilder();
      cb.requires(create.invokation(create.diz(),null,"joinToken"));
      cb.ensures(create.invokation(create.diz(),null,"idleToken"));
      cb.ensures(rewrite(c.post_condition));
      Method join=create.method_decl(
          create.primitive_type(PrimitiveSort.Void),
          cb.getContract(),
          "joinOperator",
          new DeclarationStatement[0],
          null
      );
      currentTargetClass.add_dynamic(join);
      Method token=create.predicate("joinToken",null);
      currentTargetClass.add_dynamic(token);
      token=create.predicate("idleToken",null);
      currentTargetClass.add_dynamic(token);
    }
    if(m.kind==Method.Kind.Constructor){
      ASTClass parent=(ASTClass)m.getParent();
      boolean runnable=false;
      for(Method method:parent.dynamicMethods()){
        if (method.name().equals("run")){
          runnable=true; 
          break;
        }
      }
      if (runnable){
        currentContractBuilder=new ContractBuilder();
        currentContractBuilder.ensures(create.invokation(create.diz(),null,"idleToken"));
        super.visit(m);
        Method method=(Method)result;
        BlockStatement block=(BlockStatement)method.getBody();
        block.append(
            create.special(ASTSpecial.Kind.Inhale,
            create.invokation(create.diz(),null,"idleToken")));
        return;
      }
    }
    super.visit(m);
  }

}
