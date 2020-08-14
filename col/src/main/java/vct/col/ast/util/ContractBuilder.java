// -*- tab-width:2 ; indent-tabs-mode:nil -*-
package vct.col.ast.util;

import hre.ast.CompositeOrigin;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.expr.OperatorExpression;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.stmt.decl.SignalsClause;
import vct.col.ast.stmt.decl.VariableDeclaration;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.decl.Contract;
import vct.col.ast.type.Type;

import java.util.*;

import static vct.col.ast.stmt.decl.Contract.default_true;

public class ContractBuilder {

  private boolean empty=true;
  
  private ASTNode pre_condition = default_true;
  private ASTNode post_condition = default_true;
  private ASTNode invariant = default_true;
  private ArrayList<DeclarationStatement> given = new ArrayList<>();
  private ArrayList<DeclarationStatement> yields = new ArrayList<>();
  private HashSet<ASTNode> modifiable;
  private HashSet<ASTNode> accessible;
  private ArrayList<SignalsClause> signals = new ArrayList<>();
  
  public boolean isEmpty() {
    return invariant.isConstant(default_true)
        && pre_condition.isConstant(default_true)
        && post_condition.isConstant(default_true)
        && given.size()==0 && yields.size()==0
        && signals.size()==0
        && modifiable == null
        ;
  }

  private static final void scan_to(ArrayList<DeclarationStatement> list, BlockStatement decls){
    int N=decls.getLength();
    for(int i=0;i<N;i++){
      ASTNode d=decls.getStatement(i);
      if (d instanceof DeclarationStatement){
        DeclarationStatement decl=(DeclarationStatement)d;
        if (decl.init() != null) throw new Error("illegal initialization");
        list.add(decl);
      } else {
        throw new Error("not a declaration: "+d.getClass());
      }
    }
  }
  
  /**
   * Add the given declarations to the list of given variables.
   * @param decls A block consisting of declaration statement only.
   */
  public void given(BlockStatement decls){
    empty=false;
    scan_to(given,decls);
  }
  /**
   * Add the given declarations to the list of yielded variables.
   * @param decls
   */
  public void yields(BlockStatement decls){
    empty=false;
    scan_to(yields,decls);    
  }
  /**
   * Add the given declarations to the list of given variables.
   * @param decls Any number of declarations.
   */
  public void given(DeclarationStatement ... decls){
    empty=false;
    for(DeclarationStatement d:decls) given.add(d);
  }
  
  public void given(VariableDeclaration decl){
	empty=false;
    for(DeclarationStatement d:decl.flatten()){
      given.add(d);
    }
  }

  public void yields(VariableDeclaration decl){
    empty=false;
    for(DeclarationStatement d:decl.flatten()){
      yields.add(d);
    }
  }
  /**
   * Add the given declarations to the list of yielded variables.
   * @param decls Any number of declarations.
   */
  public void yields(DeclarationStatement ... decls){
    empty=false;
    for(DeclarationStatement d:decls) yields.add(d);
  }
  public void ensures(ASTNode condition){
    ensures(condition,true);
  }
  public void ensures(ASTNode condition,boolean at_end){
    empty=false;
    if (post_condition==default_true) {
      post_condition=condition;
    } else {
      ASTNode tmp=post_condition;
      if (at_end){
        post_condition = new OperatorExpression(StandardOperator.Star, new ASTNode[] { post_condition, condition });
      } else {
        post_condition = new OperatorExpression(StandardOperator.Star, new ASTNode[] { condition, post_condition });
      }
      post_condition.setOrigin(new CompositeOrigin(tmp.getOrigin(),condition.getOrigin()));
    }
  }
  public void requires(ASTNode condition){
    requires(condition,true);
  }
  public void requires(ASTNode condition,boolean at_end){
    empty=false;
    if (pre_condition==default_true) {
      pre_condition=condition;
    } else {
      ASTNode tmp=pre_condition;
      if (at_end){
        pre_condition = new OperatorExpression(StandardOperator.Star, new ASTNode[] { pre_condition, condition });
      } else {
        pre_condition = new OperatorExpression(StandardOperator.Star, new ASTNode[] { condition, pre_condition });
      }
      pre_condition.setOrigin(new CompositeOrigin(tmp.getOrigin(),condition.getOrigin()));
    }
  }

  public void appendInvariant(ASTNode condition){
    empty=false;
    if (invariant==default_true) {
      invariant=condition;
    } else {
      ASTNode tmp=invariant;
      invariant = new OperatorExpression(StandardOperator.Star, new ASTNode[] { invariant, condition });
      invariant.setOrigin(new CompositeOrigin(tmp.getOrigin(),condition.getOrigin()));
    }
  }
  
  public void prependInvariant(ASTNode condition){
    empty=false;
    if (invariant==default_true) {
      invariant=condition;
    } else {
      ASTNode tmp=invariant;
      invariant = new OperatorExpression(StandardOperator.Star, new ASTNode[] { condition, invariant });
      invariant.setOrigin(new CompositeOrigin(tmp.getOrigin(),condition.getOrigin()));
    }
  }
 
  public Contract getContract(){
    return getContract(true);
  }
  
  public Contract getContract(boolean null_on_empty){
    if (empty && null_on_empty) return null;
    DeclarationStatement[] decls=new DeclarationStatement[0];
    ASTNode[] mods=null;
    if (modifiable!=null){
      mods=modifiable.toArray(new ASTNode[0]);
    }
    ASTNode[] accs=null;
    if (accessible!=null){
      accs=accessible.toArray(new ASTNode[0]);
    }
    return new Contract(
            given.toArray(decls),
            yields.toArray(decls),
            mods,
            accs,
            invariant,
            pre_condition,
            post_condition,
            signals.toArray(new SignalsClause[0])
            );
  }
  public void modifies(ASTNode ... locs) {
    empty=false;
    if (modifiable==null) modifiable=new HashSet<ASTNode>();
    for (ASTNode loc : locs){
      modifiable.add(loc);
    }
  }
  public void accesses(ASTNode ... locs) {
    empty=false;
    if (accessible==null) accessible=new HashSet<ASTNode>();
    for (ASTNode loc : locs){
      accessible.add(loc);
    }
  }

  public static Contract emptyContract() {
    return new Contract(new DeclarationStatement[0],new DeclarationStatement[0],default_true,default_true,default_true);
  }

  public void signals(String name, Type type, ASTNode condition) {
      signals(new SignalsClause(name, type, condition));
  }

  public void signals(SignalsClause signalsClause) {
    empty = false;
    signals.add(signalsClause);
  }

  public void requires(Iterable<ASTNode> collection) {
    for(ASTNode item:collection){
      requires(item); 
    }
  }
  public void ensures(Iterable<ASTNode> collection) {
    for(ASTNode item:collection){
      ensures(item); 
    }
  }

  public void context(Iterable<ASTNode> e) {
    requires(e);
    ensures(e);
  }

  public void context(ASTNode e) {
    requires(e);
    ensures(e);
  }

}

