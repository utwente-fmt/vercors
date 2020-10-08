package viper.api;

import java.util.ArrayList;
import java.util.Map;
import java.util.Map.Entry;

import hre.ast.MessageOrigin;
import hre.ast.Origin;
import hre.lang.HREError;
import hre.util.Triple;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.type.ASTReserved;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.type.Type;
import vct.col.ast.util.ASTFactory;

public class VerCorsExpressionFactory implements
    ExpressionFactory<Origin, Type, ASTNode> {

  public VerCorsExpressionFactory(ASTFactory<?> create){
    this.create=create;
  }
  
  private ASTFactory<?> create;
  
  @Override
  public ASTNode add(Origin o, ASTNode e1, ASTNode e2) {
    enter(o);
    ASTNode res=create.expression(StandardOperator.Plus, e1,e2);
    leave();
    return res;
  }

  @Override
  public ASTNode perm_add(Origin o, ASTNode e1, ASTNode e2) {
    return add(o, e1, e2);
  }

  @Override
  public ASTNode and(Origin o, ASTNode e1, ASTNode e2) {
    enter(o);
    ASTNode res;
    if (e1.getType()!=null || e2.getType()!=null){
      res=create.expression(StandardOperator.Star, e1,e2);
    } else {
      res=create.expression(StandardOperator.And, e1,e2);
    }
    leave();
    return res;
  }

  @Override
  public ASTNode any_set_contains(Origin o, ASTNode e1, ASTNode e2) {
    enter(o);
    ASTNode res=create.expression(StandardOperator.Member, e1,e2);
    leave();
    return res;
  }

  @Override
  public ASTNode any_set_minus(Origin o, ASTNode e1, ASTNode e2) {
    enter(o);
    ASTNode res=create.expression(StandardOperator.Minus, e1, e2);
    leave();
    return res;
  }

  @Override
  public ASTNode any_set_intersection(Origin o, ASTNode e1, ASTNode e2) {
    enter(o);
    ASTNode res=create.expression(StandardOperator.Mult, e1, e2);
    leave();
    return res;
  }

  @Override
  public ASTNode any_set_subset(Origin o, ASTNode e1, ASTNode e2) {
      enter(o);
      //TODO check what to put here.
      // Basically, the LTE operator is used in PVL, but this does not have to hold for any other language.
      ASTNode res= create.expression(StandardOperator.SubSet, e1, e2);
      leave();
    return res;
  }

  @Override
  public ASTNode any_set_union(Origin o, ASTNode e1, ASTNode e2) {
    enter(o);
    ASTNode res=create.expression(StandardOperator.Plus, e1, e2);
    leave();
    return res;
  }

  @Override
  public ASTNode append(Origin o, ASTNode e1, ASTNode e2) {
    enter(o);
    ASTNode res=create.expression(StandardOperator.Concat,e1,e2);
    leave();
    return res;
  }

  @Override
  public ASTNode cond(Origin o, ASTNode e1, ASTNode e2, ASTNode e3) {
    enter(o);
    ASTNode res=create.expression(StandardOperator.ITE, e1,e2,e3);
    leave();
    return res;
 }

  @Override
  public ASTNode let(Origin o,String name,Type t, ASTNode e1, ASTNode e2){
    enter(o);
    DeclarationStatement decl=create.field_decl(name,t, e1);
    ASTNode res=create.let_expr(decl, e2);
    leave();
    return res;

  }
  
  @Override
  public ASTNode Constant(Origin o, boolean b) {
    enter(o);
    ASTNode res=create.constant(b);
    leave();
    return res;
  }

  @Override
  public ASTNode Constant(Origin o, int i) {
    enter(o);
    ASTNode res=create.constant(i);
    leave();
    return res;
  }


  @Override
  public ASTNode current_perm(Origin o, ASTNode expr){
    enter(o);
    ASTNode res=create.expression(StandardOperator.CurrentPerm,expr);
    res.setType(create.primitive_type(PrimitiveSort.Resource));
    leave();
    return res;
  }
  
  @Override
  public ASTNode floor_div(Origin o, ASTNode e1, ASTNode e2) {
    enter(o);
    ASTNode res=create.expression(StandardOperator.FloorDiv, e1,e2);
    leave();
    return res;
  }

  @Override
  public ASTNode div(Origin o, ASTNode e1, ASTNode e2) {
    enter(o);
    ASTNode res=create.expression(StandardOperator.Div, e1,e2);
    leave();
    return res;
  }

  private <E extends ASTNode> ASTNode[] toArray(Map<String, E> map){
    ArrayList<ASTNode> list=new ArrayList<ASTNode>();
    for(Entry<String, E> e:map.entrySet()){
      ASTNode n=e.getValue();
      n.addLabel(create.label(e.getKey()));
      list.add(n);
    }
    return list.toArray(new ASTNode[map.size()]);
  }
  
  @Override
  public ASTNode domain_call(Origin o, String name,
      java.util.List<ASTNode> args, Map<String, Type> dpars, Type rt, String domain) {
    enter(o);
    MethodInvokation res=create.invokation(
        create.class_type(domain,toArray(dpars)),
        null,
        name,
        args);
    leave();
    return res;
  }

  @Override
  public ASTNode drop(Origin o, ASTNode e1, ASTNode e2) {
    enter(o);
    ASTNode res=create.expression(StandardOperator.Drop, e1,e2);
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
  public ASTNode eq(Origin o, ASTNode e1, ASTNode e2) {
    enter(o);
    ASTNode res=create.expression(StandardOperator.EQ, e1,e2);
    leave();
    return res;
  }

  @Override
  public ASTNode exists(Origin o, java.util.List<Triple<Origin,String,Type>> vars,
      ASTNode e) {
    enter(o);
    ASTNode res=create.exists(e, vars);
    leave();
    return res;
  }

  @Override
  public ASTNode explicit_bag(Origin o, Type t,  java.util.List<ASTNode> elems) {
    enter(o);
    t=create.primitive_type(PrimitiveSort.Bag,t);
    ASTNode res=create.struct_value(t , null , elems );
    res.setType(t);
    leave();
    return res;
  }

  @Override
  public ASTNode explicit_seq(Origin o, Type t, java.util.List<ASTNode>  elems) {
    enter(o);
    t=create.primitive_type(PrimitiveSort.Sequence,t);
    ASTNode res=create.struct_value(t , null , elems );
    res.setType(t);
    leave();
    return res;
  }

  @Override
  public ASTNode explicit_set(Origin o, Type t, java.util.List<ASTNode> elems) {
    enter(o);
    t=create.primitive_type(PrimitiveSort.Set, t);
    ASTNode res=create.struct_value(t , null , elems );
    res.setType(t);
    leave();
    return res;
  }

  @Override
  public ASTNode field_access(Origin o, ASTNode e1, ASTNode e2) {
    enter(o);
    ASTNode res=create.expression(StandardOperator.Perm,e1,e2);
    res.setType(create.primitive_type(PrimitiveSort.Resource));
    leave();
    return res;
  }

  @Override
  public ASTNode FieldAccess(Origin o, ASTNode obj, String field, Type t) {
    enter(o);
    ASTNode res=create.dereference(obj, field);
    leave();
    return res;
  }

  private ASTNode[][] toAofA(java.util.List<java.util.List<ASTNode>> lofl){
    ASTNode[][] aofa=new ASTNode[lofl.size()][];
    for(int i=0;i<aofa.length;i++){
      java.util.List<ASTNode> l=lofl.get(i);
      aofa[i]=l.toArray(new ASTNode[l.size()]);
    }
    return aofa;
  }
  
  @Override
  public ASTNode forall(Origin o, java.util.List<Triple<Origin,String,Type>> vars,
      java.util.List<java.util.List<ASTNode>> triggers, ASTNode e) {
    enter(o);
    ASTNode res=create.forall(toAofA(triggers), create.constant(true), e,
        create.to_decls(vars));
    leave();
    return res;
  }

  @Override
  public ASTNode frac(Origin o, ASTNode e1, ASTNode e2) {
    enter(o);
    ASTNode res=create.expression(StandardOperator.Div, e1,e2);
    leave();
    return res;
  }

  @Override
  public ASTNode function_call(Origin o, String name,
      java.util.List<ASTNode> args, Type rt) {
    enter(o);
    ASTNode res=create.invokation(null,null,name, args);
    leave();
    return res;
  }

  @Override
  public ASTNode gt(Origin o, ASTNode e1, ASTNode e2) {
    enter(o);
    ASTNode res=create.expression(StandardOperator.GT, e1,e2);
    leave();
    return res;
  }

  @Override
  public ASTNode gte(Origin o, ASTNode e1, ASTNode e2) {
    enter(o);
    ASTNode res=create.expression(StandardOperator.GTE, e1,e2);
    leave();
    return res;
  }

  @Override
  public ASTNode implies(Origin o, ASTNode e1, ASTNode e2) {
    enter(o);
    ASTNode res=create.expression(StandardOperator.Implies, e1,e2);
    if (e1.getType()!=null || e2.getType()!=null){
      res.setType(create.primitive_type(PrimitiveSort.Resource));
    }
    leave();
    return res;
  }

  @Override
  public ASTNode index(Origin o, ASTNode e1, ASTNode e2) {
    enter(o);
    ASTNode res=create.expression(StandardOperator.Subscript, e1,e2);
    leave();
    return res;
  }

  private void leave(){
    create.leave();
  }

  @Override
  public ASTNode local_name(Origin o, String name, Type t) {
    enter(o);
    ASTNode res=create.local_name(name);
    res.setType(t);
    leave();
    return res;
  }

  @Override
  public ASTNode lt(Origin o, ASTNode e1, ASTNode e2) {
    enter(o);
    ASTNode res=create.expression(StandardOperator.LT, e1,e2);
    leave();
    return res;
  }

  @Override
  public ASTNode lte(Origin o, ASTNode e1, ASTNode e2) {
    enter(o);
    ASTNode res=create.expression(StandardOperator.LTE, e1,e2);
    leave();
    return res;
  }

  @Override
  public ASTNode mod(Origin o, ASTNode e1, ASTNode e2) {
    enter(o);
    ASTNode res=create.expression(StandardOperator.Mod, e1,e2);
    leave();
    return res;
  }

  @Override
  public ASTNode mult(Origin o, ASTNode e1, ASTNode e2) {
    enter(o);
    ASTNode res=create.expression(StandardOperator.Mult, e1,e2);
    leave();
    return res;
  }

  @Override
  public ASTNode neg(Origin o, ASTNode e1) {
    enter(o);
    ASTNode res=create.expression(StandardOperator.UMinus, e1);
    leave();
    return res;
  }

  @Override
  public ASTNode neq(Origin o, ASTNode e1, ASTNode e2) {
    enter(o);
    ASTNode res=create.expression(StandardOperator.NEQ, e1,e2);
    leave();
    return res;
  }

  @Override
  public ASTNode no_perm(Origin o) {
    enter(o);
    ASTNode res=create.reserved_name(ASTReserved.NoPerm);
    leave();
    return res;
  }

  @Override
  public ASTNode not(Origin o, ASTNode e1) {
    enter(o);
    ASTNode res=create.expression(StandardOperator.Not, e1);
    leave();
    return res;
  }

  @Override
  public ASTNode null_(Origin o) {
    enter(o);
    ASTNode res=create.reserved_name(ASTReserved.Null);
    leave();
    return res;
  }

  @Override
  public ASTNode old(Origin o, ASTNode e1) {
    enter(o);
    ASTNode res=create.expression(StandardOperator.Mult, e1);
    leave();
    return res;
  }

  @Override
  public ASTNode or(Origin o, ASTNode e1, ASTNode e2) {
    enter(o);
    ASTNode res=create.expression(StandardOperator.Or, e1,e2);
    leave();
    return res;
  }

  @Override
  public ASTNode predicate_call(Origin o, String name,
      java.util.List<ASTNode> args) {
    enter(o);
    ASTNode res=create.invokation(null, null, name, args);
    leave();
    return res;
  }

  @Override
  public ASTNode range(Origin o, ASTNode e1, ASTNode e2) {
    throw new HREError("missing case");
  }

  @Override
  public ASTNode read_perm(Origin o) {
    enter(o);
    ASTNode res=create.reserved_name(ASTReserved.ReadPerm);
    leave();
    return res;
  }
  
  @Override
  public ASTNode result(Origin o, Type t) {
    enter(o);
    ASTNode res=create.reserved_name(ASTReserved.Result, t);
    leave();
    return res;
 
  }

  @Override
  public ASTNode scale_access(Origin o, ASTNode e1, ASTNode e2) {
    enter(o);
    ASTNode res=create.expression(StandardOperator.Scale, e2,e1);
    res.setType(create.primitive_type(PrimitiveSort.Resource));
    leave();
    return res;
 }
  
  @Override
  public ASTNode seq_contains(Origin o, ASTNode e1, ASTNode e2) {
    enter(o);
    ASTNode res=create.expression(StandardOperator.Member, e2,e1);
    leave();
    return res;
  }

  @Override
  public ASTNode size(Origin o, ASTNode e1) {
    enter(o);
    ASTNode res=create.expression(StandardOperator.Size, e1);
    leave();
    return res;
  }

  @Override
  public ASTNode sub(Origin o, ASTNode e1, ASTNode e2) {
    enter(o);
    ASTNode res=create.expression(StandardOperator.Minus, e1,e2);
    leave();
    return res;
  }

  @Override
  public ASTNode take(Origin o, ASTNode e1, ASTNode e2) {
    enter(o);
    ASTNode res=create.expression(StandardOperator.Take, e1, e2);
    leave();
    return res;
  }

  @Override
  public ASTNode slice(Origin o, ASTNode e1, ASTNode e2, ASTNode e3) {
    enter(o);
    ASTNode res = create.expression(StandardOperator.Slice, e1, e2, e3);
    leave();
    return res;
  }

  @Override
  public ASTNode seq_update(Origin o, ASTNode e1, ASTNode e2, ASTNode e3) {
    enter(o);
    ASTNode res = create.expression(StandardOperator.SeqUpdate, e1, e2, e3);
    leave();
    return res;
  }

  @Override
  public ASTNode unfolding_in(Origin o, ASTNode e1, ASTNode e2) {
    enter(o);
    ASTNode res=create.expression(StandardOperator.Unfolding,e1,e2);
    leave();
    return res;
  }

  @Override
  public ASTNode union(Origin o, ASTNode e1, ASTNode e2) {
    throw new HREError("missing case");
  }

  @Override
  public ASTNode write_perm(Origin o) {
    enter(o);
    ASTNode res=create.reserved_name(ASTReserved.FullPerm);
    leave();
    return res;
  }

}
