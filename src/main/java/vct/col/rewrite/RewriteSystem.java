package vct.col.rewrite;

import hre.ast.Origin;
import hre.lang.HREError;
import hre.lang.Ref;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import vct.col.ast.expr.*;
import vct.col.ast.expr.constant.ConstantExpression;
import vct.col.ast.expr.constant.StructValue;
import vct.col.util.ASTMapping1;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.*;
import vct.col.ast.stmt.decl.*;
import vct.col.ast.stmt.terminal.AssignmentStatement;
import vct.col.ast.stmt.terminal.ReturnStatement;
import vct.col.ast.type.*;
import vct.col.util.ASTUtils;
import vct.util.Configuration;

import static hre.lang.System.*;

class RewriteRule {
  public final String name;
  public final Set<String> vars;
  public final ASTNode lhs;
  public final ASTNode rhs;
  public RewriteRule(String name,Set<String> vars,ASTNode lhs,ASTNode rhs){
    this.name=name;
    this.vars=vars;
    this.lhs=lhs;
    this.rhs=rhs;
  }
}

class MatchLinear implements ASTMapping1<Boolean,ASTNode> {
  
  public Hashtable<String,Ref<ASTNode>> match=new Hashtable<String, Ref<ASTNode>>();

  public MatchLinear(Set<String> vars){
    for(String name:vars){
      match.put(name,new Ref<ASTNode>());
    }
  }

  @Override
  public void pre_map(ASTNode n, ASTNode a) {
    
  }

  @Override
  public Boolean post_map(ASTNode n, Boolean res, ASTNode a) {
    if (res==null) throw new HREError("cannot match %s",n.getClass());
    return res;
  }

  @Override
  public Boolean map(StandardProcedure p, ASTNode a) {
    
    return null;
  }

  @Override
  public Boolean map(ConstantExpression e, ASTNode a) {
    return a.isConstant(e);
  }

  @Override
  public Boolean map(OperatorExpression e1, ASTNode a) {
    if (e1.isa(StandardOperator.IndependentOf)){
      ASTNode tmp = match.get(((NameExpression)e1.arg(1)).getName()).get();
      if (tmp instanceof DeclarationStatement){
        DeclarationStatement decl=(DeclarationStatement)tmp;
        return !ASTUtils.find_name(a, decl.name()) && e1.arg(0).apply(this,a);
      } else {
        return false;
      }
    }
    if (a.isa(e1.operator())){
      OperatorExpression e2=(OperatorExpression)a;
      ASTNode[] args1 = e1.argsJava().toArray(new ASTNode[0]);
      ASTNode[] args2 = e2.argsJava().toArray(new ASTNode[0]);
      if (args1.length!=args2.length) return false;
      for(int i=0;i<args1.length;i++){
        if (!args1[i].apply(this,args2[i])){
          return false;
        }
      }
      return true;
    } else {
      return false;
    }
  }

  @Override
  public Boolean map(NameExpression e, ASTNode a) {
    if(a == null || a.getType() == null || !e.getType().supertypeof(null, a.getType())) {
      return false;
    }

    String name=e.getName();
    Ref<ASTNode> ref=match.get(name);
    if(ref==null){
      return a.isName(name);
    } else if (ref.get()==null) {
      ref.set(a);
      return true;
    } else {
      if (ref.get() instanceof DeclarationStatement){
        DeclarationStatement dref=(DeclarationStatement)ref.get();
        return a.isName(dref.name());
      }
      return ref.get().match(a);
      //throw new HREError("non-linear left-hand side");
    }
  }

  @Override
  public Boolean map(ClassType t, ASTNode a) {
    
    return null;
  }

  @Override
  public Boolean map(FunctionType t, ASTNode a) {
    
    return null;
  }

  @Override
  public Boolean map(PrimitiveType t, ASTNode a) {
    
    return null;
  }

  @Override
  public Boolean map(RecordType t, ASTNode a) {
    
    return null;
  }

  @Override
  public Boolean map(MethodInvokation e, ASTNode a) {
    if (a instanceof MethodInvokation){
      MethodInvokation ee=(MethodInvokation)a;
      if (!e.method.equals(ee.method)) return false;
      if (!e.object.apply(this,ee.object)) return false;
      int N=e.getArity();
      for(int i=0;i<N;i++){
        if (!e.getArg(i).apply(this,ee.getArg(i))) return false;
      }
      return true;
    } else {
      return false;
    }
  }

  @Override
  public Boolean map(BlockStatement s, ASTNode a) {
    
    return null;
  }

  @Override
  public Boolean map(IfStatement s, ASTNode a) {
    
    return null;
  }

  @Override
  public Boolean map(ReturnStatement s, ASTNode a) {
    
    return null;
  }

  @Override
  public Boolean map(AssignmentStatement s, ASTNode a) {
    
    return null;
  }

  @Override
  public Boolean map(DeclarationStatement s, ASTNode a) {
    
    return null;
  }

  @Override
  public Boolean map(LoopStatement s, ASTNode a) {
    
    return null;
  }

  @Override
  public Boolean map(Method m, ASTNode a) {
    
    return null;
  }

  @Override
  public Boolean map(ASTClass c, ASTNode a) {
    
    return null;
  }

  @Override
  public Boolean map(BindingExpression e, ASTNode a) {
    if (e.getDeclCount()!=1) throw new HREError("binders must declare precisely one variable");
    DeclarationStatement decl=e.getDeclaration(0);
    String name=decl.name();
    Ref<ASTNode> ref=match.get(name);
    if(ref==null){
      throw new HREError("bound variable must be a matched variable");
    } else if (ref.get()==null) {
      if (!(a instanceof BindingExpression)) return false;
      BindingExpression ee=(BindingExpression)a;
      if (ee.getDeclCount()!=1) return false;
      DeclarationStatement ee_decl=ee.getDeclaration(0);
      Debug("attempting %s -> %s for",name, ee_decl.name());
      Debug(" %s%n match with%n  %s",
          Configuration.getDiagSyntax().print(e),
          Configuration.getDiagSyntax().print(a));
      ref.set(ee_decl);
      if (ee.binder!=e.binder) return false;
      return e.select.apply(this,ee.select) && e.main.apply(this,ee.main);
    } else {
      throw new HREError("non-linear left-hand side");
    }

  }

  @Override
  public Boolean map(Dereference e1, ASTNode a) {
    if (!(a instanceof Dereference)) return false;
    Dereference e2=(Dereference)a;
    return e1.field().equals(e2.field()) && e1.obj().apply(this,e2.obj());
  }

  @Override
  public Boolean map(Lemma lemma, ASTNode a) {
    
    return null;
  }

  @Override
  public Boolean map(ParallelAtomic pa, ASTNode a) {
    
    return null;
  }

  @Override
  public Boolean map(ParallelBarrier parallelBarrier, ASTNode a) {
    
    return null;
  }

  @Override
  public Boolean map(ParallelBlock parallelBlock, ASTNode a) {
    
    return null;
  }

  @Override
  public Boolean map(ParallelRegion region, ASTNode a) {
    
    return null;
  }

  @Override
  public Boolean map(Contract contract, ASTNode a) {
    
    return null;
  }

  @Override
  public Boolean map(ASTSpecial special, ASTNode a) {
    
    return null;
  }

  @Override
  public Boolean map(VariableDeclaration variableDeclaration, ASTNode a) {
    
    return null;
  }

  @Override
  public Boolean map(TupleType tupleType, ASTNode a) {
    
    return null;
  }

  @Override
  public Boolean map(AxiomaticDataType adt, ASTNode a) {
    
    return null;
  }

  @Override
  public Boolean map(Axiom axiom, ASTNode a) {
    
    return null;
  }

  @Override
  public Boolean map(Hole hole, ASTNode a) {
    
    return null;
  }

  @Override
  public Boolean map(ActionBlock actionBlock, ASTNode a) {
    
    return null;
  }

  @Override
  public Boolean map(TypeExpression t, ASTNode a) {
    
    return null;
  }

  @Override
  public Boolean map(ForEachLoop s, ASTNode a) {
    
    return null;
  }

  @Override
  public Boolean map(NameSpace ns, ASTNode a) {
    
    return null;
  }

  @Override
  public Boolean map(TryCatchBlock tcb, ASTNode a) {
    
    return null;
  }

  @Override
  public Boolean map(FieldAccess s, ASTNode a) {
    
    return null;
  }

  @Override
  public Boolean map(ParallelInvariant inv, ASTNode a) {
    
    return null;
  }

  @Override
  public Boolean map(TypeVariable v, ASTNode a) {
    
    return null;
  }

  @Override
  public Boolean map(StructValue v, ASTNode a) {
    
    return null;
  }

  @Override
  public Boolean map(VectorBlock vb, ASTNode a) {
    return null;
  }

  @Override
  public Boolean map(Constraining c, ASTNode a) {
    return null;
  }

  @Override
  public Boolean map(Switch s, ASTNode a) {
    return null;
  }

  @Override
  public Boolean map(TryWithResources t, ASTNode a) {
    return null;
  }

}

class MatchSubstitution extends AbstractRewriter {

  private Origin origin;

  
  
  @Override
  public void enter(ASTNode n){
    super.enter(n);
    create.setOrigin(origin);
  }

  @Override
  public void leave(ASTNode n){
    super.leave(n);
  }
  
  public Hashtable<String,Ref<ASTNode>> match;
  
  public MatchSubstitution(Hashtable<String, Ref<ASTNode>> match,Origin o) {
    super(null,null);
    this.match=match;
    this.origin=o;
  }
  
  @Override
  public void visit(NameExpression e){
    if (e.getKind()==NameExpression.Kind.Reserved) {
      super.visit(e);
      return;
    }
    String name=e.getName();
    Ref<ASTNode> ref=match.get(name);
    if(ref==null){
      super.visit(e);
    } else {
      ASTNode n=ref.get();
      if (n instanceof DeclarationStatement){
        DeclarationStatement dref=(DeclarationStatement)n;
        result = create.local_name(dref.name());
      } else {
        if (n==null){
          // variable used in rewrite system, but not in LHS? 
          super.visit(e);
        } else {
          result = n;
        }
      }
    }
  }
  
  AtomicInteger ai=new AtomicInteger();
  
  @Override
  public void visit(BindingExpression e){
    DeclarationStatement decls[]=e.getDeclarations();
    for(int i=0;i<decls.length;i++){
      Ref<ASTNode> ref = match.get(decls[i].name());
      DeclarationStatement dref;
      if(ref==null) {
        int no=ai.getAndIncrement();
        dref=create.field_decl(decls[i].name() + "_rw_" + no, decls[i].getType()); 
        match.put(decls[i].name(), new Ref<ASTNode>(dref));
      } else {
        dref=(DeclarationStatement)ref.get();
      }
      decls[i]=create.field_decl(dref.name(), rewrite(dref.getType()), rewrite(decls[i].initJava()));
    }
    if (e.binder== Binder.Let){
      HashMap<NameExpression, ASTNode> map=new HashMap<NameExpression, ASTNode>();
      for(int i=0;i<decls.length;i++){
        map.put(create.local_name(decls[i].name()), decls[i].initJava());
      }
      Substitution sigma=new Substitution(source(),map);
      ASTNode tmp=rewrite(e.main);
      ASTNode res=sigma.rewrite(tmp);
      result=res;
    } else {
      result=create.binder(e.binder,rewrite(e.result_type),decls,rewrite(e.triggers),rewrite(e.select),rewrite(e.main));
    }
  }

  @Override
  public void post_visit(ASTNode node) {
    if(result.getType() == null) {
      result.setType(node.getType());
    }

    super.post_visit(node);
  }
   
}

class Normalizer extends AbstractRewriter {

  private RewriteSystem trs;
  
  public Normalizer(ProgramUnit source,RewriteSystem trs) {
    super(source);
    this.trs=trs;
  }
  
  @Override
  public void post_visit(ASTNode node){
    result.setType(node.getType());
    Ref<ASTNode> ref=new Ref<ASTNode>(result);
    boolean again=(node instanceof ExpressionNode) && trs.step(ref);
    super.post_visit(node);
    if(again){
      result=rewrite(ref.get());
    } else {
      result=ref.get();
    }
  }

}

public class RewriteSystem {

  public ProgramUnit normalize(ProgramUnit pu){
    Normalizer n=new Normalizer(pu,this);
    ProgramUnit res=n.rewriteAll();
    for(Method m:methods){
      res.add(n.copy_rw.rewrite(m));
    }
    return res;
  }

  private ArrayList<RewriteRule> rules=new ArrayList<RewriteRule>();
  
  private ArrayList<Method> methods=new ArrayList<Method>();
  
  private AbstractRewriter normalize;
  
  public boolean step(Ref<ASTNode> term){
    for(RewriteRule rule:rules){
      MatchLinear matcher=new MatchLinear(rule.vars);
      if (rule.lhs.apply(matcher,term.get())){
        MatchSubstitution sigma=new MatchSubstitution(matcher.match,term.get().getOrigin());
        Debug("++ match axiom %s",rule.name);
        term.set(sigma.rewrite(rule.rhs));
        return true;
      }
      Debug("-- no match axiom %s",rule.name);
    }
    return false;
  }
  
  public RewriteSystem(ProgramUnit pu,String sys){
    normalize=new AbstractRewriter(pu){
      @Override
      public void post_visit(ASTNode node){
        Ref<ASTNode> ref=new Ref<ASTNode>(result);
        boolean again=step(ref);
        super.post_visit(node);
        if(again){
          result=rewrite(ref.get());
        }
      }
    };
    HashSet<String> vars=new HashSet<String>();
    for(ASTNode d:pu.find(sys)){
      if(d instanceof DeclarationStatement){
        DeclarationStatement decl=(DeclarationStatement)d;
        String name = decl.name();
        //Warning("variable %s",name);
        vars.add(name);
      }
    }
    for(ASTNode d:pu.find(sys)){
      if(d instanceof DeclarationStatement){
        continue;
      }
      if (d instanceof Axiom){
        Axiom axiom=(Axiom)d;
        //Warning("axiom %s",axiom.name);
        if (!axiom.rule().isa(StandardOperator.EQ)){
          Fail("not a == rule");
        }
        ASTNode lhs=((OperatorExpression)axiom.rule()).arg(0);
        ASTNode rhs=((OperatorExpression)axiom.rule()).arg(1);
        rules.add(new RewriteRule(axiom.name(), vars, lhs, rhs));
        continue;
      }
      if (d instanceof Method && ((Method)d).kind==Method.Kind.Pure){
        methods.add((Method)d);
        continue;
      }
      if (d instanceof Method && ((Method)d).kind==Method.Kind.Constructor){
        continue;
      }
      if (d instanceof ASTSpecial &&
         ((ASTSpecial)d).kind==ASTSpecial.Kind.Comment) {
        continue;
      }
      d.getOrigin().report("fatal","unexpected item in rewrite system: %s",d);
      Fail("Fatal");
    }

  }

  public ASTNode normalize(ASTNode tmp) {
    return normalize.rewrite(tmp);
  }
}
