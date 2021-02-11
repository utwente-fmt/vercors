package viper.api;

import hre.ast.MessageOrigin;
import hre.ast.Origin;
import hre.lang.HREError;
import hre.tools.TimeKeeper;
import hre.util.Triple;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.decl.*;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.type.Type;
import vct.col.ast.util.ASTFactory;
import vct.col.ast.util.ASTUtils;
import vct.col.ast.util.Configuration;
import vct.col.ast.util.ContractBuilder;

import java.util.*;

public class VerCorsProgramFactory implements
    ProgramFactory<Origin, Type, ASTNode, ASTNode,
    Method, Axiom, ProgramUnit> {
  
  public VerCorsProgramFactory(ASTFactory<?> create){
    this.create=create;
  }
  
  public Hashtable<String,Set<Origin>> refuted;
  public HashSet<Origin> satCheckAsserts = new HashSet<>();

  private ASTFactory<?> create;
   
  @Override
  public void add_adt(ProgramUnit p, Origin o, String name,
      java.util.List<Method> funs, java.util.List<Axiom> axioms,
      java.util.List<String> pars) {
    enter(o);
    DeclarationStatement decls[]=new DeclarationStatement[pars.size()];
    for(int i=0;i<decls.length;i++){
      decls[i]=create.field_decl(pars.get(i),create.primitive_type(PrimitiveSort.Class));
    }
    AxiomaticDataType adt=create.adt(name,decls);
    for(Method m:funs) adt.add_cons(m);
    for(Axiom a:axioms) adt.add_axiom(a);
    p.add(adt);
    leave();
  }

  @Override
  public void add_field(ProgramUnit p, Origin o, String name, Type t) {
    enter(o);
    p.add(create.field_decl(name, t));
    leave();
  }

  @Override
  public void add_function(ProgramUnit p, Origin o, String name,
      List<Triple<Origin,String,Type>> args, Type t,
      java.util.List<ASTNode> pres, java.util.List<ASTNode> posts, ASTNode body) {
    enter(o);
    ContractBuilder cb=new ContractBuilder();
    for(ASTNode c:pres){
      cb.requires(c);
    }
    for(ASTNode c:posts){
      cb.ensures(c);
    }
    Method m=create.function_decl(t,cb.getContract(), name, create.to_decls(args), body);
    m.setStatic(true);
    p.add(m);
    leave();
  }

  @Override
  public void add_method(ProgramUnit p, Origin o, String name,
      java.util.List<ASTNode> pres, java.util.List<ASTNode> posts,
      List<Triple<Origin,String,Type>> in,
      List<Triple<Origin,String,Type>> out,
      List<Triple<Origin,String,Type>> local,
      List<String> labels, // ignored
      ASTNode body
  ) {
    enter(o);
    ContractBuilder cb=new ContractBuilder();
    for(ASTNode c:pres){
      cb.requires(c);
    }
    for(ASTNode c:posts){
      cb.ensures(c);
    }
    ArrayList<DeclarationStatement> args=new ArrayList<DeclarationStatement>();
    for(DeclarationStatement d:create.to_decls(in)){
      d.setFlag(ASTFlags.IN_ARG,true);
      args.add(d);
    }
    for(DeclarationStatement d:create.to_decls(out)){
      d.setFlag(ASTFlags.OUT_ARG,true);
      args.add(d);
    }
    BlockStatement block=create.block();
    for(DeclarationStatement d:create.to_decls(local)){
      block.add(d);
    }
    if(body!=null){
      block.append(body);
    }
    Method m=create.method_decl(
        create.primitive_type(PrimitiveSort.Void),
        cb.getContract(),
        name,
        args.toArray(new DeclarationStatement[0]),
        block);
    m.setStatic(true);
    p.add(m);
    leave();
  }

  @Override
  public void add_predicate(ProgramUnit p, Origin o, String name,
      List<Triple<Origin,String,Type>> args, ASTNode body) {
    enter(o);
    Method m=create.predicate(name, body, create.to_decls(args));
    m.setStatic(true);
    p.add(m);
    leave();   
  }

 
  
  @Override
  public Axiom daxiom(Origin o, String name, ASTNode expr, String domain) {
    enter(o);
    Axiom res=create.axiom(name, expr);
    leave();
    return res;
  }

  @Override
  public Method dfunc(Origin o, String name,
      List<Triple<Origin,String,Type>> args, Type t, String domain, boolean unique) {
    enter(o);
    Method res=create.function_decl(t,null, name,create.to_decls(args),null);
    res.setFlag(ASTFlags.UNIQUE, unique);
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
  
  private void leave(){
    create.leave();
  }

  @Override
  public ProgramUnit parse_program(String file) {
    throw new HREError("missing case");
  }

 
  @Override
  public ProgramUnit program() {
    return new ProgramUnit();
  }

  @Override
  public <T, E, S, DFunc, DAxiom, P> P convert(
      ViperAPI<Origin, T, E, S, DFunc, DAxiom, P> api,
      ProgramUnit arg) {
    
    SilverTypeMap<T> type=new SilverTypeMap<T>(api);
    SilverExpressionMap<T, E> expr=new SilverExpressionMap<T,E>(api,type);
    SilverStatementMap<T, E, S> stat=new SilverStatementMap<T,E,S>(api,type,expr);
    P program=api.prog.program();
    
    TimeKeeper tk = new TimeKeeper();
    for(ASTNode entry:arg) {
      if (entry instanceof Method) {
        Method m = (Method)entry;
        switch(m.kind){
        case Plain:{
          stat.refuted=new HashSet<Origin>();
          ArrayList<Triple<Origin,String,T>> in=new ArrayList<Triple<Origin,String,T>>();
          ArrayList<Triple<Origin,String,T>> out=new ArrayList<Triple<Origin,String,T>>();
          for(DeclarationStatement decl:m.getArgs()){
            T t=decl.getType().apply(type);
            if (decl.isValidFlag(ASTFlags.OUT_ARG) && decl.getFlag(ASTFlags.OUT_ARG)){
              out.add(new Triple<Origin,String,T>(decl.getOrigin(),decl.name(), t));
            } else {
              in.add(new Triple<Origin,String,T>(decl.getOrigin(),decl.name(), t));
            }
          }
          ArrayList<Triple<Origin,String,T>> locals=new ArrayList<Triple<Origin,String,T>>();
          ArrayList<String> labels = new ArrayList<>();
          S body;
          if (m.getBody() instanceof BlockStatement){
            BlockStatement block=(BlockStatement)m.getBody();
            ArrayList<S> stats=new ArrayList<S>();
            VerCorsProgramFactory.split_block(type, stat, block, locals, labels, stats);
            body=api.stat.block(block.getOrigin(),stats);
          } else if (m.getBody()==null){
            Origin o=m.getOrigin();
            ArrayList<S> l=new ArrayList<S>();
            l.add(api.stat.inhale(o,api.expr.Constant(o, false)));
            body=api.stat.block(o,l);
          } else {
            throw new HREError("unexpected body %s", Configuration.getDiagSyntax().print(m.getBody()));
          }
          ArrayList<E> pres=new ArrayList<E>();
          ArrayList<E> posts=new ArrayList<E>();
          Contract c=m.getContract();
          if (c!=null){
            for(ASTNode n:ASTUtils.conjuncts(c.invariant,StandardOperator.Star)){
              pres.add(n.apply(expr));
              posts.add(n.apply(expr));
            }
            for(ASTNode n:ASTUtils.conjuncts(c.pre_condition,StandardOperator.Star)){
              pres.add(n.apply(expr));
            }
            for(ASTNode n:ASTUtils.conjuncts(c.post_condition,StandardOperator.Star)){
              posts.add(n.apply(expr));
            }
          }
          api.prog.add_method(program,m.getOrigin(),m.name(),pres,posts,in,out,locals,labels,body);
          // TODO: fix refuted accounting.
          refuted.put(m.name(),stat.refuted);
          stat.refuted=null;
          break;
        }
        case Pure:{
          ArrayList<Triple<Origin,String,T>> args=new ArrayList<Triple<Origin,String,T>>();
          for(DeclarationStatement decl:m.getArgs()){
            args.add(new Triple<Origin,String,T>(decl.getOrigin(),decl.name(),decl.getType().apply(type)));
          }
          T t=m.getReturnType().apply(type);
          ArrayList<E> pres=new ArrayList<E>();
          ArrayList<E> posts=new ArrayList<E>();
          Contract c=m.getContract();
          if (c!=null){
            for(ASTNode n:ASTUtils.conjuncts(c.pre_condition,StandardOperator.Star)){
              pres.add(n.apply(expr));
            }
            for(ASTNode n:ASTUtils.conjuncts(c.post_condition,StandardOperator.Star)){
              posts.add(n.apply(expr));
            }
          }
          ASTNode b=m.getBody();
          E body=(b==null?null:b.apply(expr));
          api.prog.add_function(program,m.getOrigin(),m.name(),args,t,pres,posts,body);
          break;
        }
        case Predicate:{
          ASTNode b=m.getBody();
          E body=(b==null?null:b.apply(expr));
          ArrayList<Triple<Origin,String,T>> args=new ArrayList<Triple<Origin,String,T>>();
          for(DeclarationStatement decl:m.getArgs()){
            args.add(new Triple<Origin,String,T>(decl.getOrigin(),decl.name(),decl.getType().apply(type)));
          }
          api.prog.add_predicate(program,m.getOrigin(),m.name(),args,body);
          break;
        }
        default:
          throw new HREError("method kind %s not supported",m.kind);
        }
      } else if (entry instanceof ASTClass){
        ASTClass cl=(ASTClass) entry;
        if (cl.name().equals("Ref")&& cl.kind==ASTClass.ClassKind.Record){
          for(DeclarationStatement decl:cl.dynamicFields()){
            api.prog.add_field(program, decl.getOrigin(), decl.name(), decl.getType().apply(type));
          }
        } else {
          throw new HREError("bad class entry: %s",cl.name());
        }
      } else if (entry instanceof AxiomaticDataType) {
        AxiomaticDataType adt=(AxiomaticDataType)entry;
        ArrayList<DFunc> funcs=new ArrayList<DFunc>();
        for(Method m:adt.constructorsJava()){
          List<Triple<Origin,String,T>> args=new ArrayList<Triple<Origin,String,T>>();
          for(DeclarationStatement decl:m.getArgs()){
            args.add(new Triple<Origin,String,T>(decl.getOrigin(),decl.name(),decl.getType().apply(type)));
          }
          funcs.add(api.prog.dfunc(m.getOrigin(), m.name(), args,m.getReturnType().apply(type), adt.name(), m.isValidFlag(ASTFlags.UNIQUE) && m.getFlag(ASTFlags.UNIQUE)));
        }
        for(Method m:adt.mappingsJava()){
          List<Triple<Origin,String,T>> args=new ArrayList<Triple<Origin,String,T>>();
          for(DeclarationStatement decl:m.getArgs()){
            args.add(new Triple<Origin,String,T>(decl.getOrigin(),decl.name(),decl.getType().apply(type)));
          }
          funcs.add(api.prog.dfunc(m.getOrigin(), m.name(), args, m.getReturnType().apply(type), adt.name(), m.isValidFlag(ASTFlags.UNIQUE) && m.getFlag(ASTFlags.UNIQUE)));
        }
        ArrayList<DAxiom> axioms=new ArrayList<DAxiom>();
        for (Axiom axiom : adt.axiomsJava()) {
          axioms.add(api.prog.daxiom(axiom.getOrigin(),axiom.name(), axiom.rule().apply(expr), adt.name()));
        }
        ArrayList<String> pars=new ArrayList<String>();
        for (DeclarationStatement decl : adt.parametersJava()) {
          pars.add(decl.name());
        }
        api.prog.add_adt(program,adt.getOrigin(), adt.name(), funcs,axioms,pars);
      } else if(entry instanceof ASTSpecial){
        ASTSpecial s=(ASTSpecial)entry;
        throw new HREError("bad special declaration entry: %s",s.kind);
      } else {
        throw new HREError("bad entry: %s",entry.getClass());
      }
    }

    // Save the encountered sat check assert origins for later
    satCheckAsserts = stat.satCheckAsserts;

    hre.lang.System.Progress("conversion took %dms", tk.show());
    return program;
  }

  protected static <T, E, S, Program> void split_block(
      SilverTypeMap<T> type,
      SilverStatementMap<T, E, S> stat,
      BlockStatement block,
      List<Triple<Origin,String,T>> locals,
      List<String> labels,
      ArrayList<S> stats
  ) throws HREError {
    for(ASTNode node : block) {
      if(node instanceof DeclarationStatement) {
        locals.add(new Triple<>(node.getOrigin(), ((DeclarationStatement) node).name(), node.getType().apply(type)));
      } else if(node.isSpecial(ASTSpecial.Kind.Label)) {
        labels.add(((ASTSpecial) node).getArg(0).toString());
        stats.add(node.apply(stat));
      } else {
        stats.add(node.apply(stat));
      }
    }
  }

}
