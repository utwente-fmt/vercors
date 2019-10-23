package vct.silver;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.List;
import java.util.Set;

import hre.ast.MessageOrigin;
import hre.ast.Origin;
import hre.lang.HREError;
import vct.col.ast.stmt.decl.ASTFlags;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.stmt.decl.ASTClass;
import vct.col.ast.stmt.decl.Axiom;
import vct.col.ast.stmt.decl.AxiomaticDataType;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.decl.Contract;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.util.ContractBuilder;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.type.Type;
import vct.col.util.ASTFactory;
import vct.col.util.ASTUtils;
import vct.error.VerificationError;
import vct.util.Configuration;
import viper.api.ExpressionFactory;
import viper.api.ProgramFactory;
import viper.api.ViperAPI;
import viper.api.Triple;

public class VerCorsProgramFactory implements
    ProgramFactory<Origin, VerificationError, Type, ASTNode, ASTNode,
    Method, Axiom, ProgramUnit> {
  
  public VerCorsProgramFactory(ASTFactory<?> create){
    this.create=create;
  }
  
  public Hashtable<String,Set<Origin>> refuted;
  
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
      List<Triple<Origin,String,Type>> local, ASTNode body) {
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
      List<Triple<Origin,String,Type>> args, Type t, String domain) {
    enter(o);
    Method res=create.function_decl(t,null, name,create.to_decls(args),null);
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
  public <Err, T, E, S, DFunc, DAxiom, P> P convert(
      ViperAPI<Origin, Err, T, E, S, DFunc, DAxiom, P> api,
      ProgramUnit arg) {
    
    SilverTypeMap<T> type=new SilverTypeMap<T>(api);
    SilverExpressionMap<T, E> expr=new SilverExpressionMap<T,E>(api,type);
    SilverStatementMap<T, E, S> stat=new SilverStatementMap<T,E,S>(api,type,expr);
    P program=api.prog.program();
    
    long base=System.currentTimeMillis();
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
          S body;
          if (m.getBody() instanceof BlockStatement){
            BlockStatement block=(BlockStatement)m.getBody();
            ArrayList<S> stats=new ArrayList<S>();
            VerCorsProgramFactory.split_block(api.expr, type, stat, locals, block, stats);
            body=api.stat.block(block.getOrigin(),stats);
          } else if (m.getBody()==null){
            Origin o=m.getOrigin();
            ArrayList<S> l=new ArrayList<S>();
            l.add(api.stat.inhale(o,api.expr.Constant(o, false)));
            body=api.stat.block(o,l);
          } else {
            throw new HREError("unexpected body %s",Configuration.getDiagSyntax().print(m.getBody()));
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
          api.prog.add_method(program,m.getOrigin(),m.name(),pres,posts,in,out,locals,body);
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
          funcs.add(api.prog.dfunc(m.getOrigin(), m.name(), args,m.getReturnType().apply(type), adt.name()));
        }
        for(Method m:adt.mappingsJava()){
          List<Triple<Origin,String,T>> args=new ArrayList<Triple<Origin,String,T>>();
          for(DeclarationStatement decl:m.getArgs()){
            args.add(new Triple<Origin,String,T>(decl.getOrigin(),decl.name(),decl.getType().apply(type)));
          }
          funcs.add(api.prog.dfunc(m.getOrigin(), m.name(), args, m.getReturnType().apply(type), adt.name()));
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
        switch(s.kind){
          case Comment:
            // comments are not supported in silver.
            continue;
          default:
            throw new HREError("bad special declaration entry: %s",s.kind);

        }
      } else {
        throw new HREError("bad entry: %s",entry.getClass());
      }
    }
    long end=System.currentTimeMillis();
    hre.lang.System.Progress("conversion took %dms",end-base);
    return program;
  }

  protected static <T, E, S, Program> void split_block(
      ExpressionFactory<Origin,T, E> verifier,
      SilverTypeMap<T> type, SilverStatementMap<T, E, S> stat,
      List<Triple<Origin,String,T>> locals, BlockStatement block, ArrayList<S> stats)
      throws HREError {
    int i=0;
    int N=block.getLength();
    while(i<N && (block.get(i) instanceof DeclarationStatement)){
      DeclarationStatement decl=(DeclarationStatement)block.get(i);
      locals.add(new Triple<Origin, String, T>(decl.getOrigin(),decl.name(),decl.getType().apply(type)));
      i=i+1;
    }
    for(;i<N;i++){
      if (block.get(i) instanceof DeclarationStatement) {
        throw new HREError("illegal declaration");
      }
      S s=block.get(i).apply(stat);
      if (s!=null){
        stats.add(s);
      }
    }
  }

}
