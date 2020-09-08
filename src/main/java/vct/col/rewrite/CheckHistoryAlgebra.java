package vct.col.rewrite;

import hre.lang.HREError;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.concurrent.atomic.AtomicInteger;

import vct.col.ast.expr.Binder;
import vct.col.ast.expr.Dereference;
import vct.col.ast.expr.FieldAccess;
import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.expr.NameExpression;
import vct.col.ast.expr.NameExpressionKind;
import vct.col.ast.expr.OperatorExpression;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.decl.ASTClass;
import vct.col.ast.stmt.decl.ASTDeclaration;
import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.stmt.decl.ASTSpecial.Kind;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.ActionBlock;
import vct.col.ast.stmt.decl.AxiomaticDataType;
import vct.col.ast.stmt.decl.Contract;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.type.ASTReserved;
import vct.col.ast.type.ClassType;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.type.PrimitiveType;
import vct.col.ast.type.Type;
import vct.col.ast.util.AbstractRewriter;
import vct.col.ast.util.Configuration;
import vct.col.ast.util.ContractBuilder;
import vct.col.util.AstToId;
import vct.logging.ErrorMapping;
import vct.logging.VerCorsError.ErrorCode;

import static vct.col.ast.expr.StandardOperator.CurrentPerm;
import static vct.col.ast.expr.StandardOperator.EQ;
import static vct.col.ast.expr.StandardOperator.LT;
import static vct.col.ast.expr.StandardOperator.Old;
import static vct.col.ast.expr.StandardOperator.Perm;
import static vct.col.ast.expr.StandardOperator.PointsTo;
import static vct.col.ast.expr.StandardOperator.Star;
import static vct.col.ast.type.ASTReserved.FullPerm;

public class CheckHistoryAlgebra extends AbstractRewriter {
  
  public static final String ASSIGN_HIST="assign_hist";

  private AtomicInteger count=new AtomicInteger();
  
  public static enum Mode { AxiomVerification, ProgramVerification };
  public final Mode mode;
  
  public CheckHistoryAlgebra(ProgramUnit source, Mode mode, ErrorMapping map) {
    super(source);
    this.mode=mode;
    map.add(ASSIGN_HIST,
        ErrorCode.CallPreCondition,
        ErrorCode.AssignmentFailed);
  }
  
  private Type adt_type;
  private AxiomaticDataType adt;
  private ASTClass hist_class;

  private String histName(String name) {
    return name + "_hist_hist";
  }
  
  @Override
  public void visit(ASTClass cl){
    boolean is_algebra=false;
    for(Method m:cl.dynamicMethods()){
      is_algebra|=m.getReturnType().isPrimitive(PrimitiveSort.Process);
    }
    if (is_algebra){
      boolean is_history = cl.name().equals("History");
      adt=create.adt("Process");
      adt_type=create.class_type("Process");
      DeclarationStatement proc_p1=create.field_decl("p1", adt_type);
      DeclarationStatement proc_p2=create.field_decl("p2", adt_type);
      adt.add_cons(create.function_decl(create.primitive_type(PrimitiveSort.Boolean),null,"p_is_choice",
          new DeclarationStatement[]{proc_p1,proc_p2},null));
      adt.add_cons(create.function_decl(adt_type,null,"p_empty",
          new DeclarationStatement[]{},null));
      adt.add_cons(create.function_decl(adt_type,null,"p_merge",
          new DeclarationStatement[]{proc_p1,proc_p2},null));
      adt.add_cons(create.function_decl(adt_type,null,"p_choice",
          new DeclarationStatement[]{proc_p1,proc_p2},null));
      adt.add_cons(create.function_decl(adt_type,null,"p_seq",
          new DeclarationStatement[]{proc_p1,proc_p2},null));
      adt.add_axiom(create.axiom("empty_1L",
          create.forall(create.constant(true),
              create.expression(StandardOperator.EQ,
                  create.domain_call("Process", "p_merge",
                      create.domain_call("Process", "p_empty"),
                      create.local_name("p")
                  ),
                  create.local_name("p")
              ),create.field_decl("p", adt_type)
          )
      ));
      adt.add_axiom(create.axiom("empty_2L",
          create.forall(create.constant(true),
              create.expression(StandardOperator.EQ,
                  create.domain_call("Process", "p_seq",
                      create.domain_call("Process", "p_empty"),
                      create.local_name("p")
                  ),
                  create.local_name("p")
              ),create.field_decl("p", adt_type)
          )
      ));
      adt.add_axiom(create.axiom("empty_2R",
          create.forall(create.constant(true),
              create.expression(StandardOperator.EQ,
                  create.domain_call("Process", "p_seq",
                      create.local_name("p"),
                      create.domain_call("Process", "p_empty")
                  ),
                  create.local_name("p")
              ),create.field_decl("p", adt_type)
          )
      ));
      adt.add_axiom(create.axiom("choice_L",
          create.binder(
              Binder.Forall,
              create.primitive_type(PrimitiveSort.Boolean),
              new DeclarationStatement[]{
                 create.field_decl("p1", adt_type)
                ,create.field_decl("p2", adt_type)},
              null,
              create.constant(true),
              create.pattern(create.domain_call("Process", "p_is_choice",
                  create.domain_call("Process", "p_choice",
                      create.local_name("p1"),create.local_name("p2")
                  ),
                  create.local_name("p1")
              ))
          )
      ));
      adt.add_axiom(create.axiom("choice_R",
          create.binder(
              Binder.Forall,
              create.primitive_type(PrimitiveSort.Boolean),
              new DeclarationStatement[]{
                 create.field_decl("p1", adt_type)
                ,create.field_decl("p2", adt_type)},
              null,
              create.constant(true),
              create.pattern(create.domain_call("Process", "p_is_choice",
                  create.domain_call("Process", "p_choice",
                      create.local_name("p1"),create.local_name("p2")
                  ),
                  create.local_name("p2")
              ))
          )
      ));
      adt.add_axiom(create.axiom("choice_dist",
          create.binder(
              Binder.Forall,
              create.primitive_type(PrimitiveSort.Boolean),
              new DeclarationStatement[]{
                 create.field_decl("p1", adt_type)
                ,create.field_decl("p2", adt_type)
                ,create.field_decl("p3", adt_type)},
              null,
              create.constant(true),
              create.expression(StandardOperator.EQ,
                  create.domain_call("Process", "p_seq",
                      create.domain_call("Process","p_choice",create.local_name("p1"),create.local_name("p2")),
                      create.local_name("p3")
                  ),
                  create.domain_call("Process", "p_choice",
                      create.domain_call("Process", "p_seq",create.local_name("p1"),create.local_name("p3")),                      
                      create.domain_call("Process", "p_seq",create.local_name("p2"),create.local_name("p3"))
                  )
              )
          )
      ));
      adt.add_axiom(create.axiom("seq_assoc",
          create.binder(
              Binder.Forall,
              create.primitive_type(PrimitiveSort.Boolean),
              new DeclarationStatement[]{
                 create.field_decl("p1", adt_type)
                ,create.field_decl("p2", adt_type)
                ,create.field_decl("p3", adt_type)},
              new ASTNode[][]{
                new ASTNode[]{create.domain_call("Process", "p_seq",
                    create.domain_call("Process","p_seq",create.local_name("p1"),create.local_name("p2")),
                    create.local_name("p3")
                )}
              },
              create.constant(true),
              create.expression(StandardOperator.EQ,
                  create.domain_call("Process", "p_seq",
                      create.domain_call("Process","p_seq",create.local_name("p1"),create.local_name("p2")),
                      create.local_name("p3")
                  ),
                  create.domain_call("Process", "p_seq",create.local_name("p1"),
                      create.domain_call("Process", "p_seq",create.local_name("p2"),create.local_name("p3"))
                  )
              )
          )
      ));
      switch(mode){
      case AxiomVerification:{
        ASTClass res = create.new_class(cl.name(), new DeclarationStatement[0], null);
        for(Method m:cl.dynamicMethods()){
          if (m.getKind()==Method.Kind.Constructor){
            continue;
          } else if (m.getReturnType().isPrimitive(PrimitiveSort.Process)){
            add_process_to_adt(m);
          } else if (m.getReturnType().isPrimitive(PrimitiveSort.Resource)) {
            // drop predicate.
          } else {
            res.add_dynamic(rewrite(m));
          }
        }
        result=res;
        return;        
      }
      case ProgramVerification:{
        hist_class = create.new_class(cl.name(), new DeclarationStatement[0], null);
        for(Method m:cl.dynamicMethods()){
          if (m.getKind()==Method.Kind.Constructor){
            hist_class.add_dynamic(rewrite(m));
          } else if (m.getReturnType().isPrimitive(PrimitiveSort.Process)){
            add_process_to_adt(m);
            if (m.getBody()==null) {
              add_begin_and_commit_to_class(m,is_history);
            }
          } else if (m.getReturnType().isPrimitive(PrimitiveSort.Resource)) {
            hist_class.add(rewrite(m));
          } else {
            add_lemma_to_adt(m);
          }
        }
        for(DeclarationStatement m:cl.dynamicFields()){
          hist_class.add_dynamic(create.field_decl(m.name() + "_hist_value",m.getType(), rewrite(m.initJava())));
          hist_class.add_dynamic(create.field_decl(m.name() + "_hist_init",m.getType()));
          hist_class.add_dynamic(create.field_decl(m.name() + "_hist_act",m.getType()));
          hist_class.add_dynamic(create.field_decl(m.name() + "_hist_write",m.getType()));
          hist_class.add_dynamic(create.field_decl(m.name() + "_hist_free",m.getType()));
          hist_class.add_dynamic(create.field_decl(histName(m.name()), m.getType()));
          hist_class.add_dynamic(create.field_decl(m.name() + "_hist_action",m.getType()));
          add_setters_and_getter(hist_class, m.name(), m.getType());
        }
        
        add_begin_hist_of_end_future_method(cl);
        add_split_merge_methods(cl);

        DeclarationStatement args[]=new DeclarationStatement[2];
        args[0]=create.field_decl("fr",create.primitive_type(PrimitiveSort.Fraction));
        args[1]=create.field_decl("proc",adt_type);
        hist_class.add(create.predicate("hist_idle", null, args));
        result=hist_class;
        return;
      }}
    } else {
      super.visit(cl);
    }
  }

  private void add_setters_and_getter(ASTClass cl, String name, Type type) {
    ContractBuilder hist_set_cb=new ContractBuilder();
    ASTNode var=create.dereference(create.diz(), name+"_hist_value");
    ASTNode val=create.local_name("value");
    ASTNode wr=create.dereference(create.diz(), name+"_hist_write");
    ASTNode full=create.reserved_name(FullPerm);
    hist_set_cb.requires(create.expression(Perm,var,full));
    hist_set_cb.requires(create.expression(Perm,wr,full));
    hist_set_cb.ensures(create.expression(PointsTo,var,full,val));
    hist_set_cb.ensures(create.expression(Perm,wr,full));
    cl.add_dynamic(create.method_decl(
        create.primitive_type(PrimitiveSort.Void),
        hist_set_cb.getContract(),
        "hist_set_"+name,
        new DeclarationStatement[]{create.field_decl("value", type)},
        null
    ));
  }

  protected void add_begin_hist_of_end_future_method(ASTClass cl) {
    Method begin_hist;
    boolean hist=hist_class.getName().equals("History");
    ContractBuilder cb=new ContractBuilder();
    ASTNode diz=create.reserved_name(ASTReserved.This);
    ASTNode full=create.reserved_name(FullPerm);
    for(DeclarationStatement d:cl.dynamicFields()){
      
      if (hist){
        cb.requires(free_perm(diz,d.name(), full));
        cb.ensures(hist_perm(diz,d.name(), full));
        cb.ensures(create.expression(Perm,create.dereference(create.diz(), d.name() + "_hist_init"),create.reserved_name(FullPerm)));
      } else {
        cb.requires(hist_perm(diz,d.name(), full));
        cb.ensures(free_perm(diz,d.name(), full));        
        cb.requires(create.expression(Perm,create.dereference(create.diz(), d.name() + "_hist_init"),create.reserved_name(FullPerm)));
      }
      cb.ensures(create.expression(EQ
          , create.dereference(create.diz(), d.name() + "_hist_value")
          , create.expression(Old,create.dereference(create.diz(), d.name() + "_hist_value"))
      ));
      if (hist){
        cb.ensures(create.expression(EQ
          , create.dereference(create.diz(), d.name() + "_hist_init")
          , create.expression(Old,create.dereference(create.diz(), d.name() + "_hist_value"))
        ));
      } else {
        cb.ensures(create.expression(EQ
          , create.dereference(create.diz(), d.name() + "_hist_value")
          , create.expression(Old,create.dereference(create.diz(), d.name() + "_hist_init"))
        ));
      }
    }
    ASTNode tmp=create.invokation(create.diz(), null, "hist_idle",
        create.reserved_name(ASTReserved.FullPerm),
        create.domain_call("Process", "p_empty")
    );
    if (hist){ cb.ensures(tmp); } else { cb.requires(tmp); }
    begin_hist=create.method_decl(
        create.primitive_type(PrimitiveSort.Void),
        cb.getContract(),
        hist?"begin_hist":"end_future",
        new DeclarationStatement[0],
        null
    );
    hist_class.add(begin_hist);
  }

  protected void add_split_merge_methods(ASTClass cl) {
    DeclarationStatement args[]=new DeclarationStatement[4];
    args[0]=create.field_decl("frac1",create.primitive_type(PrimitiveSort.Fraction));
    args[1]=create.field_decl("proc1",adt_type);
    args[2]=create.field_decl("frac2",create.primitive_type(PrimitiveSort.Fraction));
    args[3]=create.field_decl("proc2",adt_type);

    ASTNode fracSumBound = create.expression(
            StandardOperator.LTE,
            create.expression(StandardOperator.Plus, create.local_name("frac1"), create.local_name("frac2")),
            create.reserved_name(FullPerm)
    );

    ASTNode split1=create.invokation(create.diz(), null, "hist_idle",
        create.local_name("frac1"),create.local_name("proc1")
    );
    ASTNode split2=create.invokation(create.diz(), null, "hist_idle",
        create.local_name("frac2"),create.local_name("proc2")
    );
    ASTNode merge=create.invokation(create.diz(), null, "hist_idle",
        create.expression(StandardOperator.Plus,create.local_name("frac1"),create.local_name("frac2")),
        create.domain_call("Process","p_merge",create.local_name("proc1"),create.local_name("proc2"))
    );
    ContractBuilder split_cb=new ContractBuilder();
    ContractBuilder merge_cb=new ContractBuilder();

    split_cb.requires(fracSumBound);
    split_cb.requires(merge);
    split_cb.ensures(split1);
    split_cb.ensures(split2);

    merge_cb.requires(fracSumBound);
    merge_cb.requires(split1);
    merge_cb.requires(split2);
    merge_cb.ensures(merge);
    
    hist_class.add(create.method_decl(
        create.primitive_type(PrimitiveSort.Void),
        split_cb.getContract(),
        "split",
        args,
        null
    ));
    hist_class.add(create.method_decl(
        create.primitive_type(PrimitiveSort.Void),
        merge_cb.getContract(),
        "merge",
        args,
        null
    ));
    
  }
    
  private void add_begin_and_commit_to_class(Method m,boolean is_history) {
    Type returns=create.primitive_type(PrimitiveSort.Void);
    Contract c=m.getContract();
    int N=m.getArity();
    int K=c.accesses==null?0:c.accesses.length;
    DeclarationStatement args_short[]=new DeclarationStatement[2+K];
    DeclarationStatement args_long[]=new DeclarationStatement[2+K+N];
    ASTNode do_args[]=new ASTNode[2+K];
    ASTNode args[]=new ASTNode[N];
    args_short[0]=args_long[0]=create.field_decl("fr",create.primitive_type(PrimitiveSort.Fraction));
    args_short[1]=args_long[1]=create.field_decl("proc",adt_type);
    String access_name[]=new String[K];
    do_args[0]=create.local_name("fr");
    do_args[1]=create.local_name("proc");
    for(int i=0;i<K;i++){
      access_name[i]=((FieldAccess)c.accesses[i]).name() + "_frac";
      do_args[2+i]=create.local_name(access_name[i]);
      args_short[2+i]=args_long[2+i]=create.field_decl(access_name[i],create.primitive_type(PrimitiveSort.ZFraction));
    }
    for(int i=0;i<N;i++){
      args_long[i+2+K]=create.field_decl(m.getArgument(i),m.getArgType(i));
      args[i]=create.local_name(m.getArgument(i));
    }
    ContractBuilder begin_cb=new ContractBuilder();
    ContractBuilder commit_cb=new ContractBuilder();
    ASTNode proc=create.local_name("proc");
    ASTNode action_proc=create.domain_call("Process", "p_seq",
        create.domain_call("Process","p_"+m.name(), args), create.local_name("proc"));
    ASTNode proc_action=create.domain_call("Process", "p_seq", create.local_name("proc"),create.domain_call("Process","p_"+m.name(), args));
    begin_cb.requires(create.expression(StandardOperator.NEQ,create.local_name("fr"),create.reserved_name(ASTReserved.NoPerm)));
    begin_cb.ensures(create.expression(StandardOperator.NEQ,create.local_name("fr"),create.reserved_name(ASTReserved.NoPerm)));
    commit_cb.requires(create.expression(StandardOperator.NEQ,create.local_name("fr"),create.reserved_name(ASTReserved.NoPerm)));
    commit_cb.ensures(create.expression(StandardOperator.NEQ,create.local_name("fr"),create.reserved_name(ASTReserved.NoPerm)));
    begin_cb.requires(create.invokation(create.diz(), null,"hist_idle",create.local_name("fr"),is_history?proc:action_proc));
    begin_cb.ensures(create.invokation(create.diz(), null,"hist_do_"+m.name(), do_args));
    commit_cb.requires(create.invokation(create.diz(), null,"hist_do_"+m.name(), do_args));
    commit_cb.ensures(create.invokation(create.diz(), null,"hist_idle",create.local_name("fr"),is_history?proc_action:proc));
    HashMap<NameExpression,ASTNode> old_map=new HashMap<NameExpression, ASTNode>();
    HashMap<NameExpression,ASTNode> new_map=new HashMap<NameExpression, ASTNode>();
    if (c.modifies!=null) for(ASTNode n:c.modifies){
      add_req_ens(begin_cb, commit_cb, old_map, new_map, n, create.reserved_name(FullPerm));
    }
    if (c.accesses!=null) for(ASTNode n:c.accesses){
      add_req_ens(begin_cb, commit_cb, old_map, new_map, n, null);
    }
    Simplify simp=new Simplify(this);
    Substitution sigma=new Substitution(source(),old_map);
    ApplyOld rw_old=new ApplyOld(sigma);
    Substitution rw_new=new Substitution(source(),new_map);
    begin_cb.requires(rw_new.rewrite(simp.rewrite(c.pre_condition)));
    commit_cb.requires(rw_new.rewrite(rw_old.rewrite(simp.rewrite(c.post_condition))));

    Method begin=create.method_decl(returns,begin_cb.getContract(), m.name() + "_begin", args_long,null);
    Method commit=create.method_decl(returns,commit_cb.getContract(), m.name() + "_commit", args_long,null);
    hist_class.add_dynamic(begin);
    hist_class.add_dynamic(commit);
    hist_class.add_dynamic(create.predicate("hist_do_"+m.name(), null,args_short));
  }

  private void add_req_ens(ContractBuilder begin_cb,
      ContractBuilder commit_cb, HashMap<NameExpression, ASTNode> old_map,
      HashMap<NameExpression, ASTNode> new_map, ASTNode n,
      ASTNode frac) {
    String name=((FieldAccess)n).name();
    ASTNode nact=create.dereference(create.diz(), name+"_hist_act");
    ASTNode obj=((FieldAccess)n).object();
    n=create.dereference(((FieldAccess)n).object(), name + "_hist_value");
    old_map.put(create.field_name(name),create.dereference(create.diz(), name+"_hist_act"));
    new_map.put(create.field_name(name),create.dereference(create.diz(), name+"_hist_value"));

    
    if (frac==null){
      frac=create.local_name(name+"_frac");
    }
    begin_cb.requires(create.expression(StandardOperator.NEQ,
        frac,create.reserved_name(ASTReserved.NoPerm)));
    commit_cb.requires(create.expression(StandardOperator.NEQ,
        frac,create.reserved_name(ASTReserved.NoPerm)));
    begin_cb.ensures(create.expression(Perm,nact,frac));
    commit_cb.requires(create.expression(Perm,nact,frac));
    begin_cb.requires(hist_perm(obj,name,frac));
    begin_cb.ensures(action_perm(obj,name,frac));
    begin_cb.ensures(create.expression(EQ,n,nact));
    begin_cb.ensures(create.expression(EQ,n,create.expression(Old,n)));
    commit_cb.requires(action_perm(obj,name,frac));
    commit_cb.ensures(hist_perm(obj,name,frac));
    commit_cb.ensures(create.expression(EQ,n,create.expression(Old,n)));
  }

  private ASTNode rebuild(ASTNode x,ASTNode y){
    if (x.isa(StandardOperator.Mult)){
      ASTNode lhs=((OperatorExpression)x).arg(0);
      ASTNode rhs=((OperatorExpression)x).arg(1);
      return create.domain_call("Process","p_seq",rewrite(lhs),rebuild(rhs,y));
    } else {
      return create.domain_call("Process","p_seq",rewrite(x),y);
    }
  }
  protected void add_lemma_to_adt(Method m) {
    Contract c=m.getContract();
    int N=m.getArity();
    ASTNode eq=c.post_condition;
    if (!eq.isa(EQ)){
      Abort("cannot generate axiom for %s", Configuration.getDiagSyntax().print(eq));
    }
    ASTNode lhs=((OperatorExpression)c.post_condition).arg(0);
    ASTNode rhs=((OperatorExpression)c.post_condition).arg(1);
    lhs=rebuild(lhs,create.local_name("p"));
    rhs=create.domain_call("Process","p_seq",rewrite(rhs),create.local_name("p"));
    ASTNode [] arg_names = new ASTNode[N];
    for(int i=0;i<N;i++){
      arg_names[i]=create.local_name(m.getArgument(i));
    }
    eq=create.binder(
          Binder.Forall,
          create.primitive_type(PrimitiveSort.Boolean),
          rewrite(create.field_decl("p",adt_type),m.getArgs()),
          new ASTNode[][]{new ASTNode[]{lhs}},
          create.constant(true),
          create.expression(EQ,lhs,rhs)
      );
    adt.add_axiom(create.axiom(m.name() + "_post",eq));
  }

  protected void add_process_to_adt(Method m) {
    DeclarationStatement args[]=rewrite(m.getArgs());
    ASTNode m_body=m.getBody();
    int N=m.getArity();
    ASTNode [] arg_names = new ASTNode[N];
    for(int i=0;i<N;i++){
      arg_names[i]=create.local_name(m.getArgument(i));
    }
    if (m_body!=null){
      ASTNode eq=create.expression(StandardOperator.EQ,
              rewrite(m.getBody()),
              create.domain_call("Process","p_" + m.name(), arg_names)
          );
      if(N>0){
        ASTNode trigger;
        if (m.getBody().isa(StandardOperator.Mult)){
          trigger=rewrite(m.getBody());
        } else {
          trigger=create.domain_call("Process","p_" + m.name(), arg_names);
        }
        eq=create.binder(
          Binder.Forall,
          create.primitive_type(PrimitiveSort.Boolean),
          copy_rw.rewrite(m.getArgs()),
          new ASTNode[][]{new ASTNode[]{trigger}},
          create.constant(true),
          eq
        );
      }
      adt.add_axiom(create.axiom(m.name() + "_def_1",eq));
    }
    {
      ASTNode tmp=create.domain_call("Process","p_" + m.name(), arg_names);
      ASTNode lhs=create.domain_call("Process","p_seq",create.local_name("p"),tmp);
      ASTNode rhs=create.domain_call("Process","p_seq",create.local_name("p"),
          create.domain_call("Process","p_seq",tmp,create.domain_call("Process","p_empty")));
      ASTNode eq=create.binder(
          Binder.Forall,
          create.primitive_type(PrimitiveSort.Boolean),
          copy_rw.rewrite(create.field_decl("p",adt_type),m.getArgs()),
          new ASTNode[][]{new ASTNode[]{lhs}},
          create.constant(true),
          create.expression(EQ,lhs,rhs)
        );
      adt.add_axiom(create.axiom(m.name() + "_def_2", eq));
    }
    adt.add_cons(create.function_decl(adt_type, null,"p_" + m.name(), args, null));
  }
  
  @Override
  public void visit(MethodInvokation e){
    Method m=e.getDefinition();
    if (m.getReturnType().isPrimitive(PrimitiveSort.Process)){
      result=create.domain_call("Process", "p_"+e.method(),rewrite(e.getArgs()));
    } else {
      ASTNode in_args[]=e.getArgs();
      ASTNode args[]=new ASTNode[in_args.length];
      for(int i=0;i<in_args.length;i++){
        if (in_args[i].labels()>0 && in_args[i].isConstant(0)){
          args[i]=create.local_name(in_args[i].getLabel(0).getName());
        } else {
          args[i]=rewrite(in_args[i]);
        }
      }
      MethodInvokation res=create.invokation(rewrite(e.object()), e.dispatch(), e.method(), args);
      res.set_before(rewrite(e.get_before()));
      res.set_after(rewrite(e.get_after()));
      result=res;
    }
  }
  
  @Override
  public void visit(NameExpression e){
    // FIXME remove this if it doesn't break anything. Probably related to the rewrite of labeled args in MethodInvokation
    /*if (e.getKind()== NameExpressionKind.Label){
      result=create.unresolved_name(e.getName());
    } else */if (e.isReserved(ASTReserved.EmptyProcess)) {
      result=create.domain_call("Process", "p_empty");
    } else {
      super.visit(e);
    }
  }

  private boolean isProcessRef(ASTNode n){
    String type=n.getType().toString();
    return type.equals("History")||type.equals("Future");
  }
  private boolean isHistoryRef(ASTNode n){
    String type=n.getType().toString();
    return type.equals("History");
  }
  private boolean isFutureRef(ASTNode n){
    String type=n.getType().toString();
    return type.equals("Future");
  }
  
  @Override
  public void visit(PrimitiveType t){
    if (t.sort==PrimitiveSort.Process){
      result=adt_type;
    } else {
      super.visit(t);
    }
  }

  @Override
  public void visit(OperatorExpression e){
    switch(e.operator()){
    case CurrentPerm:{
      ASTNode arg=e.arg(0);
      if (arg instanceof FieldAccess){
        FieldAccess fa=(FieldAccess)arg;
        if (isFutureRef(fa.object()) || isHistoryRef(fa.object())){
          Abort("current permission on history/future field is unimplemented");
        }
      }
      break;
    }
    case AbstractState:{
      ASTNode data=rewrite(e.arg(0));
      ASTNode full=create.reserved_name(FullPerm);
      ASTClass cl=source().find((ClassType)e.arg(0).getType());
      ArrayList<ASTNode> props=new ArrayList<ASTNode>();
      HashMap<NameExpression,ASTNode> map=new HashMap<NameExpression, ASTNode>();
      Substitution sigma=new Substitution(source(),map);
      for(DeclarationStatement decl:cl.dynamicFields()){
        props.add(create.expression(StandardOperator.Perm,
            create.dereference(data,decl.name() + "_hist_init"),full));
        map.put(create.field_name(decl.name()),
            create.dereference(data,decl.name() + "_hist_init"));
      }
      props.add(rewrite(sigma.rewrite(e.arg(1))));
      result=create.fold(StandardOperator.Star,props);
      return;
    }
    case Or:
      if(e.getType().isPrimitive(PrimitiveSort.Process)){
        result=create.domain_call("Process","p_merge",rewrite(e.argsJava()));
        return;
      }
      break;
    case Plus:
      if(e.getType().isPrimitive(PrimitiveSort.Process)){
        result=create.domain_call("Process","p_choice",rewrite(e.argsJava()));
        return;
      }
      break;
    case Mult:
      if(e.getType().isPrimitive(PrimitiveSort.Process)){
        result=create.domain_call("Process","p_seq",rewrite(e.argsJava()));
        return;
      }
      break;
    case Future:
    case History:{
      ASTNode hist=e.arg(0);
      ASTNode frac=e.arg(1);
      ASTNode proc=e.arg(2);
      result=create.invokation(rewrite(hist),null,"hist_idle",rewrite(frac),rewrite(proc));
      return;
    }
    case ActionPerm:{
      if (e.arg(0)instanceof FieldAccess){
        FieldAccess f=(FieldAccess)e.arg(0);
        ASTNode frac=rewrite(e.arg(1));
        if (isProcessRef(f.object())){
          result=action_perm(rewrite(f.object()), f.name(), frac);
        } else {
          throw new HREError("APerm on non-history variable.");
        }
        return;
      }
      throw new HREError("APerm on non-history variable.");
    }
    case HistoryPerm:{
      if (e.arg(0)instanceof FieldAccess){
        FieldAccess f=(FieldAccess)e.arg(0);
        ASTNode frac=rewrite(e.arg(1));
        if (isProcessRef(f.object())){
          result=hist_perm(rewrite(f.object()) ,f.name(), frac);
        } else {
          throw new HREError("HPerm on non-history variable.");
        }
        return;
      }
      throw new HREError("HPerm on non-history variable.");
    }
    case Perm:{
      if (e.arg(0)instanceof FieldAccess){
        FieldAccess f=(FieldAccess)e.arg(0);
        ASTNode frac=rewrite(e.arg(1));
        if (isProcessRef(f.object())){
          result=free_perm(rewrite(f.object()), f.name(), frac);
        } else {
          result=create.expression(Perm,create.dereference(rewrite(f.object()), f.name()), frac);
        }
        return;
      }
      break;
    }
    case PointsTo:{
      if (e.arg(0)instanceof FieldAccess){
        FieldAccess f=(FieldAccess)e.arg(0);
        if (isProcessRef(f.object())){
          ASTNode frac=rewrite(e.arg(1));
          ASTNode val=rewrite(e.arg(2));
          result=free_pointsto(rewrite(f.object()) ,f.name(), frac,val);
          return;
        }
      }
      break;
    }
    default:
      break;
    }
    super.visit(e);
  }
  
  private ASTNode free_pointsto(ASTNode obj, String name, ASTNode frac,ASTNode val) {
    return create.fold(StandardOperator.Star
        ,create.expression(Perm,create.dereference(obj,name+"_hist_value"),frac)
        ,create.expression(Perm,create.dereference(obj,name+"_hist_write"),frac)
        ,create.expression(Perm,create.dereference(obj,name+"_hist_free"),frac)
        ,create.expression(EQ,create.dereference(obj,name+"_hist_value"),val)
    );
  }
  private ASTNode free_perm(ASTNode obj, String name, ASTNode frac) {
    return create.fold(StandardOperator.Star
        ,create.expression(Perm,create.dereference(obj,name+"_hist_value"),frac)
        ,create.expression(Perm,create.dereference(obj,name+"_hist_write"),frac)
        ,create.expression(Perm,create.dereference(obj,name+"_hist_free"),frac)
    );
  }
  private ASTNode hist_perm(ASTNode obj, String name, ASTNode frac) {
    return create.fold(StandardOperator.Star
        ,create.expression(Perm,create.dereference(obj,name+"_hist_value"),frac)
        ,create.expression(Perm, create.dereference(obj, histName(name)), frac)
    );
  }
  private ASTNode action_perm(ASTNode obj, String name, ASTNode frac) {
    return create.fold(StandardOperator.Star
        ,create.expression(Perm,create.dereference(obj,name+"_hist_value"),frac)
        ,create.expression(Perm,create.dereference(obj,name+"_hist_write"),frac)
        ,create.expression(Perm,create.dereference(obj,name+"_hist_action"),frac)
    );
  }

  protected boolean in_action=false;

  int counter = 0;
  
  @Override
  public void visit(ActionBlock ab){
    if (in_action){
      Fail("nested action block");
    }
    in_action=true;
    MethodInvokation act=(MethodInvokation)ab.action();
    ASTNode hist=rewrite(ab.history());
    ASTNode frac=rewrite(ab.fraction());
    ASTNode p_expr=rewrite(ab.process());
    p_expr.clearLabels();
    ArrayList<ASTNode> args=new ArrayList<ASTNode>();
    args.add(frac);
    args.add(p_expr);
    BlockStatement res=create.block();
    ArrayList<NameExpression> names=new ArrayList<NameExpression>();
    Contract ac=act.getDefinition().getContract();
    for(ASTNode n:act.getArgs()){
      args.add(rewrite(n));
    }
    BlockStatement body = create.block();
    if (ac.accesses!=null) for(ASTNode n:ac.accesses){
      create.enter(n);

      String name = "f_" + AstToId.toId(n) + counter++;
      String fieldName = ((FieldAccess) n).name();
      names.add(create.local_name(name));
      args.add(create.local_name(name));
      res.add(create.field_decl(name, create.primitive_type(PrimitiveSort.ZFraction)));
      CheckProcessAlgebra.ensureFreshZfracPresent(source(), target(), create);
      body.add(create.assignment(create.local_name(name), create.invokation(null, null, CheckProcessAlgebra.FRESH_ZFRAC_NAME)));

      body.add(create.special(Kind.Assert, create.expression(LT,
              create.constant(0),
              create.expression(CurrentPerm, create.dereference(copy_rw.rewrite(hist), histName(fieldName))
              ))));

      body.add(create.special(Kind.Assert, create.expression(LT,
              create.constant(0),
              create.expression(CurrentPerm, create.dereference(copy_rw.rewrite(hist), histName(fieldName))
              ))));

      body.add(create.special(Kind.Inhale, create.fold(Star,
              create.expression(LT, create.constant(0), create.local_name(name)),
              create.expression(LT, create.local_name(name), create.expression(CurrentPerm, create.dereference(copy_rw.rewrite(hist), histName(fieldName)))),
              create.expression(LT, create.local_name(name), create.expression(CurrentPerm, create.dereference(copy_rw.rewrite(hist), fieldName + "_hist_value")))
              )));

      create.leave();
    }
    body.add(create.invokation(hist, null, act.method()+"_begin", args.toArray(new ASTNode[0])));
    for(ASTNode n:(BlockStatement)ab.block()){
      body.add(rewrite(n));
    }
    body.add(create.invokation(hist, null, act.method()+"_commit", args.toArray(new ASTNode[0])));
    res.add(body);
    result=res;
    in_action=false;
  }
  
  @Override
  public void visit(ASTSpecial s){
    switch(s.kind){
    case ChooseHistory:{
      ASTNode hist=s.args[0];
      ASTNode frac=s.args[1];
      ASTNode P1=s.args[2];
      ASTNode P2=s.args[3];
      currentBlock.add(create.special(ASTSpecial.Kind.Exhale,
          create.invokation(rewrite(hist),null,"hist_idle",rewrite(frac),rewrite(P1))));
      currentBlock.add(create.special(ASTSpecial.Kind.Assert,
          create.domain_call("Process","p_is_choice",rewrite(P1),rewrite(P2))));
      currentBlock.add(create.special(ASTSpecial.Kind.Inhale,
          create.invokation(rewrite(hist),null,"hist_idle",rewrite(frac),rewrite(P2))));
      result=null;
      break;
    }
    case SplitHistory:{
      ASTNode hist=s.args[0];
      ASTNode args[]=new ASTNode[4];
      for(int i=0;i<4;i++){
        args[i]=rewrite(s.args[i+1]);
      }
      result=create.invokation(rewrite(hist), null , "split", args);
      break;
    }
    case MergeHistory:{
      ASTNode hist=s.args[0];
      ASTNode args[]=new ASTNode[4];
      for(int i=0;i<4;i++){
        args[i]=rewrite(s.args[i+1]);
      }
      result=create.invokation(rewrite(hist), null , "merge", args);
      break;
    }
    case CreateHistory:{
      ASTNode hist=rewrite(s.args[0]);
      result=create.invokation(hist, null , "begin_hist");
      break;
    }
    case CreateFuture:{
      setUpEffect(s);
      break;
    }
    case DestroyFuture:{
      ASTNode future=rewrite(s.args[0]);
      result=create.invokation(future, null , "end_future");
      break; 
    }
    case DestroyHistory:{
      setUpEffect(s);
      break;
    }
    default:
        super.visit(s);
        break;
    }
  }

  private void setUpEffect(ASTSpecial s) {
    ASTNode model=s.args[0];
    boolean hist=isHistoryRef(model);
    String name=(hist?"end_hist_":"begin_future_")+count.incrementAndGet();
    ASTNode proc=s.args[1];
    if (!(proc instanceof MethodInvokation)){
      Fail("second argument of %s must be a defined process",hist?"destroy":"create");
    }
    MethodInvokation effect=(MethodInvokation)proc;
    
    
    
    ClassType ct=(ClassType)model.getType();
    if (ct==null){
      Abort("type of %s is null",Configuration.getDiagSyntax().print(model));
    }
    ASTClass cl=source().find(ct);
    
    
    Method def=effect.getDefinition();
    
    Method end_hist;
    
    final HashMap<String,ASTNode> map=new HashMap<String, ASTNode>();
    final HashMap<String,ASTNode> new_map=new HashMap<String, ASTNode>();
    AbstractRewriter sigma=new AbstractRewriter(source()){
      @Override
      public void visit(Dereference d){
        ASTNode n = map.get(d.field());
        if (n==null){
          super.visit(d);
        } else {
          result=copy_rw.rewrite(n);
        }
      }
      @Override
      public void visit(FieldAccess d){
        ASTNode n=map.get(d.name());
        if (n==null){
          super.visit(d);
        } else {
          result=copy_rw.rewrite(n);
        }
      }
    };
    AbstractRewriter new_sigma=new AbstractRewriter(source()){
      @Override
      public void visit(Dereference d){
        ASTNode n = new_map.get(d.field());
        if (n==null){
          super.visit(d);
        } else {
          result=copy_rw.rewrite(n);
        }
      }
      @Override
      public void visit(FieldAccess d){
        ASTNode n=new_map.get(d.name());
        Warning("name %s",d.name());
        if (n==null){
          super.visit(d);
        } else {
          result=copy_rw.rewrite(n);
        }
      }
    };
    ApplyOld rename_old=new ApplyOld(sigma);
    ContractBuilder cb=new ContractBuilder();
    ASTNode diz=create.reserved_name(ASTReserved.This);
    ASTNode full=create.reserved_name(ASTReserved.FullPerm);
    for(DeclarationStatement d:cl.dynamicFields()){
      map.put(d.name(), create.expression(Old,create.dereference(create.diz(), d.name() + (hist?"_hist_init":"_hist_value"))));
      new_map.put(d.name(), create.dereference(create.diz(), d.name() + (hist?"_hist_value":"_hist_init")));
      if (hist){
        cb.requires(create.expression(Perm,create.dereference(create.diz(), d.name() + "_hist_init"),full));
        cb.requires(hist_perm(diz,d.name(), full));
        cb.ensures(free_perm(diz,d.name(), full));
      } else {
        cb.requires(free_perm(diz,d.name(), full));
        cb.ensures(hist_perm(diz,d.name(), full));
        cb.ensures(create.expression(Perm,create.dereference(create.diz(), d.name() + "_hist_init"),full));
      }
      cb.ensures(create.expression(EQ
          , create.dereference(create.diz(), d.name() + "_hist_value")
          , create.expression(Old,create.dereference(create.diz(), d.name() + "_hist_value"))
      ));
    }
    if (def.getContract() != null) {
      ASTNode temp = rewrite(def.getContract().pre_condition);
      Debug("REQ %s", temp);
      cb.requires(temp);
      cb.ensures(new_sigma.rewrite(rename_old.rewrite(def.getContract().post_condition)));
    }
    
    ArrayList<ASTNode> def_names=new ArrayList<ASTNode>();
    for(DeclarationStatement d:def.getArgs()){
      def_names.add(create.local_name(d.name()));
    }
    
    ASTNode tmp=create.invokation(create.diz(), null, "hist_idle",
        create.reserved_name(ASTReserved.FullPerm),
        create.domain_call("Process","p_"+effect.method(),def_names)
    );
    if (hist){ cb.requires(tmp); } else { cb.ensures(tmp); }

    
    end_hist=create.method_decl(
        create.primitive_type(PrimitiveSort.Void),
        cb.getContract(),
        name,
        rewrite(def.getArgs()),
        null
    );
    hist_class.add_dynamic(end_hist);

    result=create.invokation(rewrite(model), null ,name,rewrite(effect.getArgs()));
  }
  
  @Override
  public void visit(FieldAccess a){
    if (a.object() != null && a.object().getType() != null && isProcessRef(a.object())) {
      if (a.value() == null) {
        result=create.dereference(rewrite(a.object()), a.name() + "_hist_value");
      } else {
        result=create.invokation(rewrite(a.object()), null, "hist_set_" + a.name(), rewrite(a.value()));
      }
    } else {
      result=create.dereference(rewrite(a.object()), a.name());
      if (a.value() != null){
        result=create.assignment(result, rewrite(a.value()));
      }
    }
  }

  /**
   * This method determines the classes that have to be rewritten first.
   * @param d
   * @return
   */
  private boolean high_prio(ASTDeclaration d){
    return (d instanceof AxiomaticDataType) || d.name().equals("History") || d.name().equals("Future");
  }
  
  @Override
  public ProgramUnit rewriteAll(){
    for(ASTDeclaration d:source().get()){
      if(high_prio(d)){
        ASTDeclaration tmp=rewrite(d);
        if(tmp!=null) target().add(tmp);
      }
    }
    for(ASTDeclaration d:source().get()){
      if(!high_prio(d)){
        ASTDeclaration tmp=rewrite(d);
        if(tmp!=null) target().add(tmp);
      }
    }
    if(adt != null) {
      target().add(adt);
    }
    target().index_classes();
    return target();
  }

}
