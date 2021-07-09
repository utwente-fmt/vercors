package vct.col.rewrite;

import java.io.PrintWriter;
import java.util.*;

import hre.config.Configuration;
import vct.col.ast.stmt.decl.ASTClass;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.type.ASTReserved;
import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.stmt.decl.ASTSpecial.Kind;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.util.*;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.decl.Contract;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.stmt.composite.LoopStatement;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.expr.NameExpression;
import vct.col.ast.stmt.composite.ParallelBarrier;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.type.Type;
import vct.col.ast.util.ASTUtils;
import vct.col.util.ControlFlowAnalyzer;

public class KernelRewriter extends AbstractRewriter {
  
  /**
   * Holds the invariant for the group?
   */
  ArrayList<ASTNode> group_inv;
  
  /** Holds the number of the current barrier during
   *  the rewrite of a kernel.
   *
   */
  private int barrier_no;
  
  /**
   * Used for for building the contract of the barrier function
   * called form the thread verification function. 
   */
  private ContractBuilder barrier_cb;
  
  /**
   * Mapping from barrier statements to their numbers.
   */
  private HashMap<ParallelBarrier,Integer> barrier_map;
  
  /**
   * Map every barrier number to the list of post-resources of the barrier.
   * The program starts at the implicit barrier number 0, which is the
   * entry point of the thread.
   */
  private Hashtable<Integer,ArrayList<ASTNode>> resources;
  
  /**
   * Map every barrier to the functional pre-conditions. 
   */
  private Hashtable<Integer,ArrayList<ASTNode>> barrier_pre;
  /**
   * Map every barrier to the functional post-conditions. 
   */
  private Hashtable<Integer,ArrayList<ASTNode>> barrier_post;

  
  public KernelRewriter(ProgramUnit source) {
    super(source);
  }

  /**
   * Put all preceeding barriers of node into barriers.
   * @param barriers Set to
   * @param node The node for which to find the set of preceeding barriers.
   * 
   */
  private void find_predecessors(Set<Integer> barriers,Set<ASTNode> visited, ASTNode node) {
    if (visited.contains(node)) return;
    visited.add(node);
    for(ASTNode pred : node.getPredecessors()){
      if (pred instanceof Method){
        barriers.add(0);
      } else if (pred instanceof ParallelBarrier) {
        barriers.add(barrier_map.get(pred));
      } else {
        find_predecessors(barriers,visited, pred);
      }
    }
  }
  
  private void find_predecessors(Set<Integer> barriers, ASTNode node) {
    find_predecessors(barriers,new HashSet<ASTNode>(),node);
  }
  private ASTNode create_barrier_call(int no) {
    ArrayList<ASTNode> args=new ArrayList<ASTNode>();
    args.add(create.local_name("tcount"));
    args.add(create.local_name("gsize"));
    args.add(create.local_name("tid"));
    args.add(create.local_name("gid"));
    args.add(create.local_name("lid"));
    args.add(create.constant(no));
    args.add(create.local_name("__last_barrier"));
    for (ASTNode a:private_args){
      args.add(copy_rw.rewrite(a));
    }
    return create.assignment(create.local_name("__last_barrier"),
        create.invokation(null,null,base_name+"_barrier",args.toArray(new ASTNode[0])));
  }
  
  private void add(Hashtable<Integer,ArrayList<ASTNode>>map,int no,ASTNode res){
    ArrayList<ASTNode> list=map.get(no);
    if (list==null) {
      list=new ArrayList<ASTNode>();
      map.put(no,list);
    }
    list.add(res);
  }
  
  private ASTNode create_resources(NameExpression name) {
    ASTNode res=create.constant(true);
    for (Integer i:resources.keySet()){
      ASTNode tmp=create.expression(StandardOperator.Implies,
          create.expression(StandardOperator.EQ,name,create.constant(i.intValue())),
          create.fold(StandardOperator.Star,resources.get(i))
      );
      res=create.expression(StandardOperator.Star,res,tmp);
    }
    return res;
  }

  public void visit(ParallelBarrier pb){
    Integer no=barrier_map.get(pb);
    if (Configuration.auto_barrier.get()){
      currentBlock.add(create_barrier_call(no.intValue()));
      //Disabled these hints because old in barrier refers to before barrier... 
    } else {
      Stack<ASTNode> resource_stack=new Stack<ASTNode>();
      // check and keep resources.
      for (ASTNode clause : ASTUtils.conjuncts(pb.contract().pre_condition, StandardOperator.Star)) {
        if (clause.getType().isPrimitive(PrimitiveSort.Resource)){
          resource_stack.push(clause);
        } else {
          currentBlock.add(create.special(Kind.Assert,rewrite(clause)));
        }
      }
      // exhale exported resources
      while(!resource_stack.empty()){
        ASTNode clause=resource_stack.pop();
        currentBlock.add(create.special(ASTSpecial.Kind.Exhale,rewrite(clause)));
      }
      // inhale imported resources
      for (ASTNode clause : ASTUtils.conjuncts(pb.contract().post_condition, StandardOperator.Star)) {
        currentBlock.add(create.special(ASTSpecial.Kind.Inhale,rewrite(clause)));
      }
    }
    result=null;
  }
  
  private class BarrierScan extends RecursiveVisitor<Object> {

    public BarrierScan(ProgramUnit source) {
      super(source);
    }
    
    public void visit(ParallelBarrier pb){
      barrier_no++;
      barrier_map.put(pb,barrier_no);
      for (ASTNode clause : ASTUtils.conjuncts(pb.contract().pre_condition,StandardOperator.Star)) {
        add(barrier_pre,barrier_no,clause);
        barrier_cb.requires(create(clause).expression(StandardOperator.Implies,
            create.expression(StandardOperator.EQ,create.local_name("this_barrier"),create.constant(barrier_no)),
            rewrite(clause)
        ));
      }
      for (ASTNode clause : ASTUtils.conjuncts(pb.contract().post_condition,StandardOperator.Star)) {
        if (clause.getType().isPrimitive(PrimitiveSort.Resource)){
          add(resources,barrier_no,clause);
        } else {
          add(barrier_post,barrier_no,clause);
          barrier_cb.ensures(create(clause).expression(StandardOperator.Implies,
              create.expression(StandardOperator.EQ,create.local_name("this_barrier"),create.constant(barrier_no)),
              rewrite(clause)
          ));
        }
      }
    }
  }

  private BarrierScan barrier_scan=new BarrierScan(source());
  
  private ASTVisitor<?> cfa=new ControlFlowAnalyzer(source());
  
  private String base_name;

  private ArrayList<DeclarationStatement> private_decls;

  private ArrayList<ASTNode> private_args;

  private ArrayList<ASTNode> kernel_main_invariant;
  @Override
  public void visit(ASTClass cl){
    if (cl.kind==ASTClass.ClassKind.Kernel){
      cl.accept(cfa);
      resources=new Hashtable<Integer,ArrayList<ASTNode>>();
      barrier_pre=new Hashtable<Integer,ArrayList<ASTNode>>();
      barrier_post=new Hashtable<Integer,ArrayList<ASTNode>>();
      HashSet<String> locals=new HashSet<String>();
      HashSet<String> globals=new HashSet<String>();
      kernel_main_invariant = new ArrayList<ASTNode>();
      ArrayList<ASTNode> base_inv=new ArrayList<ASTNode>();
      ArrayList<ASTNode> constraint=new ArrayList<ASTNode>();
      kernel_main_invariant.add(create.expression(StandardOperator.LTE,create.constant(0),create.local_name("tid")));
      kernel_main_invariant.add(create.expression(StandardOperator.LT,create.local_name("tid"),create.local_name("tcount")));
      if (Configuration.assume_single_group.get()){
        kernel_main_invariant.add(create.expression(StandardOperator.EQ,create.local_name("tid"),create.local_name("lid")));
        kernel_main_invariant.add(create.expression(StandardOperator.EQ,create.local_name("tcount"),create.local_name("gsize")));
        kernel_main_invariant.add(create.expression(StandardOperator.EQ,create.local_name("gid"),create.constant(0)));
        base_inv.add(create.expression(StandardOperator.EQ,create.local_name("tcount"),create.local_name("gsize")));
        base_inv.add(create.expression(StandardOperator.EQ,create.local_name("gid"),create.constant(0)));        
      } else {
        kernel_main_invariant.add(create.expression(StandardOperator.LTE,create.constant(0),create.local_name("lid")));
        kernel_main_invariant.add(create.expression(StandardOperator.LT,create.local_name("lid"),create.local_name("gsize")));
        kernel_main_invariant.add(create.expression(StandardOperator.LTE,create.constant(0),create.local_name("gid")));
        kernel_main_invariant.add(create.expression(StandardOperator.EQ,create.local_name("tid"),
            create.expression(StandardOperator.Plus,create.local_name("lid"),
                create.expression(StandardOperator.Mult,create.local_name("gid"),create.local_name("gsize"))
            )
        ));
        base_inv.add(create.expression(StandardOperator.LTE,create.constant(0),create.local_name("tcount")));
        base_inv.add(create.expression(StandardOperator.LTE,create.constant(0),create.local_name("gsize")));
        base_inv.add(create.expression(StandardOperator.GT,create.local_name("tcount"),
            create.expression(StandardOperator.Mult,create.local_name("gid"),create.local_name("gsize"))));
      }
      ASTClass res=create.ast_class(cl.getName(), ASTClass.ClassKind.Plain,null, null, null);
      for (DeclarationStatement global:cl.staticFields()){
        globals.add(global.name());
        kernel_main_invariant.add(create.expression(StandardOperator.Value,create.field_name(global.name())));
        base_inv.add(create.expression(StandardOperator.Value,create.field_name(global.name())));
        Type t=global.getType();
        if (t.isPrimitive(PrimitiveSort.Array)){
          ASTNode tmp=create.expression(StandardOperator.EQ,
              create.array_length_dereference(create.field_name(global.name())),
              rewrite(t.secondarg())
          );
          kernel_main_invariant.add(tmp);
          base_inv.add(tmp);
          t=create.primitive_type(PrimitiveSort.Array,rewrite(t.firstarg()));          
        } else {
          t=rewrite(t);
        }
        constraint.add(create.expression(StandardOperator.EQ,create.field_name(global.name()),
            create.expression(StandardOperator.Old,create.field_name(global.name()))));
        res.add_dynamic(create.field_decl(global.name(), t));
      }
      for (DeclarationStatement local:cl.dynamicFields()){
        locals.add(local.name());
        kernel_main_invariant.add(create.expression(StandardOperator.Value,create.field_name(local.name())));
        base_inv.add(create.expression(StandardOperator.Value,create.field_name(local.name())));
        Type t=local.getType();
        if (t.isPrimitive(PrimitiveSort.Array)){
          ASTNode tmp=create.expression(StandardOperator.EQ,
              create.array_length_dereference(create.field_name(local.name())),
              rewrite(t.secondarg())
          );
          kernel_main_invariant.add(tmp);
          base_inv.add(tmp);
          t=create.primitive_type(PrimitiveSort.Array,rewrite(t.firstarg()));          
        } else {
          t=rewrite(t);
        }
        constraint.add(create.expression(StandardOperator.EQ,create.field_name(local.name()),
            create.expression(StandardOperator.Old,create.field_name(local.name()))));
        res.add_dynamic(create.field_decl(local.name(), t));
      }
      for (Method m:cl.dynamicMethods()){
        if (m.getKind()==Method.Kind.Predicate){
          if (m.getName().equals("kernel_invariant")){
            base_inv.add(copy_rw.rewrite(m.getBody()));
            kernel_main_invariant.add(copy_rw.rewrite(m.getBody()));
          }
        }
      }
      for (Method m:cl.dynamicMethods()){
        if (m.getKind()==Method.Kind.Predicate){
          if (!m.getName().equals("kernel_invariant")){
            res.add_dynamic(copy_rw.rewrite(m));
          }
          continue;          
        }
        if (m.getKind()==Method.Kind.Pure){
          res.add_dynamic(copy_rw.rewrite(m));
          continue;
        }
        group_inv=new ArrayList<ASTNode>(base_inv);
        if (m.getArity()!=0) Fail("TODO: kernel argument support");
        Type returns=create(m).primitive_type(PrimitiveSort.Void);
        Contract contract=m.getContract();
        Objects.requireNonNull(contract, "kernel without contract");
        base_name=m.getName();
        barrier_no=0;
        barrier_map=new HashMap<ParallelBarrier, Integer>();
        barrier_cb=new ContractBuilder();
        barrier_scan=new BarrierScan(source());
        m.accept(barrier_scan);
        ContractBuilder thread_cb=new ContractBuilder();
        thread_cb.requires(create.fold(StandardOperator.Star, kernel_main_invariant));
        thread_cb.ensures(create.fold(StandardOperator.Star, kernel_main_invariant));
        thread_cb.ensures(create.fold(StandardOperator.Star, constraint));
        for(ASTNode clause:ASTUtils.conjuncts(contract.pre_condition,StandardOperator.Star)){
          thread_cb.requires(clause);
          if(clause.getType().isPrimitive(PrimitiveSort.Resource)){
            add(resources,0,clause);
          } else if (!ASTUtils.find_name(clause,"tid")
                  && !ASTUtils.find_name(clause,"lid")) {
            group_inv.add(clause);
          }
        }
        for(ASTNode clause:ASTUtils.conjuncts(contract.post_condition,StandardOperator.Star)){
          thread_cb.ensures(clause);
        }        
        DeclarationStatement args[]=new DeclarationStatement[]{
            create.field_decl("tcount", create.primitive_type(PrimitiveSort.Integer)),
            create.field_decl("gsize", create.primitive_type(PrimitiveSort.Integer)),
            create.field_decl("tid", create.primitive_type(PrimitiveSort.Integer)),
            create.field_decl("gid", create.primitive_type(PrimitiveSort.Integer)),
            create.field_decl("lid", create.primitive_type(PrimitiveSort.Integer))            
        };
        private_decls = new ArrayList<DeclarationStatement>();
        private_args = new ArrayList<ASTNode>();
        BlockStatement orig_block=(BlockStatement)m.getBody();
        BlockStatement block=create.block();
        int K=orig_block.getLength();
        block.addStatement(create.field_decl("__last_barrier",create.primitive_type(PrimitiveSort.Integer),create.constant(0)));
        for(int i=0;i<K;i++){
          ASTNode s=orig_block.getStatement(i);
          if (s instanceof DeclarationStatement){
            DeclarationStatement d=(DeclarationStatement)s;
            private_decls.add(d);
            private_args.add(create.local_name(d.name()));
          }
        }
        BlockStatement save_block=currentBlock;
        currentBlock=block;
        for(int i=0;i<K;i++){
          ASTNode s=orig_block.getStatement(i);
          ASTNode r=rewrite(s);
          if (r!=null) block.addStatement(r);
        }
        currentBlock=save_block;
        Contract tc=thread_cb.getContract();
        res.add_dynamic(create(m).method_decl(returns,tc,base_name+"_main", args, block));
        
        /* dumping kernel contract */

        barrier_cb.requires(create_resources(create.local_name("last_barrier")),false);
        barrier_cb.requires(create.fold(StandardOperator.Star, kernel_main_invariant),false);
        for(ParallelBarrier barrier : barrier_map.keySet()){
          Set<Integer> preds=new HashSet<Integer>();
          ASTNode tmp=create.constant(false);
          find_predecessors(preds,barrier);
          for(Integer i : preds){
            tmp=create.expression(StandardOperator.Or,tmp,
                  create.expression(StandardOperator.EQ,
                      create.local_name("last_barrier"),
                      create.constant(i.intValue())
                  )
                );
          }
          barrier_cb.requires(create.expression(StandardOperator.Implies,
              create.expression(StandardOperator.EQ,
                  create.local_name("this_barrier"),
                  create.constant(barrier_map.get(barrier).intValue())
              ),
              tmp
          ),false);
        }
        barrier_cb.ensures(create.expression(StandardOperator.EQ,
            create.reserved_name(ASTReserved.Result),create.local_name("this_barrier")),false);
        barrier_cb.ensures(create_resources(create.reserved_name(ASTReserved.Result)),false);
        barrier_cb.ensures(create.fold(StandardOperator.Star, constraint),false);
        barrier_cb.ensures(create.fold(StandardOperator.Star, kernel_main_invariant),false);
        ArrayList<DeclarationStatement> barrier_decls=new ArrayList<DeclarationStatement>();
        barrier_decls.add(create.field_decl("tcount", create.primitive_type(PrimitiveSort.Integer)));
        barrier_decls.add(create.field_decl("gsize", create.primitive_type(PrimitiveSort.Integer)));
        barrier_decls.add(create.field_decl("tid", create.primitive_type(PrimitiveSort.Integer)));
        barrier_decls.add(create.field_decl("gid", create.primitive_type(PrimitiveSort.Integer)));
        barrier_decls.add(create.field_decl("lid", create.primitive_type(PrimitiveSort.Integer)));
        barrier_decls.add(create.field_decl("this_barrier", create.primitive_type(PrimitiveSort.Integer)));
        barrier_decls.add(create.field_decl("last_barrier", create.primitive_type(PrimitiveSort.Integer)));
        for(DeclarationStatement d:private_decls){
          barrier_decls.add(copy_rw.rewrite(d));
        }
        if (Configuration.auto_barrier.get()){
          // auto barrier uses this method, while manual barrier uses in/ex-hale.
          res.add_dynamic(create.method_decl(
              create.primitive_type(PrimitiveSort.Integer),
              barrier_cb.getContract(),
              base_name+"_barrier",
              barrier_decls.toArray(args),
              null
          ));
        }
        for (int i=1;i<=barrier_no;i++){
          ContractBuilder resource_cb;
          
          ASTNode min;
          if (Configuration.assume_single_group.get()){
            min=create.local_name("gsize");
          } else {
            min=create.expression(StandardOperator.Mult,create.local_name("gsize"),
                create.expression(StandardOperator.Plus,create.local_name("gid"),create.constant(1)));
            min=create.expression(StandardOperator.ITE,
                create.expression(StandardOperator.LT,create.local_name("tcount"),min),
                create.local_name("tcount"),min
            );
          }
          ASTNode guard;
          if (Configuration.assume_single_group.get()){
            guard=create.expression(StandardOperator.Member,create.local_name("tid"),
                create.expression(StandardOperator.RangeSeq,create.constant(0),min));
          }
          else {
            guard=create.expression(StandardOperator.Member,create.local_name("tid"),
               create.expression(StandardOperator.RangeSeq,
                 create.expression(StandardOperator.Mult,create.local_name("gid"),create.local_name("gsize")),
                 min
            ));
          }
          ASTNode local_guard=create.expression(StandardOperator.Member,create.local_name("lid"),
              create.expression(StandardOperator.RangeSeq,create.constant(0),create.local_name("gsize")));
          resource_cb=new ContractBuilder();
          resource_cb.requires(create.fold(StandardOperator.Star, group_inv));
          if (Configuration.auto_barrier.get()){        
            for(ASTNode claim:resources.get(0)){
              if (ASTUtils.find_name(claim,"lid")){
                resource_cb.requires(create.starall(
                    copy_rw.rewrite(local_guard),
                    copy_rw.rewrite(claim),
                    create.field_decl("lid", create.primitive_type(PrimitiveSort.Integer))
                ));
              } else {
                resource_cb.requires(create.starall(
                    copy_rw.rewrite(guard),
                    copy_rw.rewrite(claim),
                    create.field_decl("tid", create.primitive_type(PrimitiveSort.Integer))
                ));
              }
            }
          } else {
            for(ASTNode claim:barrier_pre.get(i)){
              if(claim.getType().isBoolean()){
                if (ASTUtils.find_name(claim,"lid")){
                  resource_cb.requires(create.forall(
                      copy_rw.rewrite(local_guard),
                      copy_rw.rewrite(claim),
                      create.field_decl("lid", create.primitive_type(PrimitiveSort.Integer))
                  ));
                } else if (ASTUtils.find_name(claim,"tid")){
                  resource_cb.requires(create.forall(
                      copy_rw.rewrite(guard),
                      copy_rw.rewrite(claim),
                      create.field_decl("tid", create.primitive_type(PrimitiveSort.Integer))
                  ));
                } else {
                  resource_cb.requires(copy_rw.rewrite(claim));
                }
              } else {
                if (ASTUtils.find_name(claim,"lid")){
                  resource_cb.requires(create.starall(
                      copy_rw.rewrite(local_guard),
                      copy_rw.rewrite(claim),
                      create.field_decl("lid", create.primitive_type(PrimitiveSort.Integer))
                  ));
                } else {
                  resource_cb.requires(create.starall(
                      copy_rw.rewrite(guard),
                      copy_rw.rewrite(claim),
                      create.field_decl("tid", create.primitive_type(PrimitiveSort.Integer))
                  ));
                }                
              }
            }
          }
          resource_cb.ensures(create.fold(StandardOperator.Star, group_inv));
          resource_cb.ensures(create.fold(StandardOperator.Star, constraint));
          if (resources.get(i)!=null) for(ASTNode claim:resources.get(i)){
            resource_cb.ensures(create.starall(
                copy_rw.rewrite(guard),
                copy_rw.rewrite(claim),
                create.field_decl("tid", create.primitive_type(PrimitiveSort.Integer))
            ));
          }
          ArrayList<DeclarationStatement> resource_decls=new ArrayList<DeclarationStatement>();
          resource_decls.add(create.field_decl("tcount", create.primitive_type(PrimitiveSort.Integer)));
          resource_decls.add(create.field_decl("gsize", create.primitive_type(PrimitiveSort.Integer)));
          resource_decls.add(create.field_decl("gid", create.primitive_type(PrimitiveSort.Integer)));
          for(DeclarationStatement d:private_decls){
            resource_decls.add(copy_rw.rewrite(d));
          }
          if (Configuration.enable_resource_check.get()){
            res.add_dynamic(create.method_decl(
              create.primitive_type(PrimitiveSort.Void),
              resource_cb.getContract(),
              base_name+"_resources_of_"+i,
              resource_decls.toArray(new DeclarationStatement[0]),
              create.block()
            ));
          }
        }
        
        for (int i=1;i<=barrier_no;i++){
          ContractBuilder cb=new ContractBuilder();
          ArrayList<DeclarationStatement> post_check_decls=new ArrayList<DeclarationStatement>();
          post_check_decls.add(create.field_decl("tcount", create.primitive_type(PrimitiveSort.Integer)));
          post_check_decls.add(create.field_decl("gsize", create.primitive_type(PrimitiveSort.Integer)));
          post_check_decls.add(create.field_decl("tid", create.primitive_type(PrimitiveSort.Integer)));
          post_check_decls.add(create.field_decl("gid", create.primitive_type(PrimitiveSort.Integer)));
          post_check_decls.add(create.field_decl("lid", create.primitive_type(PrimitiveSort.Integer)));
          for(DeclarationStatement d:private_decls){
            post_check_decls.add(copy_rw.rewrite(d));
          }

          Hashtable<NameExpression,ASTNode> map=new Hashtable<NameExpression, ASTNode>();
          map.put(create.local_name("tid"),create.local_name("_x_tid"));
          map.put(create.local_name("lid"),create.local_name("_x_lid"));
          Substitution sigma=new Substitution(source(),map);

          ASTNode min;
          ASTNode low;
          if (Configuration.assume_single_group.get()){
            low=create.constant(0);
            min=create.local_name("gsize");
          } else {
            low=create.expression(StandardOperator.Mult,create.local_name("gid"),create.local_name("gsize"));
              min=create.expression(StandardOperator.Mult,create.local_name("gsize"),
                  create.expression(StandardOperator.Plus,create.local_name("gid"),create.constant(1)));
              min=create.expression(StandardOperator.ITE,
                  create.expression(StandardOperator.LT,create.local_name("tcount"),min),
                  create.local_name("tcount"),min
              );
          }
          
          ASTNode guard=create.expression(StandardOperator.And,
              create.expression(StandardOperator.LTE,low,create.local_name("_x_tid")),
              create.expression(StandardOperator.LT,create.local_name("_x_tid"),min)
          );
          
          ASTNode local_guard=create.expression(StandardOperator.And,
              create.expression(StandardOperator.LTE,create.constant(0),create.local_name("_x_lid")),
              create.expression(StandardOperator.LT,create.local_name("_x_lid"),create.local_name("gsize"))
          );
          
          cb.requires(create.fold(StandardOperator.Star, kernel_main_invariant));
          for(ASTNode claim:resources.get(0)){
            if (ASTUtils.find_name(claim,"lid")){
              cb.requires(create.starall(
                  copy_rw.rewrite(local_guard),
                  sigma.rewrite(claim),
                  create.field_decl("_x_lid", create.primitive_type(PrimitiveSort.Integer))
              ));
            } else {
              cb.requires(create.starall(
                  copy_rw.rewrite(guard),
                  sigma.rewrite(claim),
                  create.field_decl("_x_tid", create.primitive_type(PrimitiveSort.Integer))
              ));
            }
          }
          if (barrier_pre.get(i)!=null) for(ASTNode claim:barrier_pre.get(i)){
              if (!claim.getType().isBoolean()){
                if (Configuration.auto_barrier.get()){
                  Fail("%s: resource in requires clause while using auto barriers.",claim.getOrigin());
                }
                PrintWriter outputWriter = hre.lang.System.getLogLevelOutputWriter(hre.lang.System.LogLevel.Info);
                outputWriter.print("skipping:");
                vct.col.ast.util.Configuration.getDiagSyntax().print(outputWriter, claim);
                outputWriter.print("%n");
                outputWriter.close();

                continue;
              }
              if (ASTUtils.find_name(claim,"lid")){
                cb.requires(create.forall(
                    copy_rw.rewrite(local_guard),
                    sigma.rewrite(claim),
                    create.field_decl("_x_lid", create.primitive_type(PrimitiveSort.Integer))
                ));
              } else if (ASTUtils.find_name(claim,"tid")){
                cb.requires(create.forall(
                    copy_rw.rewrite(guard),
                    sigma.rewrite(claim),
                    create.field_decl("_x_tid", create.primitive_type(PrimitiveSort.Integer))
                ));
              } else {
            	cb.requires(copy_rw.rewrite(claim));
              }
            }
          cb.ensures(create.fold(StandardOperator.Star,kernel_main_invariant));
          cb.ensures(create.fold(StandardOperator.Star,constraint));
          if (resources.get(i)!=null) cb.ensures(create.fold(StandardOperator.Star, resources.get(i)));
          if (barrier_post.get(i)!=null) cb.ensures(create.fold(StandardOperator.Star, barrier_post.get(i)));
          if(Configuration.enable_post_check.get()){
            res.add_dynamic(create.method_decl(
              create.primitive_type(PrimitiveSort.Void),
              cb.getContract(),
              base_name+"_post_check_"+i,
              post_check_decls.toArray(new DeclarationStatement[0]),
              create.block()
            ));
          }
        }
      }
      for(ASTNode n:cl.staticMethods()){
        res.add_static(rewrite(n));
      }
      result=res;
    } else {
      result=copy_rw.rewrite(cl);
    }
  }

  @Override
  public void visit(LoopStatement s) {
    LoopStatement res=new LoopStatement();
    ASTNode tmp;
    tmp=s.getInitBlock();
    if (tmp!=null) res.setInitBlock(tmp.apply(this));
    tmp=s.getUpdateBlock();
    if (tmp!=null) res.setUpdateBlock(tmp.apply(this));
    tmp=s.getEntryGuard();
    if (tmp!=null) res.setEntryGuard(tmp.apply(this));
    tmp=s.getExitGuard();
    if (tmp!=null) res.setExitGuard(tmp.apply(this));
    for(ASTNode inv:kernel_main_invariant){
      res.appendInvariant(copy_rw.rewrite(inv));
    }
    if (Configuration.auto_barrier.get()){
      ASTNode inv1=create.constant(false);
      Set<Integer> preds=new HashSet<Integer>();
      find_predecessors(preds,s.getBody());
      for(Integer i : preds){
        inv1=create.expression(StandardOperator.Or,inv1,
              create.expression(StandardOperator.EQ,
                  create.local_name("__last_barrier"),
                  create.constant(i.intValue())
              )
            );
      }
      res.appendInvariant(inv1);
      res.appendInvariant(create_resources(create.local_name("__last_barrier")));
    }
    for(ASTNode inv:s.getInvariants()){
      res.appendInvariant(inv.apply(this));
    }
    tmp=s.getBody();
    res.setBody(tmp.apply(this));
    res.set_before(rewrite(s.get_before()));
    res.set_after(rewrite(s.get_after()));
    res.setOrigin(s.getOrigin());
    result=res;
  }

}
