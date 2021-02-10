// -*- tab-width:2 ; indent-tabs-mode:nil -*-
package vct.col.ast.util;

import scala.collection.JavaConverters;
import scala.collection.JavaConverters.*;
import java.util.*;
import java.util.Map.Entry;

import vct.col.ast.expr.*;
import vct.col.ast.expr.constant.ConstantExpression;
import vct.col.ast.expr.constant.StructValue;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.*;
import vct.col.ast.stmt.decl.*;
import vct.col.ast.stmt.decl.ASTClass.ClassKind;
import vct.col.ast.stmt.decl.ASTSpecial.Kind;
import vct.col.ast.stmt.composite.Switch.Case;
import vct.col.ast.stmt.terminal.AssignmentStatement;
import vct.col.ast.stmt.terminal.ReturnStatement;
import vct.col.ast.type.*;
import hre.util.Triple;
import hre.ast.Origin;
import hre.util.FrameControl;
import hre.util.FrameReference;

import static hre.lang.System.*;
import static vct.col.ast.type.ASTReserved.Null;

/**
 * This class provides a factory for ASTNodes, that can be
 * configured to (semi-)automatically insert origins.
 * 
 * For every AST construction method, the newly created node will have the current
 * top of the origin stack as its origin.
 * 
 * To create an AST node with a specific origin, one can use chaining:
 * <pre>
  create.setOrigin(origin).expression(....)
 * </pre>
 * 
 * @author Stefan Blom
 * @param E The type of object from which this factory can extract origins.
 */
public class ASTFactory<E> implements FrameControl {
  
  /**
   * The stack of origins that this factory uses.
   */
  private final FrameReference<Origin> origin_stack=new FrameReference<Origin>();

  /**
   * Visitor to be called immediately after construction of a new node.
   * This variable may be null;
   */
  private ASTVisitor<?> post=null;
  

  private final AbstractRewriter copy_rw;
  
  /**
   * Create a new AST factory.
   * @param copy_rw 
   */
  public ASTFactory(AbstractRewriter copy_rw){
    this.copy_rw=copy_rw;
  }
  
  /**
   * Create a new AST factory.
   */
  public ASTFactory(){
    this.copy_rw=new AbstractRewriter(null,null);
  }

  /**
   * Create a new abstract class.
   */
  public ASTClass abstract_class(String name, DeclarationStatement parameters[], ClassType super_class, ClassType ... supports) {
    ClassType bases[]={super_class};
    if (super_class==null) bases=null;
    return ast_class(name,ClassKind.Abstract,parameters,bases,supports);
  }
  
  public void addRandomConstructor(ASTClass cl){
    enter();
    setOrigin(cl.getOrigin());
    ContractBuilder cb=new ContractBuilder();
    for(DeclarationStatement field : cl.dynamicFields()){
      cb.requires(expression(
          StandardOperator.Perm,
          field_name(field.name()),
          constant(100)
      ));
      cb.ensures(expression(
          StandardOperator.Perm,
          field_name(field.name()),
          constant(100)
     ));
    }
    Method cons=method_kind(
        Method.Kind.Constructor,
        primitive_type(PrimitiveSort.Void),
        cb.getContract(),
        cl.getName(),
        new DeclarationStatement[0],
        block()
    );
    cl.add_dynamic(cons);
    leave();
  }
 
  public ASTNode fullPermission(){
    return reserved_name(ASTReserved.FullPerm);
  }

  public void addZeroConstructor(ASTClass cl){
    enter();
    setOrigin(cl.getOrigin());
    ContractBuilder cb=new ContractBuilder();
    BlockStatement body=block();
    for(DeclarationStatement field : cl.dynamicFields()){
      ASTNode zero=field.getType().zero();
      if (zero!=null){
        zero.setOrigin(cl.getOrigin());
        cb.ensures(expression(
             StandardOperator.PointsTo,
             field_name(field.name()),
             fullPermission(),
             zero
        ));
        body.addStatement(assignment(field_name(field.name()),zero));
      } else {
        cb.ensures(expression(
            StandardOperator.Perm,
            field_name(field.name()),
            fullPermission()
       ));       
      }
    }
    Method cons=method_kind(
        Method.Kind.Constructor,
        primitive_type(PrimitiveSort.Void),
        cb.getContract(),
        cl.getName(),
        new DeclarationStatement[0],
        body);
    cl.add_dynamic(cons);
    leave();
  }
  
  /**
   * Create a name expression that refers to an argument variable.
   */
  public NameExpression argument_name(String name) {
    NameExpression res=new NameExpression(name);
    res.setKind(NameExpressionKind.Argument);
    res.setOrigin(origin_stack.get());
    res.accept_if(post);
    return res;
  }

  /**
   * Create an assignment statement/expression.
   */
  public ASTNode assignment(ASTNode loc, ASTNode val) {
    AssignmentStatement res=new AssignmentStatement(loc,val);
    res.setOrigin(origin_stack.get());
    res.accept_if(post);
    return res;
  }
  
  /**
   * Create a new class.
   */
  public ASTClass ast_class(String name,ClassKind kind,DeclarationStatement parameters[],ClassType bases[],ClassType supports[]) {
    if (bases==null) bases=new ClassType[0];
    if (supports==null) supports=new ClassType[0];
    if (parameters==null) parameters=new DeclarationStatement[0];
    ASTClass res=new ASTClass(name,kind,parameters,bases,supports);
    res.setOrigin(origin_stack.get());
    res.accept_if(post);
    return res;    
  }
  
  public ParallelBarrier barrier(String label, Contract c, ArrayList<String> fences, BlockStatement body){
     return barrier(origin_stack.get(),label,c,fences,body);
   }
  
  /**
    * Create a new barrier node.
    */
   public ParallelBarrier barrier(Origin origin,String label,Contract c,ArrayList<String> fences, BlockStatement body){
     ParallelBarrier res = new ParallelBarrier(label, c, fences, body);
     res.setOrigin(origin);
     res.accept_if(post);
     return res;
   }

  /**
   * Create a new binding expression.
   */
  public ASTNode binder(Binder b, Type result_type, DeclarationStatement decls[], ASTNode triggers[][], ASTNode selection, ASTNode main) {
    ASTNode res=new BindingExpression(b,result_type,decls,triggers,selection,main);
    res.setOrigin(origin_stack.get());
    res.accept_if(post);
    return res;
  }
  /**
   * Create a new block, with the given statements as (initial) contents.
   */
  public BlockStatement block(ASTNode ... args) {
    return block(origin_stack.get(),args);        
  }

  public BlockStatement block(Origin origin, ASTNode ... args) {
    BlockStatement res=new BlockStatement();
    for(ASTNode node:args){
      res.addStatement(node);
    }
    res.setOrigin(origin);
    res.accept_if(post);
    return res;
  }

  /**
   * Create a new class type node.
   */
  public ClassType class_type(Origin origin,String name[],ASTNode ... args){
    ClassType res=new ClassType(name,args);
    res.setOrigin(origin);
    res.accept_if(post);
    return res;
  }
  
  public ClassType class_type(Origin origin, String name[], List<ASTNode> args){
    ClassType res = new ClassType(name, args);
    res.setOrigin(origin);
    res.accept_if(post);
    return res;
  }

  public ClassType class_type(Origin origin,String name,ASTNode ... args){
    String tmp[]=new String[1];
    tmp[0]=name;
    return class_type(origin, tmp, args);
  }
  
  public ClassType class_type(Origin origin,String name, List<ASTNode> args){
    String tmp[]=new String[1];
    tmp[0]=name;
    return class_type(origin, tmp, args);
  }
  
  public ClassType class_type(String name[],ASTNode ... args){
    return class_type(origin_stack.get(),name,args);
  }
  public ClassType class_type(String name,ASTNode ... args){
    return class_type(origin_stack.get(),name,args);
  }
  public ClassType class_type(String name, List<ASTNode> args){
    return class_type(origin_stack.get(), name, args);
  }

  public ConstantExpression constant(boolean b) {
    return constant(origin_stack.get(),b);
  }
  public ConstantExpression constant(double i) {
    return constant(origin_stack.get(),i);
  }
  public ConstantExpression constant(int i) {
    return constant(origin_stack.get(),i);
  }
  public ConstantExpression constant(long i) {
    return constant(origin_stack.get(),i);
  }
  /**
   * Create a new boolean constant.
   */
  public ConstantExpression constant(Origin origin, boolean b) {
    ConstantExpression res=new ConstantExpression(b,origin);
    res.accept_if(post);
    return res;    
  }
  /**
   * Create a new double constant.
   */
  public ConstantExpression constant(Origin origin, double i) {
    ConstantExpression res=new ConstantExpression(i,origin);
    res.accept_if(post);
    return res;    
  }
  /**
   * Create a new integer constant.
   */
  public ConstantExpression constant(Origin origin, int i) {
    ConstantExpression res=new ConstantExpression(i,origin);
    res.accept_if(post);
    return res;    
  }

  /**
   * Create a new long constant.
   */
  public ConstantExpression constant(Origin origin, long i) {
    ConstantExpression res=new ConstantExpression(i,origin);
    res.accept_if(post);
    return res;    
  }
  /**
   * Create a new string constant.
   */
  public ConstantExpression constant(Origin origin, String s) {
    ConstantExpression res=new ConstantExpression(s,origin);
    res.accept_if(post);
    return res;    
  }
  public ConstantExpression constant(String s) {
    return constant(origin_stack.get(),s);
  }
  
   
  /** Create a dereference expression.
   */
  public Dereference dereference(ASTNode object, String field){
    Dereference res=new Dereference(object,field);
    res.setOrigin(origin_stack.get());
    res.accept_if(post);
    return res;
  }
  
  /**
   * Enter a new stack frame of the origin stack.
   */
  public void enter(){
    origin_stack.enter();
  }
  
  /**
   * Enter a new stack frame of the origin stack.
   */
  public void enter(ASTNode n){
    origin_stack.enter();
    setOrigin(n.getOrigin());
  }
  
  /**
   * Create a new operator expression.
   */
  public OperatorExpression expression(Origin origin,StandardOperator op, ASTNode ... args){
    if (op==null) Abort("null operator at %s",origin);
    if (args==null) Abort("null arguments at %s",origin);
    OperatorExpression res = new OperatorExpression(op, args);
    res.setOrigin(origin);
    res.accept_if(post);
    return res;
  }
  
  public OperatorExpression expression(StandardOperator op, ASTNode ... args){
    return expression(origin_stack.get(),op,args);
  }
  public OperatorExpression expression(StandardOperator op, List<ASTNode> args){
    return expression(origin_stack.get(),op,args.toArray(new ASTNode[args.size()]));
  }
  /**
   * Create a new variable declaration with default value.
   * 
   * Used for members, arguments and local variables.
   * 
   * @param name The name of the variable.
   * @param type The type of the variable.
   * @param init The optional initial value of the variable.
   * @return An AST node containing the variable declaration.
   */
  public DeclarationStatement field_decl(String name, Type type,ASTNode init) {
    return field_decl(origin_stack.get(),name,type,init);
  }
  
  public DeclarationStatement field_decl(String name, Type type) {
    return field_decl(origin_stack.get(),name,type,null);
  }
  
  public DeclarationStatement field_decl(Origin o,String name, Type type){
    return field_decl(o,name,type,null);
  }

  public DeclarationStatement field_decl(Origin o,String name, Type type,ASTNode init) {
    if (type.isNull()){
      Abort("cannot declare variable %s of <<null>> type.",name);
    }
    DeclarationStatement res=new DeclarationStatement(name,type,init);
    res.setOrigin(o);
    res.accept_if(post);
    return res;    
  }
  
  /**
   * Create a name expression that refers to a field name.
   */
  public NameExpression field_name(String name) {
    NameExpression res=new NameExpression(name);
    res.setKind(NameExpressionKind.Field);
    res.setOrigin(origin_stack.get());
    res.accept_if(post);
    return res;
  }

  /**
   * Create a name expression that refers to a field name.
   */
  public GPUOpt gpuoptimization(GPUOptName name, List<ASTNode> args) {
    GPUOpt res=new GPUOpt(name, JavaConverters.asScalaBuffer(args).toList());
    res.setOrigin(origin_stack.get());
    res.accept_if(post);
    return res;
  }


  /**
   * Fold left of a non-empty list. 
   * 
   * @param op Operator to fold with.
   * @param list Non-empty list of terms.
   * @return folded list.
   */
   public ASTNode fold(StandardOperator op, List<ASTNode> list) {
     if (list.size()==0){
       switch(op){
       case And:
       case Star:
         return constant(true);
       case Or:
         return constant(false);
       default:
         Abort("cannot fold empty list, because neutral element of %s is not implemented",op);
       }
     }
     ASTNode res=list.get(0);
     int N=list.size();
     for(int i=1;i<N;i++){
       res=expression(op,res,copy_rw.rewrite(list.get(i)));
     }
     return res;
   }

   public IfStatement fold(List<IfStatement> list) {
     if (list.size() == 0) {
       Abort("cannot fold empty list of ifs");
     }

     IfStatement currentIf = list.get(0);
     for (int i = 1; i < list.size(); i++) {
       IfStatement nextIf = list.get(i);

       // Only the last if can have an else statement
       if (i != list.size() - 1 && nextIf.hasElse()) {
         Abort("Only last if in the list is allowed to have an else");
       }

       currentIf.addClause(IfStatement.elseGuard(), nextIf);
       currentIf = nextIf;
     }

     return list.get(0);
   }
   
   /**
    * Fold left of a non-empty list. 
    * 
    * @param op Operator to fold with.
    * @param list Non-empty list of terms.
    * @return folded list.
    */
    public ASTNode fold(StandardOperator op, ASTNode ... list) {
      if (list.length==0){
        switch(op){
        case And:
        case Star:
          return constant(true);
        default:
          Abort("cannot fold empty list, because neutral element of %s is not implemented",op);
        }
      }
      ASTNode res=list[0];
      int N=list.length;
      for(int i=1;i<N;i++){
        res=expression(op,res,copy_rw.rewrite(list[i]));
      }
      return res;
    }

    public LoopStatement for_loop(ASTNode init, ASTNode test, ASTNode update, ASTNode body, List<ASTNode> invariant){
        LoopStatement res=new LoopStatement();
        res.setEntryGuard(test);
        res.setInitBlock(init);
        res.setUpdateBlock(update);
        res.setBody(body);
        res.setOrigin(origin_stack.get());
        for (ASTNode inv:invariant) res.appendInvariant(inv);
        res.fixate();
        res.accept_if(post);
        return res;    
      }

      public LoopStatement loop(ASTNode init_block,
                                ASTNode entry_guard,
                                ASTNode exit_guard,
                                ASTNode update_block,
                                ASTNode body,
                                Contract contract
      ){
        LoopStatement res=new LoopStatement();

        res.setInitBlock(init_block);
        res.setEntryGuard(entry_guard);
        res.setEntryGuard(exit_guard);
        res.setUpdateBlock(update_block);
        res.setBody(body);
        res.setContract(contract);
        res.setOrigin(origin_stack.get());
        res.accept_if(post);
        return res;
      }
    
    public LoopStatement for_loop(ASTNode init, ASTNode test, ASTNode update, ASTNode body,ASTNode ... invariant){
      return for_loop(init, test, update, body, Arrays.asList(invariant));
    }
          
    public LoopStatement for_loop(ASTNode init, ASTNode test, ASTNode update, ASTNode body, GPUOpt gpuopt, Contract contract){
      LoopStatement res=new LoopStatement();
      res.setEntryGuard(test);
      res.setInitBlock(init);
      res.setUpdateBlock(update);
      res.setBody(body);
      res.setOrigin(origin_stack.get());
      res.setUnroll(gpuopt);
      res.setContract(contract);
      res.accept_if(post);
      return res;    
    }
          
    public BindingExpression exists(ASTNode guard, ASTNode claim, DeclarationStatement ... decl) {
      BindingExpression res=new BindingExpression(
          Binder.Exists,
          primitive_type(PrimitiveSort.Boolean),
          decl,
          null,
          guard,
          claim
      );
      res.setOrigin(origin_stack.get());
      res.accept_if(post);
      return res;
    }
    public BindingExpression summation(ASTNode guard, ASTNode claim, DeclarationStatement ... decl) {
      int i=decl.length-1;
      BindingExpression res=new BindingExpression(
          Binder.Sum,
          null,
          new DeclarationStatement[]{decl[i]},
          null,
          guard,
          claim
      );
      res.setOrigin(origin_stack.get());
      res.accept_if(post);
      while(i>0){
        i--;
        res=new BindingExpression(
            Binder.Sum,
            null,
            new DeclarationStatement[]{decl[i]},
            null,
            constant(true),
            res
        );
        res.setOrigin(origin_stack.get());
        res.accept_if(post);
      }
      return res;
    }
  public BindingExpression let_expr(DeclarationStatement decl,ASTNode in) {
    BindingExpression res=new BindingExpression(
        Binder.Let,
        null,
        new DeclarationStatement[]{decl},
        null,
        null,
        in
    );
    res.setOrigin(origin_stack.get());
    res.accept_if(post);
    return res;
  }

  /**
   * Create a function declaration
   */
  public Method function_decl(Type returns,Contract contract,String name,DeclarationStatement args[],ASTNode body){
    return method_kind(Method.Kind.Pure,returns,contract,name,args,false,body);
  }
  
  /**
   * Get the current origin. 
   */
  public Origin getOrigin() {
    return origin_stack.get();
  }
  /**
   * Create a name expression referring to an arbitrary name.
   */
  public NameExpression identifier(String name){
    NameExpression res=new NameExpression(name);
    res.setKind(NameExpressionKind.Unresolved);
    res.setOrigin(origin_stack.get());
    res.accept_if(post);
    return res;
  }
  
  /**
   * Create an if-then-else statement.
   */
  public IfStatement ifthenelse(ASTNode test,ASTNode ... branches){
    if (branches.length<1 || branches.length>2 ) Abort("illegal number of branches");
    IfStatement res=new IfStatement();
    res.addClause(test,branches[0]);
    if (branches.length==2 && branches[1]!=null) res.addClause(IfStatement.elseGuard(), branches[1]);
    res.setOrigin(origin_stack.get());
    res.accept_if(post);
    return res;    
  }
  
  /**
   * Create a new domain function call.
   */
  public ASTNode domain_call(String domain,String method,ASTNode ... args){
    MethodInvokation res=new MethodInvokation(class_type(domain),null,method,args);
    res.setOrigin(origin_stack.get());
    res.accept_if(post);
    return res;
  }
  
  public ASTNode domain_call(String domain,String method,List<ASTNode> args){
    return domain_call(domain,method,args.toArray(new ASTNode[args.size()]));
  }
  /**
   * Create a new method invokation node.
   */
  public MethodInvokation invokation(ASTNode object,ClassType dispatch,String method,ASTNode ... args){
    MethodInvokation res=new MethodInvokation(object,dispatch,method,args);
    res.setOrigin(origin_stack.get());
    res.accept_if(post);
    return res;
  }

  public MethodInvokation invokation(ASTNode object,ClassType dispatch,String method,List<ASTNode> args){
    return invokation(object,dispatch,method,args.toArray(new ASTNode[args.size()]));
  }

  /**
   * Create a name expression that refers to a label.
   */
  public NameExpression label(String name) {
    NameExpression res=new NameExpression(name);
    res.setKind(NameExpressionKind.Label);
    res.setOrigin(origin_stack.get());
    res.accept_if(post);
    return res;
  }

  /**
   * Leave the current stack frame of the origin stack.
   */
  public void leave(){
    origin_stack.leave();
  }
  public ASTNode lemma(BlockStatement block) {
    ASTNode res=new Lemma(block);
    res.setOrigin(origin_stack.get());
    res.accept_if(post);
    return res;
  }

  /**
   * Create a name expression that refers to a local variable.
   */
  public NameExpression local_name(Origin origin, String name) {
    NameExpression res=new NameExpression(name);
    res.setKind(NameExpressionKind.Local);
    res.setOrigin(origin);
    res.accept_if(post);
    return res;
  }
  
  public NameExpression local_name(String name) {
    return local_name(origin_stack.get(), name);
  }

  /**
   * Create a method declaration
   */
  public Method method_decl(Type returns,Contract contract,String name,DeclarationStatement[] args,ASTNode body){
    return method_kind(Method.Kind.Plain,returns,contract,name,args,false,body);
  }
  public Method method_decl(Type returns, Type[] signals, Contract contract,String name,DeclarationStatement[] args,ASTNode body){
    return method_kind(Method.Kind.Plain,returns,signals,contract,name,args,false,body);
  }
  public Method method_decl(Type returns,Contract contract,String name,List<DeclarationStatement> args,ASTNode body){
    return method_kind(Method.Kind.Plain,returns,contract,name,args.toArray(new DeclarationStatement[args.size()]),false,body);
  }
  
  /**
   * Create a method declaration
   */
  public Method method_kind(Method.Kind kind,Type returns,Contract contract,String name,DeclarationStatement args[],ASTNode body){
    return method_kind(kind,returns,contract,name,args,false,body);
  }
  /**
   * Create a method declaration
   */
  public Method method_kind(Method.Kind kind,Type returns,Contract contract,String name,List<DeclarationStatement> args,boolean varArgs,ASTNode body){    
    return method_kind(kind,returns,contract,name,args.toArray(new DeclarationStatement[args.size()]),varArgs,body);
  }
  public Method method_kind(Method.Kind kind,Type returns, Type[] signals, Contract contract,String name,List<DeclarationStatement> args,boolean varArgs,ASTNode body){
    return method_kind(kind,returns, signals, contract,name,args.toArray(new DeclarationStatement[args.size()]),varArgs,body);
  }
  public Method method_kind(Method.Kind kind,Type returns,Contract contract,String name,DeclarationStatement args[],boolean varArgs,ASTNode body){
    return method_kind(kind, returns, new Type[0], contract, name, args, varArgs, body);
  }
  public Method method_kind(Method.Kind kind,Type returns, Type[] signals, Contract contract,String name,DeclarationStatement[] args,boolean varArgs,ASTNode body){
    Method res=new Method(kind,name,returns,signals,contract,args,varArgs,body);
    res.setOrigin(origin_stack.get());
    res.accept_if(post);
    return res;
  }

  /**
   * Create a name expression referring to a method name.
   */
  public NameExpression method_name(Origin origin,String name){
    NameExpression res=new NameExpression(name);
    res.setKind(NameExpressionKind.Method);
    res.setOrigin(origin);
    res.accept_if(post);
    return res;
  }
  public NameExpression method_name(String name) {
    return method_name(origin_stack.get(),name);
  }

  /**
   * Create a name expression that refers to a specific kind.
   */
  public NameExpression name(NameExpressionKind kind, ASTReserved word, String name) {
    NameExpression res=new NameExpression(kind,word,name);
    res.setOrigin(origin_stack.get());
    res.accept_if(post);
    return res;
  }

  /**
   * Create a new plain class.
   */
  public ASTClass new_class(String name,DeclarationStatement parameters[],ClassType super_class,ClassType ... supports) {
    ClassType bases[]={super_class};
    if (super_class==null) bases=null;
    return ast_class(name,ClassKind.Plain,parameters,bases,supports);
  }

  
  /**
   * Allocate a new record. 
   */
  public OperatorExpression new_record(Type type,ASTNode ... args){
    return expression(StandardOperator.New,type,args);
  }
  
  /**
   * Create an instantiation of a new object and invoke a constructor on it.
   */
  public MethodInvokation new_object(ClassType type,ASTNode ... args){
    //return expression(StandardOperator.Build,type,args);
    return invokation(null,type,Method.JavaConstructor, args);
  }
  
  /**
   * Construct a non_null expression.
   * 
   * @param expr Expression that is supposed to be non-null.
   * @return AST for <code> expr != null </code>
   */
  public ASTNode non_null(ASTNode expr) {
    return expression(StandardOperator.NEQ,expr,reserved_name(Null));
  }
  
  /**
   * Construct a non_null expression for a variable name.
   */
 public ASTNode non_null(String string) {
    return non_null(unresolved_name(string));
  }

 public ParallelAtomic parallel_atomic(BlockStatement block,String ... strings){
    return parallel_atomic(origin_stack.get(),block,strings);
  }

 /**
   * Create a new parallel atomic block.
   */
  public ParallelAtomic parallel_atomic(Origin origin,BlockStatement block,String ... strings){
    ASTNode labels[]=new ASTNode[strings.length];
    for(int i=0;i<strings.length;i++){
      labels[i]=label(strings[i]);
    }
    return csl_atomic(origin,block,labels);
  }
  
  public ParallelAtomic csl_atomic(ASTNode block,ASTNode ... invs){
    return csl_atomic(origin_stack.get(),block,invs);
  }
  public ParallelAtomic csl_atomic(Origin origin,ASTNode block,ASTNode ... invs){
    ParallelAtomic res = new ParallelAtomic(block, invs);
    res.setOrigin(origin);
    res.accept_if(post);
    return res;    
  }
 
  
  public ParallelBlock parallel_block(
      String label,
      Contract c,
      DeclarationStatement iters[],
      BlockStatement block,
      ASTNode deps[]
  ){
    return parallel_block(origin_stack.get(),label,c, iters, block, deps);
  }
  
  public ParallelBlock parallel_block(
	      String label,
	      Contract c,
	      List<DeclarationStatement> iters,
	      BlockStatement block,
	      ASTNode deps[]
	  ){
	    return parallel_block(origin_stack.get(),label,c, iters, block, deps);
	  }

  public ParallelBlock parallel_block(
      String label,
      Contract c,
      DeclarationStatement iters[],
      BlockStatement block
  ){
    return parallel_block(origin_stack.get(),label,c, iters, block, null);
  }

  /**
    * Create a new parallel block.
    */
  public ParallelBlock parallel_block(
    Origin origin,
    String label,
    Contract contract,
    DeclarationStatement iters[],
    BlockStatement block,
    ASTNode deps[]
  ){
	  return parallel_block(origin, label, contract, Arrays.asList(iters), block, deps);
  }
  
  /**
   * Create a new parallel block.
   */
 public ParallelBlock parallel_block(
   Origin origin,
   String label,
   Contract contract,
   List<DeclarationStatement> iters,
   BlockStatement block,
   ASTNode deps[]
 ){
	if (deps == null) {
	  deps = new ASTNode[0]; 
	}
	  
   ParallelBlock res = new ParallelBlock(label,contract, iters, block, deps);
   res.setOrigin(origin);
   res.accept_if(post);
   return res;
 }

  /**
   * Create a predicate declaration.
   */
  public Method predicate(String name, ASTNode body,DeclarationStatement ... args) {
    return method_kind(Method.Kind.Predicate,primitive_type(PrimitiveSort.Resource),null,name,args,false,body);
  } 
  
  public Method predicate(String name, ASTNode body,List<DeclarationStatement> args) {
    return method_kind(Method.Kind.Predicate,primitive_type(PrimitiveSort.Resource),null,name,args,false,body);
  } 

  /**
   * Create a new primitive type.
   */
  public PrimitiveType primitive_type(Origin origin,PrimitiveSort sort,ASTNode ... args){
    PrimitiveType res=new PrimitiveType(sort,args);
    res.setOrigin(origin);
    res.accept_if(post);
    return res;        
  }
  
  public PrimitiveType primitive_type(Origin origin,PrimitiveSort sort, List<ASTNode> args){
    PrimitiveType res=new PrimitiveType(sort,args);
    res.setOrigin(origin);
    res.accept_if(post);
    return res;        
  }

 public PrimitiveType primitive_type(PrimitiveSort sort,ASTNode ... args){
  return primitive_type(origin_stack.get(),sort,args);
}
 
 public PrimitiveType primitive_type(PrimitiveSort sort, List<ASTNode> args){
  return primitive_type(origin_stack.get(),sort,args);
}

  /**
   * Create a new reserved name expression.
   * 
   * Reserved names are (for now) all reserved keywords:
   * this, super, null, \result, etc. To allow for future refactoring,
   * this method returns ASTNode on purpose. E.g. null might
   * yield a constant expression instead of a name expression.
   */
  public NameExpression reserved_name(ASTReserved name){
    NameExpression res=new NameExpression(name);
    res.setOrigin(origin_stack.get());
    res.accept_if(post);
    return res;
  }
  
  /**
   * Create a new reserved name expression with a fixed type.
   *
   * Added to experiment with kernels, may not become permanent.
   */
  public NameExpression reserved_name(ASTReserved name,Type t){
    NameExpression res=new NameExpression(name);
    res.setOrigin(origin_stack.get());
    res.accept_if(post);
    res.setType(t);
    return res;
  }
 
 
 /**
 * Create a new return statement.
 * @param value At most one node which is the returned value.
 */
public ReturnStatement return_statement(ASTNode ... value){
  if (value.length>1) Abort("illegal number of return values");
  ReturnStatement res;
  if (value.length==0){
    res=new ReturnStatement();
  } else {
    res=new ReturnStatement(value[0]);
  }
  res.setOrigin(origin_stack.get());
  res.accept_if(post);
  return res;    
}
 /**
 * Replace the current origin.
 * 
 * This method returns the AST to allow chaining.
 * 
 * @param origin The new origin.
 * @return The AST factory.
 */
public ASTFactory<E> setOrigin(Origin origin) {
  this.origin_stack.set(origin);
  return this;
}
public ASTSpecial special(Origin origin, ASTSpecial.Kind kind, ASTNode ... args) {
  ASTSpecial res=new ASTSpecial(kind,args);
  res.setOrigin(origin);
  res.accept_if(post);
  return res;
}
 public ASTSpecial special(ASTSpecial.Kind kind, ASTNode ... args) {
  return special(origin_stack.get(),kind,args);
}
 public ASTSpecial special_decl(Origin origin, ASTSpecial.Kind kind, ASTNode ... args) {
   ASTSpecial res=new ASTSpecial(kind,args);
   res.setOrigin(origin);
   res.accept_if(post);
   return res;
 }
  public ASTSpecial special_decl(ASTSpecial.Kind kind, ASTNode ... args) {
   return special_decl(origin_stack.get(),kind,args);
 }

  public ASTNode starall(ASTNode[][] triggers, ASTNode guard, ASTNode claim, DeclarationStatement ... decl) {
    if (decl.length==0){
      return expression(StandardOperator.Implies,guard,claim);
    }
    int i=decl.length-1;
    BindingExpression res=new BindingExpression(
        Binder.Star,
        primitive_type(PrimitiveSort.Resource),
        new DeclarationStatement[]{decl[i]},
        triggers,
        guard,
        claim
    );
    res.setOrigin(origin_stack.get());
    res.accept_if(post);
    while(i>0){
      i--;
      res=new BindingExpression(
          Binder.Star,
          primitive_type(PrimitiveSort.Resource),
          new DeclarationStatement[]{decl[i]},
          null,
          constant(true),
          res
      );
      res.setOrigin(origin_stack.get());
      res.accept_if(post);
    }
    return res;
  }
  
  public ASTNode starall(ASTNode guard, ASTNode claim, DeclarationStatement ... decl) {
    return starall(null, guard, claim, decl);
  }
  
  public ASTNode forall(ASTNode guard, ASTNode claim, DeclarationStatement ... decl) {
    // Separate quantifiers are needed for the simplifier! 
    if (decl.length==0){
      return expression(StandardOperator.Implies,guard,claim);
    }
    int i=decl.length-1;
    BindingExpression res=new BindingExpression(
        Binder.Forall,
        primitive_type(PrimitiveSort.Boolean),
        new DeclarationStatement[]{decl[i]},
        new ASTNode[0][],
        guard,
        claim
    );
    res.setOrigin(origin_stack.get());
    res.accept_if(post);
    while(i>0){
      i--;
      res=new BindingExpression(
          Binder.Forall,
          primitive_type(PrimitiveSort.Boolean),
          new DeclarationStatement[]{decl[i]},
          new ASTNode[0][],
          constant(true),
          res
      );
      res.setOrigin(origin_stack.get());
      res.accept_if(post);
    }
    return res;
  }

  /**
   *
   * @param resultType The type of the element in the resulting set.
   * @param selector The condition to include an element in the set.
   * @param main The expression that has to be part of the resulting set.
   * @param varBounds The conditions specifiying the bounds of the declared variables.
   * @param decl The declared variables.
   * @return
   */
  public ASTNode setComp(Type resultType, ASTNode selector, ASTNode main, Map<NameExpression, ASTNode> varBounds, DeclarationStatement[] decl) {
    if (decl.length == 0) {
      Fail("For set comprehension, at least one declaration is needed.");
    }
    SetComprehension res = new SetComprehension(
            resultType,
            decl,
            selector,
            main,
            varBounds
    );
    res.setOrigin(origin_stack.get());
    res.accept_if(post);
    return res;
  }

  public ASTNode forall(ASTNode triggers[][],ASTNode guard, ASTNode claim, DeclarationStatement ... decl) {
    // Single quantifier is needed for correct triggering of axioms. 
    if (decl.length==0){
      return expression(StandardOperator.Implies,guard,claim);
    }
    BindingExpression res=new BindingExpression(
        Binder.Forall,
        primitive_type(PrimitiveSort.Boolean),
        decl,
        triggers,
        guard,
        claim
    );
    res.setOrigin(origin_stack.get());
    res.accept_if(post);
    return res;
  }
 /**
 * Create a reserved name this that also refers to the given class type.
 */
public ASTNode this_expression(ClassType t) {
  NameExpression res=new NameExpression(ASTReserved.This);
  res.setType(t);
  res.setOrigin(origin_stack.get());
  res.accept_if(post);
  return res;
}

public ASTNode diz() {
  NameExpression result = new NameExpression(ASTReserved.This);
  result.setOrigin(origin_stack.get());
  result.accept_if(post);
  return result;
}

/**
 * Create a name expression that refers to an unresolved name.
 */
public NameExpression unresolved_name(String name) {
  NameExpression res=new NameExpression(name);
  res.setKind(NameExpressionKind.Unresolved);
  res.setOrigin(origin_stack.get());
  res.accept_if(post);
  return res;
}

public VariableDeclaration variable_decl(Type type) {
  VariableDeclaration res=new VariableDeclaration(type);
  res.setOrigin(origin_stack.get());
  res.accept_if(post);
  return res;
}

/**
 * Create a new while loop.
 */
public LoopStatement while_loop(ASTNode test,ASTNode body, ASTNode ... invariant) {
  return while_loop(test,body, null, invariant);
}

public LoopStatement while_loop (ASTNode test,ASTNode body, GPUOpt gpuopt, ASTNode ... invariant){
  LoopStatement res=new LoopStatement();
  res.setEntryGuard(test);
  res.setBody(body);
  res.setOrigin(origin_stack.get());
  res.setUnroll(null);
  for (ASTNode inv:invariant) res.appendInvariant(inv);
  res.fixate();
  res.accept_if(post);
  return res;
}

/**
 * Create a new while loop.
 */
public LoopStatement while_loop(ASTNode test,ASTNode body, Contract contract) {
  return while_loop(test,body, null, contract);
}

public LoopStatement while_loop(ASTNode test,ASTNode body, GPUOpt gpuOpt, Contract contract){
  LoopStatement res=new LoopStatement();
  res.setEntryGuard(test);
  res.setBody(body);
  res.setOrigin(origin_stack.get());
  res.setUnroll(gpuOpt);
  res.setContract(contract);
  res.accept_if(post);
  return res;    
}

public Type tuple_type(Type ... t) {
  Type res=new TupleType(t);
  res.setOrigin(origin_stack.get());
  res.accept_if(post);
  return res;
}

public Type arrow_type(Type src, Type tgt) {
  Type res=new FunctionType(src, tgt);
  res.setOrigin(origin_stack.get());
  res.accept_if(post);
  return res;
}

public Type arrow_type(Type[] types, Type tgt) {
  Type res=new FunctionType(types,tgt);
  res.setOrigin(origin_stack.get());
  res.accept_if(post);
  return res;
}

public Type arrow_type(List<Type> types, Type tgt) {
  Type res=new FunctionType(types, tgt);
  res.setOrigin(origin_stack.get());
  res.accept_if(post);
  return res;
}

public ASTNode new_array(Type t, ASTNode size) {
  return expression(StandardOperator.NewArray,t,size);
}

public AxiomaticDataType adt(String name, DeclarationStatement ... pars) {
  AxiomaticDataType res=new AxiomaticDataType(name,pars);
  res.setOrigin(origin_stack.get());
  res.accept_if(post);
  return res;
}

public Axiom axiom(String name, ASTNode exp){
  Axiom res=new Axiom(name,exp);
  res.setOrigin(origin_stack.get());
  res.accept_if(post);
  return res;
}

  public OperatorExpression expression(StandardOperator op, ASTNode arg0, ASTNode [] args){
    ASTNode all_args[]=new ASTNode[args.length+1];
    all_args[0]=arg0;
    for(int i=0;i<args.length;i++){
      all_args[i+1]=args[i];
    }
    return expression(op,all_args);
  }
  
  public ActionBlock action_block(
      ASTNode history,
      ASTNode fraction,
      ASTNode process,
      ASTNode action,
      Map<String,ASTNode> map,
      ASTNode block) {
    ActionBlock res = new ActionBlock(history, fraction, process, action, map, block);
    res.setOrigin(origin_stack.get());
    res.accept_if(post);
    return res;
  }
  
  public Type __const(Type type) {
    return type_expression(TypeOperator.Const,type);
  }
  
  public Type __extern(Type type) {
    return type_expression(TypeOperator.Extern,type);
  }
  public Type __static(Type type) {
    return type_expression(TypeOperator.Static,type);
  }

  public Type type_expression(TypeOperator op,Type ... args) {
    Type res=new TypeExpression(op,args);
    res.setOrigin(origin_stack.get());
    res.accept_if(post);
    return res;
  }
  public Type __kernel(Type type) {
    return type_expression(TypeOperator.Kernel,type);
  }
  public Type __global(Type type) {
    return type_expression(TypeOperator.Global,type);
  }
  public Type __local(Type type) {
    return type_expression(TypeOperator.Local,type);
  }
  public Type __short(Type type) {
    return type_expression(TypeOperator.Short,type);
  }
  public Type __signed(Type type) {
    return type_expression(TypeOperator.Signed,type);
  }
  public Type __unsigned(Type type) {
    return type_expression(TypeOperator.Unsigned,type);
  }
  public Type __long(Type type) {
    return type_expression(TypeOperator.Long,type);
  }

  public ForEachLoop foreach(DeclarationStatement[] decls, ASTNode guard, ASTNode body) {
    ForEachLoop res=new ForEachLoop(decls,guard,body);
    res.setOrigin(origin_stack.get());
    res.accept_if(post);
    return res;
  }
  
  public NameSpace namespace(String ... name){
    return namespace(origin_stack.get(),name);
  }
  public NameSpace namespace(Origin o,String ... name){
    NameSpace res=new NameSpace(name);
    res.setOrigin(o);
    res.accept_if(post);
    return res;
  }

  public TryCatchBlock try_catch(BlockStatement main, BlockStatement after) {
     return try_catch(origin_stack.get(),main,after);
  }

  public TryCatchBlock try_catch(Origin o, BlockStatement main,
      BlockStatement after) {
    TryCatchBlock res=new TryCatchBlock(main,after);
    res.setOrigin(o);
    res.accept_if(post);
    return res;
  }

  public TryWithResources try_with_resources(BlockStatement main, BlockStatement after) {
    return try_with_resources(origin_stack.get(), main, after);
  }

  public TryWithResources try_with_resources(Origin o, BlockStatement main, BlockStatement after) {
    TryWithResources res = new TryWithResources(main, after);
    res.setOrigin(o);
    res.accept_if(post);
    return res;
  }

  public FieldAccess set_field(Origin o, ClassName claz, ASTNode obj, String name, ASTNode val){
    FieldAccess res=new FieldAccess(claz, obj, name, val);
    res.setOrigin(o);
    res.accept_if(post);    
    return res;
  }
  
  public FieldAccess set_field(ClassName claz,ASTNode obj,String name,ASTNode val){
    return set_field(origin_stack.get(),claz,obj,name,val);
  }

  public FieldAccess get_field(ClassName claz,ASTNode obj,String name){
    return set_field(origin_stack.get(),claz,obj,name,null);
  }

  public ASTNode invariant_block(String label, ASTNode inv, BlockStatement block) {
    return invariant_block(origin_stack.get(),label,inv,block);
  }

  private ASTNode invariant_block(Origin origin, String label, ASTNode inv,
      BlockStatement block) {
    ParallelInvariant res=new ParallelInvariant(label,inv,block);
    res.setOrigin(origin);
    res.accept_if(post);
    return res;
  }

  public ParallelRegion region(Contract c,ParallelBlock ... blocks) {
    return region(origin_stack.get(),c,blocks);
  }
  
  public ParallelRegion region(Contract c, List<ParallelBlock> blocks) {
    return region(origin_stack.get(), c, blocks);
  }

  public ParallelRegion region(Origin origin,Contract c,ParallelBlock ... blocks) {
    ParallelRegion res=new ParallelRegion(c,blocks);
    res.setOrigin(origin);
    res.accept_if(post);
    return res;
  }

    public ASTNode expression(StandardOperator op, ASTNode n,java.util.List<ASTNode> ns) {
        return expression(op,n,ns.toArray(new ASTNode[ns.size()]));
    }

  public ParallelRegion region(Origin origin, Contract c, List<ParallelBlock> blocks) {
    ParallelRegion res=new ParallelRegion(c, blocks);
	res.setOrigin(origin);
	res.accept_if(post);
	return res;
  }

  public ASTNode region(Contract c,ArrayList<ParallelBlock> res) {
    return region(c,res.toArray(new ParallelBlock[res.size()]));
  }

  public Method function_decl(Type t, Contract contract, String name,
      java.util.List<DeclarationStatement> args, ASTNode body) {
    return function_decl(t,contract,name,args.toArray(new DeclarationStatement[args.size()]),body);
  }

  public ClassType class_type(String name, Map<String, Type> args) {
    return class_type(name,toArray(args));
  }

  private <F extends ASTNode> ASTNode[] toArray(Map<String, F> map){
    ArrayList<ASTNode> list=new ArrayList<ASTNode>();
    for(Entry<String, F> e:map.entrySet()){
      ASTNode n=e.getValue();
      n.addLabel(label(e.getKey()));
      list.add(n);
    }
    return list.toArray(new ASTNode[map.size()]);
  }

  public DeclarationStatement[] to_decls(List<Triple<Origin,String,Type>> vars){
    DeclarationStatement[] res=new DeclarationStatement[vars.size()];
    for(int i=0;i<res.length;i++){
      Triple<Origin,String,Type> d=vars.get(i);
      res[i]=field_decl(d.v1,d.v2,d.v3);
    }
    return res;
  }
  public ASTNode exists(ASTNode e, List<Triple<Origin,String,Type>> vars) {
    return exists(constant(true),e,to_decls(vars));
  }

  public TypeVariable type_variable(String name) {
    return type_variable(origin_stack.get(),name);
  }
  
  public TypeVariable type_variable(Origin o,String name) {
    TypeVariable res=new TypeVariable(name);
    res.setOrigin(o);
    res.accept_if(post);    
    return res;
  }

  public ASTNode special(Kind kind, ArrayList<ASTNode> args) {
    return special(kind,args.toArray(new ASTNode[args.size()]));
  }
  
  public StructValue struct_value(Origin o, Type type, Map<String, Integer> map, ASTNode ... values) {
	if (map == null) {
	  map = new java.util.Hashtable<String, Integer>();
	}
    StructValue res=new StructValue(type,map,values);
    res.setOrigin(o);
    res.accept_if(post);    
    return res;
  }

  public StructValue struct_value(Type type,Map<String, Integer> map, ASTNode ... values) {
    return struct_value(origin_stack.get(),type,map,values);
  }
  
  public StructValue struct_value(Type type,Map<String, Integer> map, List<ASTNode> values) {
    return struct_value(origin_stack.get(),type,map,values.toArray(new ASTNode[values.size()]));
  }

  public VectorBlock vector_block(DeclarationStatement iter,BlockStatement block) {
    return vector_block(origin_stack.get(),iter,block);
  }

  public VectorBlock vector_block(Origin o, DeclarationStatement iter,BlockStatement block) {
    VectorBlock res=new VectorBlock(iter,block);
    res.setOrigin(o);
    res.accept_if(post);    
    return res;
  }

  public Constraining constraining(BlockStatement block, NameExpression ... vars) {
    return constraining(origin_stack.get(),block,vars);
  }

  private Constraining constraining(Origin o, BlockStatement block,NameExpression ... vars) {
    Constraining res=new Constraining(block,vars);
    res.setOrigin(o);
    res.accept_if(post);    
    return res;
  }

  public Constraining constraining(BlockStatement block, List<NameExpression> names) {
    return constraining(origin_stack.get(),block,names.toArray(new NameExpression[names.size()]));
  }

  public ASTNode special(Kind kind, List<ASTNode> names){
    return special(kind,names.toArray(new ASTNode[names.size()]));
  }

  public ASTNode switch_statement(ASTNode expr, List<Case> cases) {
    return switch_statement(expr, cases.toArray(new Case[cases.size()]));
  }

  public ASTNode switch_statement(ASTNode expr, Case[] cases) {
    Switch res=new Switch(expr, cases);
    res.setOrigin(origin_stack.get());
    res.accept_if(post);    
    return res;
  }
  
  /**
   * Creates an AST structure for the postfix statement: `x%op%` for some unary operator `%op%`.
   * The statement is then rewritten to `x := x %op% 1` for a binary variant of the operator `%op%`.
   * @param varname The variable name that is subject to `%op%`.
   * @param operator The binary operator `%op%`. 
   * @return The AST structure that represents the incrementation.
   */
  private ASTNode postfix_operator(String varname, StandardOperator operator) {
	  NameExpression location = identifier(varname);
	  ASTNode arguments[] = new ASTNode[] { location, new ConstantExpression(1) };
	  OperatorExpression incr = new OperatorExpression(operator, arguments);
	  AssignmentStatement res = new AssignmentStatement(location, incr);
	  
	  res.setOrigin(origin_stack.get());
	  res.accept_if(post);
	  return res;
  }

  /**
   * Creates an AST structure for the postfix incremental statement: `x++`. However,
   * the statement is rewritten to `x := x + 1`.
   * @param varname The variable name that is subject to incrementation. 
   * @return The AST structure that represents the incrementation.
   */
  public ASTNode postfix_increment(String varname) {
    return postfix_operator(varname, StandardOperator.Plus);
  }
  
  /**
   * Creates an AST structure for the postfix decremental statement: `x--`. However,
   * the statement is rewritten to `x := x - 1`.
   * @param varname The variable name that is subject to decrementation. 
   * @return The AST structure that represents the decrementation.
   */
  public ASTNode postfix_decrement(String varname) {
    return postfix_operator(varname, StandardOperator.Minus); 
  }

  public Synchronized syncBlock(ASTNode expr, ASTNode statement) {
    return syncBlock(origin_stack.get(), expr, statement);
  }

  public Synchronized syncBlock(Origin origin, ASTNode expr, ASTNode statement) {
    Synchronized sync = new Synchronized(expr, statement);
    sync.setOrigin(origin);
    return sync;
  }

  public InlineQuantifierPattern pattern(ASTNode inner) {
    return pattern(origin_stack.get(), inner);
  }

  public InlineQuantifierPattern pattern(Origin origin, ASTNode inner) {
    InlineQuantifierPattern pattern = new InlineQuantifierPattern(inner);
    pattern.setOrigin(origin);
    return pattern;
  }

  public ASTSpecial gotoStatement(NameExpression label) {
    return special(ASTSpecial.Kind.Goto, label);
  }

  public ASTSpecial gotoStatement(String targetLabel) {
    return special(ASTSpecial.Kind.Goto, label(targetLabel));
  }

  public ASTSpecial labelDecl(String labelName) {
    return labelDecl(label(labelName));
  }

  public ASTSpecial labelDecl(NameExpression label) {
    return special(Kind.Label, label);
  }

  public CatchClause catchClause(String name, Type[] types, BlockStatement block) {
    return catchClause(origin_stack.get(), name, types, block);
  }

  public CatchClause catchClause(Origin origin, String name, Type[] types, BlockStatement block) {
    CatchClause cc = new CatchClause(name, types, block);
    cc.setOrigin(origin);
    return cc;
  }

  public SignalsClause signalsClause(String name, Type type, ASTNode condition) {
    return signalsClause(origin_stack.get(), name, type, condition);
  }

  public SignalsClause signalsClause(Origin origin, String name, Type type, ASTNode condition) {
    SignalsClause sc = new SignalsClause(name, type, condition);
    sc.setOrigin(origin);
    return sc;
  }
}

