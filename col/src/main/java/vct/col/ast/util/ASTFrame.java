package vct.col.ast.util;

import hre.lang.Failure;
import hre.util.SingleNameSpace;
import scala.Option;
import vct.col.ast.expr.*;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.*;
import vct.col.ast.stmt.decl.*;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.type.PrimitiveType;
import vct.col.ast.type.TypeExpression;
import vct.col.ast.type.TypeOperator;

import java.util.Stack;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Utility class that provides common functionality for exploring abstract syntax trees.
 * 
 * @author Stefan Blom
 *
 */
public abstract class ASTFrame<T> {
  
  /**
   * Information record for variables.
   * 
   * @author Stefan Blom
   */
  public static class VariableInfo {
    
    /**
     * Reference to the place where the variable was defined.
     */
    public final ASTNode reference;
    
    /**
     * Stores the kind of the variable.
     */
    public final NameExpressionKind kind;
    
    /**
     * Constructor for a variable info record.
     */
    public VariableInfo(ASTNode reference, NameExpressionKind kind){
      this.reference=reference;
      this.kind=kind;
    }
  }
  
  public final SingleNameSpace<String,VariableInfo> variables;
  
  /**
   * Field for communicating return value.
   */
  protected T result;
  
  private AtomicReference<T> result_ref;
  
  private Stack<T> result_stack;
  
  public T getResult(){
    return result_ref.get();
  }

  /**
   * Holds the source program unit.
   */
  private ProgramUnit source;
  
  /**
   * Getter form the source program unit.
   * @return source program unit.
   */
  public ProgramUnit source(){
    return source;
  }
  
  /**
   * Holds the target program unit, or null.
   */
  private ProgramUnit target;
  
  /**
   * Getter form the target program unit.
   * @return target program unit.
   */
 public ProgramUnit target(){
    return target;
  }

  /**
   * Create a new frame with just a source program unit.
   * 
   * @param source
   */
  public ASTFrame(ProgramUnit source,boolean do_scope){
    this(source,null,do_scope);
  }
  
  /**
   * Create a new frame with both source and target program units.
   * 
   * @param source
   */
  public ASTFrame(ProgramUnit source,ProgramUnit target,boolean do_scope){
    this.source=source;
    this.target=target;
    node_stack=new Stack<ASTNode>();
    class_stack=new Stack<ASTClass>();
    method_stack=new Stack<Method>();
    result_stack=new Stack<T>();
    result_ref=new AtomicReference<T>();
    variables=new SingleNameSpace<String,VariableInfo>();
    scope=new ManageScope();
  }
  
  /**
   * Create a shared frame.
   * 
   * @param share The frame with which to share information.
   */
  public ASTFrame(ASTFrame<T> share){
    node_stack=share.node_stack;
    class_stack=share.class_stack;
    method_stack=share.method_stack;
    source=share.source;
    target=share.target;
    result_stack=share.result_stack;
    result_ref=share.result_ref;
    variables=share.variables;
    scope=share.scope;
  }
  
  /**
   * Stack of current nodes.
   */
  private Stack<ASTNode> node_stack;
  
  /** */
  protected ASTNode getParentNode(){
    return node_stack.size() >= 2 ? node_stack.get(node_stack.size()-2) : null;
  }

  protected ASTNode getAncestor(int stepsBack) {
    return node_stack.size() >= 2 + stepsBack ? node_stack.get(node_stack.size()-2-stepsBack) : null;
  }

  /**
   * Stack of current classes.
   */
  private Stack<ASTClass> class_stack;
  
  /**
   * Stack of current methods.
   */
  private Stack<Method> method_stack;


  public void enter_before(ASTNode node){
    /* Might be needed or not:
    variables.enter();
    if (node.getParent() instanceof MethodInvokation){
      MethodInvokation mi=(MethodInvokation)node.getParent();
      Contract c=null;
      if (mi.getDefinition()!=null){
        c=mi.getDefinition().getContract();
      }
      if (c!=null){
        for(DeclarationStatement decl:c.given){
          variables.add(decl.getName(),new VariableInfo(decl,NameExpressionKind.Argument));
        }
        scan_labels(c.pre_condition);
      }
    }
    */
  }
  public void leave_before(ASTNode node){
    /* must match enter! 
    variables.leave();
    */
  }
  public void enter_after(ASTNode node){
    if (scope!=null) scope.enter_after(node);
  }
  public void leave_after(ASTNode node){
    if (scope!=null) scope.leave_after(node);
  }

  enum Action {ENTER,LEAVE,ENTER_AFTER,LEAVE_AFTER};
  
  final ManageScope scope;
      
  class ManageScope extends EmptyVisitor<Action> {

    public void enter(ASTNode n){
      action=Action.ENTER;
      n.accept_simple(this);
    }
    
    public void leave(ASTNode n){
      action=Action.LEAVE;
      n.accept_simple(this);
    }
    
    public void enter_after(ASTNode n){
      action=Action.ENTER_AFTER;
      n.accept_simple(this);
    }
    
    public void leave_after(ASTNode n){
      action=Action.LEAVE_AFTER;
      n.accept_simple(this);
    }
    
    public Action action;

    @Override
    public void visit(AxiomaticDataType adt) {
      switch(action){
      case ENTER:
        variables.enter();
        for (DeclarationStatement decl : adt.parametersJava()) {
          variables.add(decl.name(), new VariableInfo(decl, NameExpressionKind.Argument));
        }
        break;
      case LEAVE:
        variables.leave();
        break;
      default:
        break;
      }
    }
 
/*
    @Override
    public void visit( node){
      switch(action){
      case ENTER:
        break;
      case LEAVE:
        break;
      }
    }
  */
    
    @Override
    public void visit(MethodInvokation node){
      switch(action){
      case ENTER:
        for(NameExpression label:node.getLabels()){
          variables.add(label.getName(),new VariableInfo(node, NameExpressionKind.Label));
        }
        break;
      case LEAVE:
        break;
      case ENTER_AFTER:
        method_stack.push(node.getDefinition());
        break;
      case LEAVE_AFTER:
        method_stack.pop();
        break;
      }
    }

    
    @Override
    public void visit(OperatorExpression node){
      switch(action){
      case ENTER:
        switch(((OperatorExpression)node).operator()){
          case BindOutput:{
            ASTNode e=((OperatorExpression) node).arg(0);
            if (e instanceof NameExpression){
              NameExpression name=(NameExpression) e;
              variables.add(name.getName(),new VariableInfo(node, NameExpressionKind.Output));
            } else {
              Abort("unexpected output binder argument: %s",e.getClass());
            }
            break;
          }
        default:
          break;
        }
        break;
      case LEAVE:
        break;
      default:
        break;
      }
    }

    @Override
    public void visit(VariableDeclaration decl) {
      for(DeclarationStatement stat : decl.flatten()) {
        switch(action) {
          case ENTER:
            variables.add(stat.name(), new VariableInfo(stat, NameExpressionKind.Local));
            break;
          case LEAVE:
            break;
        }
      }
    }
   
    @Override
    public void visit(DeclarationStatement node){
      switch(action){
      case ENTER:
        DeclarationStatement decl=(DeclarationStatement)node;
        if (decl.getParent() instanceof BlockStatement || decl.getParent()==null){
          variables.add(decl.name(), new VariableInfo(decl, NameExpressionKind.Local));
        }
        break;
      case LEAVE:
        break;
      default:
        break;
      }
    }
    
    @Override
    public void visit(ASTSpecial node){
      switch(action){
      case ENTER:
        switch(((ASTSpecial)node).kind){
        case Witness:{
          for(NameExpression name:node.getArg(0).getLabels()){
            variables.add(name.getName(),new VariableInfo(node, NameExpressionKind.Label));
          }
          break;
        }
        case Apply:{
          for(NameExpression name:node.getLabels()){
            variables.add(name.getName(),new VariableInfo(node, NameExpressionKind.Label));
          }
          break;          
        }
        case CreateHistory:
           scan_labels(((ASTSpecial)node).args[0]);
        default:
          break;
        }
        break;
      case LEAVE:
        break;
      default:
        break;
      }
    }

    
    @Override
    public void visit(ASTClass node){
      switch(action){
      case ENTER:
        class_stack.push((ASTClass)node);
        variables.enter();
        recursively_add_class_info((ASTClass)node);
        Contract contract=((ASTClass)node).getContract();
        if (contract!=null){
          for (DeclarationStatement decl:contract.given){
            variables.add(decl.name(), new VariableInfo(decl, NameExpressionKind.Field));
          }
        }
        break;
      case LEAVE:
        variables.leave();
        class_stack.pop(); 
        break;
      default:
        break;
      }
    }

    @Override
    public void visit(Method node){
      switch(action){
      case ENTER:
        method_stack.push(node);
        variables.enter();
        for (DeclarationStatement decl:(node).getArgs()) {
          variables.add(decl.name(), new VariableInfo(decl, NameExpressionKind.Argument));
        }
        if(node.getReturnType() instanceof TypeExpression && ((TypeExpression) node.getReturnType()).operator() == TypeOperator.Kernel) {
          for(String kernelArgument : new String[]{"opencl_lid", "opencl_gid", "opencl_gcount", "opencl_gsize"}) {
            variables.add(kernelArgument, new VariableInfo(
                    new DeclarationStatement(kernelArgument, new PrimitiveType(PrimitiveSort.Integer)),
                    NameExpressionKind.Argument));
          }
        }
        add_contract_vars(node);
        break;
      case LEAVE:
        method_stack.pop();
        variables.leave();
        break;
      default:
        break;
      }
    }

    @Override
    public void visit(BlockStatement node){
      switch(action){
      case ENTER:
        variables.enter();
        if (node.getParent() instanceof MethodInvokation){
          MethodInvokation s=(MethodInvokation)node.getParent();
          Method def=s.getDefinition();
          add_contract_vars(def);
        }
        BlockStatement block=(BlockStatement)node;
        int N=block.size();
        for(int i=0;i<N;i++){
          scan_labels(block.getStatement(i));
        }
        break;
      case LEAVE:
        variables.leave();
       break;
      default:
        break;
      }
    }

    @Override
    public void visit(LoopStatement loop){
      switch(action){
      case ENTER:
        variables.enter();
        for(ASTNode inv:loop.getInvariants()){
          scan_labels(inv);
        }
        if (loop.getInitBlock() instanceof DeclarationStatement){
          DeclarationStatement decl=(DeclarationStatement)loop.getInitBlock();
          variables.add(decl.name(), new VariableInfo(decl, NameExpressionKind.Local));
        }
        if (loop.getInitBlock() instanceof BlockStatement){
          BlockStatement block=(BlockStatement)loop.getInitBlock();
          int N=block.getLength();
          for(int i=0;i<N;i++){
            if (block.getStatement(i) instanceof DeclarationStatement){
              DeclarationStatement decl=(DeclarationStatement)block.getStatement(i);
              variables.add(decl.name(), new VariableInfo(decl, NameExpressionKind.Local));
            } else if(block.getStatement(i) instanceof VariableDeclaration) {
              for(DeclarationStatement child : ((VariableDeclaration) block.getStatement(i)).flatten()) {
                variables.add(child.name(), new VariableInfo(child, NameExpressionKind.Local));
              }
            }
          }
        }
        break;
      case LEAVE:
        variables.leave();
        break;
      default:
        break;
      }
    }

    @Override
    public void visit(ForEachLoop node){
      switch(action){
      case ENTER:
        variables.enter();
        ForEachLoop loop=(ForEachLoop)node;
        for(DeclarationStatement decl:loop.decls){
          variables.add(decl.name(), new VariableInfo(decl, NameExpressionKind.Local));
        }
        break;
      case LEAVE:
        variables.leave();
        break;
      default:
        break;
      }
    }

    @Override
    public void visit(BindingExpression node){
      switch(action){
      case ENTER:
        variables.enter();
        for(DeclarationStatement decl:((BindingExpression)node).getDeclarations()){
          variables.add(decl.name(), new VariableInfo(decl, NameExpressionKind.Local));
        }
        break;
      case LEAVE:
        variables.leave();
        break;
      default:
        break;
      }
    }

    @Override
    public void visit(ParallelBlock pb){
      switch(action){
      case ENTER:
        variables.enter();
        for (DeclarationStatement decl : pb.itersJava()) {
          variables.add(decl.name(), new VariableInfo(decl, NameExpressionKind.Local));
        }
        break;
      case LEAVE:
        variables.leave();
        break;
      default:
        break;
      }
    }
    @Override
    public void visit(VectorBlock pb){
      switch(action){
      case ENTER:
        variables.enter();
        variables.add(pb.iter().name(), new VariableInfo(pb.iter(), NameExpressionKind.Local));
        break;
      case LEAVE:
        variables.leave();
        break;
      default:
        break;
      }
    }

    @Override
    public void visit(CatchClause cc) {
      switch(action){
        case ENTER:
          variables.enter();
          if (cc.javaCatchTypes().length != 1) {
            // To support this, the least upper bound of all the types must be used here
            // See: https://docs.oracle.com/javase/specs/jls/se8/html/jls-14.html#jls-14.20
            Abort("Multi-catch not yet supported");
          }
          DeclarationStatement ccDeclarationStatement = new DeclarationStatement(cc.name(), cc.javaCatchTypes()[0], Option.empty());
          variables.add(cc.name(), new VariableInfo(ccDeclarationStatement, NameExpressionKind.Local));
          break;
        case LEAVE:
          variables.leave();
          break;
        default:
          break;
      }
    }

    @Override
    public void visit(SignalsClause sc) {
      switch(action){
        case ENTER:
          variables.enter();
          variables.add(sc.name(), new VariableInfo(sc.asDeclarationStatement(), NameExpressionKind.Local));
          break;
        case LEAVE:
          variables.leave();
          break;
        default:
          break;
      }
    }
  };
  
  private void recursively_add_class_info(ASTClass cl) {
    for (int i=0;i<cl.super_classes.length;i++){
      if (source != null) {
        ASTClass super_class=source.find(cl.super_classes[i]);
        if (super_class!=null){
          recursively_add_class_info(super_class);
        }
      }
    }
    for (int i=0;i<cl.implemented_classes.length;i++){
      if (source != null) {
        ASTClass super_class=source.find(cl.implemented_classes[i]);
        if (super_class!=null){
          recursively_add_class_info(super_class);
        }
      }
    }
    for(DeclarationStatement decl:cl.dynamicFields()){
      variables.add(decl.name(),new VariableInfo(decl, NameExpressionKind.Field));
    }
    for(DeclarationStatement decl:cl.staticFields()){
      variables.add(decl.name(),new VariableInfo(decl, NameExpressionKind.Field));
    }
  }

  private void add_contract_vars(Method m) {
    if (m==null) return;
    Contract c=m.getContract();
    if (c==null) return;
    for(DeclarationStatement decl:c.given){
      variables.add(decl.name(),new VariableInfo(decl, NameExpressionKind.Argument));
    }
    scan_labels(c.pre_condition);
    for(DeclarationStatement decl:c.yields){
      variables.add(decl.name(),new VariableInfo(decl, NameExpressionKind.Local));
    }
    scan_labels(c.post_condition);
  }

  
  private void scan_labels(ASTNode node) {
    //if (node instanceof MethodInvokation){
      for(NameExpression label:node.getLabels()){
        variables.add(label.getName(),new VariableInfo(node, NameExpressionKind.Label));
      }
      if (node instanceof ASTSpecial){
        ASTSpecial s=(ASTSpecial)node;
        if (s.kind==ASTSpecial.Kind.Label){
          NameExpression label=(NameExpression)s.args[0];
          variables.add(label.getName(),new VariableInfo(node, NameExpressionKind.Label));
        }
      }
    //}
    if (node instanceof OperatorExpression){
      for (ASTNode arg : ((OperatorExpression)node).argsJava()) {
        scan_labels(arg);
      }
    }
  }

  public void enter(ASTNode node){
    node_stack.push(node);
    Debug("entering %s",node.getClass());
    result_stack.push(result);
    result=null;
    if (scope!=null) scope.enter(node);
  }

  public void leave(ASTNode node){
    if (scope!=null) scope.leave(node);
    result_ref.set(result);
    result=result_stack.pop();
    Debug("leaving %s",node.getClass());
    node_stack.pop();
  }

  public ASTNode current_node(){
    if (node_stack.isEmpty()) {
      return null;
    } else {
      return node_stack.peek();
    }
  }

  public ASTClass current_class(){
    if (class_stack.isEmpty()){
      return null;
    } else {
      return class_stack.peek();
    }
  }
  
  public Method current_method(){
    if (method_stack.isEmpty()) {
      return null;
    } else {
      return method_stack.peek();
    }
  }
  
  public void Abort(String format,Object ...args){
    hre.lang.System.Abort("("+this.getClass()+")At "+current_node().getOrigin()+": "+format,args);
  }
  public void Debug(String format,Object ...args){
    ASTNode node=current_node();
    if (node!=null){
      hre.lang.System.Debug("At "+node.getOrigin()+": "+format,args);
    } else {
      hre.lang.System.Debug(format,args);
    }
  }
  public void Fail(String format,Object ...args){
    if (current_node()!=null){
      hre.lang.System.Fail("At "+current_node().getOrigin()+": "+format,args);
    } else {
      hre.lang.System.Fail(format,args);
    }
  }

  public Failure Failure(String format, Object... args) {
    if(current_node() != null) {
      return hre.lang.System.Failure("At " + current_node() + ": " + format, args);
    } else {
      return hre.lang.System.Failure(format, args);
    }
  }

  public void Warning(String format,Object ...args){
    if (current_node()!=null){
      hre.lang.System.Warning("("+this.getClass()+"): at "+current_node().getOrigin()+": "+format, args);
    } else {
      hre.lang.System.Warning("("+this.getClass()+"): "+format, args);
    }
  }

}
