package vct.col.rewrite;

import hre.ast.MessageOrigin;

import java.util.Stack;

import vct.col.ast.*;

public class Flatten extends AbstractRewriter {

  public Flatten(ProgramUnit source) {
    super(source);
  }

  /* TODO check for pure expression while copying! */
  private AbstractRewriter copy_pure=new AbstractRewriter(source());
  
  private Stack<BlockStatement> block_stack=new Stack<BlockStatement>();
  private BlockStatement current_block=null;
  private BlockStatement declaration_block=null;
  private static long counter=0;
  
  @Override
  public void visit(ASTSpecial s){
    result=copy_pure.rewrite(s);
  }
  public void visit(BlockStatement s){
    block_stack.push(current_block);
    current_block=create.block();
    visit_body(s);
    result=current_block;
    current_block=block_stack.pop();
  }

  public void visit(MethodInvokation e) {
    Debug("call to %s",e.method);
    ASTNode object=rewrite(e.object);
    int N=e.getArity();
    ASTNode args[]=new ASTNode[N];
    for(int i=0;i<N;i++){
      args[i]=e.getArg(i).apply(this);
    }
    String name="__flatten_"+(++counter);
    if (e.getType()==null){
      Abort("result type of call unknown at %s",e.getOrigin());
    }
    if (e.getType().isVoid()||e.getType().isNull()||declaration_block==null){
      result=create.invokation(object,rewrite(e.dispatch),e.method,args);
      ((MethodInvokation)result).set_before(copy_rw.rewrite(e.get_before()));
      ((MethodInvokation)result).set_after(copy_rw.rewrite(e.get_after()));
    } else {
      Debug("declaring variable %s (%s)",name,e.getType());
      ASTNode n=create.field_decl(name,e.getType());
      Debug("inserting in %s",declaration_block);
      declaration_block.add_statement(n);
      Debug("assigning resutl of call");
      MethodInvokation call=create.invokation(object,rewrite(e.dispatch),e.method,args);
      call.set_before(copy_pure.rewrite(e.get_before()));
      call.set_after(copy_pure.rewrite(e.get_after()));
      for(NameExpression lbl:e.getLabels()){
        Debug("FLATTEN: copying label %s",lbl);
        call.addLabel(lbl);
      }
      current_block.add_statement(create.assignment(create.local_name(name),call));
      Debug("return variable name");
      result=create.local_name(name);
      auto_labels=false;
    }
  }
 
  public void visit(OperatorExpression e){
    if (e.getType()==null) Abort("untyped operator %s in clause at %s",e.operator(),e.getOrigin());
    switch(e.operator()){
    case AddAssign:
    {
      ASTNode loc=e.arg(0);
      ASTNode loc_res=loc.apply(this);
      
      ASTNode val=e.arg(1);
      ASTNode val_res=val.apply(this);
      
      //current_block.add_statement(create.assignment(loc_res,create.expression(StandardOperator.Plus,loc_res,val_res)));
      //result=null;
      result=create.expression(StandardOperator.Assign,loc_res,create.expression(StandardOperator.Plus,loc_res,val_res));
      return;
    }
    case PreIncr:
    case PreDecr:
    {
      boolean expression=e.getParent()==null;
      StandardOperator op=e.operator()==StandardOperator.PreIncr?StandardOperator.Plus:StandardOperator.Minus;
      ASTNode arg=e.arg(0);
      ASTNode arg_out=arg.apply(this);
      
      String name="__flatten_"+(++counter);
      if (expression){
        declaration_block.add_statement(create.field_decl(name,e.getType(),null));
      }
      ASTNode effect=create.assignment(arg_out,create.expression(op,arg_out,create.constant(1)));
      if (expression){
        current_block.add_statement(effect);
        current_block.add_statement(create.assignment(create.local_name(name),arg_out));
        result=create.local_name(name);
      } else {
        result=effect;
      }
      return;
    }
    case PostIncr:
    case PostDecr:
    {
      StandardOperator op=e.operator()==StandardOperator.PostIncr?StandardOperator.Plus:StandardOperator.Minus;
      ASTNode arg=e.arg(0);
      ASTNode arg_out=arg.apply(this);
      String name="__flatten_"+(++counter);
      declaration_block.add_statement(create.field_decl(name,e.getType(),null));
      current_block.add_statement(create.assignment(create.local_name(name),arg_out));
      current_block.add_statement(create.assignment(arg_out,create.expression(op,arg_out,create.constant(1))));
      result=create.local_name(name);
      return;
    }
    default:
      super.visit(e);
      return;
    }
  }
  
  public void visit(DeclarationStatement s){
    Type t=s.getType();
    ASTNode tmp=t.apply(this);
    if (tmp instanceof Type){
      t=(Type)tmp;
    } else {
      throw new Error("type AST rewrote to non-type AST");
    }
    String name=s.getName();
    ASTNode init=s.getInit();
    if (init!=null) {
      if (current_block==null){
        Abort("internal error: current block is null");
      }
      current_block.add_statement(create.field_decl(name,t,null));
      init=init.apply(this);
      result=create.assignment(create.local_name(name),init);
    } else {
      result=create.field_decl(name,t,null);
    }
  }
  
  @Override
  public void visit(AssignmentStatement s) {
    ASTNode loc=s.location();
    ASTNode val=s.expression();
    if (loc instanceof Dereference
    && !val.getType().equals(ClassType.null_type)
    && !val.getType().equals(ClassType.label_type)){
      loc=rewrite(loc);
      val=add_as_var(val);      
    } else {
      loc=rewrite(loc);
      val=rewrite(val);
    }
    result=create.assignment(loc,val);
    //if (//s.getLocation().getType().equals(ClassType.label_type)||
    //    s.getExpression().getType().equals(ClassType.label_type)){
    //  ASTNode loc=s.getLocation().apply(this);
    //  ASTNode val=s.getExpression().apply(this);
    //  result=create.assignment(loc,val);
    /*  return;
    }
    ASTNode loc=s.getLocation().apply(this);
    ASTNode val=rewrite(s.getExpression());
    if (loc instanceof Dereference){
      Dereference d=(Dereference)loc;
      if (d.field.equals("item")){
        Dereference old_d=(Dereference)s.getLocation();
        if (old_d.object.isa(StandardOperator.Subscript)){
          OperatorExpression e=(OperatorExpression)old_d.object;
          ASTNode ref=copy_rw.rewrite(d.object);
          ASTNode ar=copy_rw.rewrite(e.getArg(0));
          ASTNode idx=copy_rw.rewrite(e.getArg(1));
          String var_name="__random_i";
          DeclarationStatement decl=create.field_decl(var_name, create.primitive_type(Sort.Integer));
          
          ASTNode claim=create.expression(StandardOperator.NEQ,ref,
              create.expression(StandardOperator.Subscript,ar,create.local_name(var_name)));;
          ASTNode guard1=create.expression(StandardOperator.LTE,create.constant(0),create.local_name(var_name));
          ASTNode guard2=create.expression(StandardOperator.LT,create.local_name(var_name),
              create.expression(StandardOperator.Size,ar));
          ASTNode guard3=create.expression(StandardOperator.NEQ,idx,create.local_name(var_name));
          ASTNode guard=create.fold(StandardOperator.And,guard1,guard2,guard3);
          ASTNode clue=create.forall(guard, claim, decl);
          current_block.add_statement(create.expression(StandardOperator.Assume,clue));
        }
      }
    }
    result=create.assignment(loc,val);
    */
  }

  private ASTNode add_as_var(ASTNode e){
    create.enter();
    create(e);
    String name="__flatten_"+(++counter);
    if (e.getType()==null){
      Abort("result type unknown at %s",e.getOrigin());
    }
    Type t=e.getType();
    if (t.getOrigin()==null){
      Debug("fixing null origin near %s",e.getOrigin());
      t.setOrigin(new MessageOrigin("Flatten.add_as_var fix near %s",e.getOrigin()));
    }
    ASTNode n=create.field_decl(name,t);
    declaration_block.add_statement(n);
    ASTNode ee=e.apply(this);
    current_block.add_statement(create.assignment(create.local_name(name),ee));
    ASTNode tmp=create.local_name(name);
    create.leave();
    return tmp;
  }

  public void visit(ReturnStatement s){
    ASTNode e=s.getExpression();
    if (e!=null){
      e=add_as_var(e);
      result=create.return_statement(e);
    } else {
      result=create.return_statement();
    }
    ((ReturnStatement)result).set_after(copy_rw.rewrite(s.get_after()));
  }
  private void visit_body(ASTNode body){
    if (body instanceof BlockStatement){
      BlockStatement s=(BlockStatement)body;
      int N=s.getLength();
      for(int i=0;i<N;i++){
        ASTNode stat=s.getStatement(i);
        if (stat instanceof ReturnStatement){
          // TODO: properly implement this with exec before and exec after.
          ASTNode last=stat.apply(this);
          for(i++;i<N;i++){
            visit_body(s.getStatement(i));
          }
          current_block.add_statement(last);
        } else {
          visit_body(s.getStatement(i));
        }
      }
    } else {
      current_block.add_statement(body.apply(this));
    }
  }
  
  public void visit(IfStatement s) {
    IfStatement res=new IfStatement();
    res.setOrigin(s.getOrigin());
    int N=s.getCount();
    for(int i=0;i<N;i++){
      ASTNode guard=s.getGuard(i);
      if (guard!=IfStatement.elseGuard()) guard=guard.apply(copy_pure);
      block_stack.push(current_block);
      current_block=create.block();
      visit_body(s.getStatement(i));
      res.addClause(guard,current_block);
      current_block=block_stack.pop();
    }
    result=res; return ;
  }

  public void visit(Method m) {
    switch(m.kind){
    case Predicate:
    case Pure:
      result=copy_pure.rewrite(m);
      return;
    default:
      break;
    }
    String name=m.getName();
    DeclarationStatement args[]=copy_pure.rewrite(m.getArgs());
    Contract mc=m.getContract();
    Contract c=null;
    if (mc!=null){
      c=copy_pure.rewrite(mc);
    }
    Method.Kind kind=m.kind;
    Method res=create.method_kind(kind,rewrite(m.getReturnType()) , c, name, args, m.usesVarArgs(),null);
    ASTNode body=m.getBody();
    if (body!=null) {
      if (body instanceof BlockStatement) {
        // if block
        block_stack.push(current_block);
        current_block=create.block();
        declaration_block=current_block;
        visit_body(body);
        declaration_block=null;
        res.setBody(current_block);
        current_block=block_stack.pop();
      } else {
        // if expression (pure function or predicate!)
        res.setBody(body.apply(copy_pure));
      }
    }
    result=res;
  }
  
  @Override
  public void visit(LoopStatement s) {
    //checkPermission(s);
    LoopStatement res=new LoopStatement();
    ASTNode tmp;
    tmp=s.getInitBlock();
    if (tmp!=null) res.setInitBlock(tmp.apply(copy_pure));
    tmp=s.getUpdateBlock();
    if (tmp!=null) res.setUpdateBlock(tmp.apply(copy_pure));
    tmp=s.getEntryGuard();
    if (tmp!=null) res.setEntryGuard(tmp.apply(copy_pure));
    tmp=s.getExitGuard();
    if (tmp!=null) res.setExitGuard(tmp.apply(copy_pure));
    for(ASTNode inv:s.getInvariants()){
      res.appendInvariant(inv.apply(copy_pure));
    }
    tmp=s.getBody();
    res.setBody(tmp.apply(this));
    res.setOrigin(s.getOrigin());
    res.set_before(copy_rw.rewrite(s.get_before()));
    res.set_after(copy_rw.rewrite(s.get_after()));
    result=res; return ;
  }

  private boolean simple_expression(ASTNode n){
    return (n instanceof NameExpression)||(n instanceof ClassType);
  }
  
  @Override
  public void visit(Dereference e){
    if (simple_expression(e.object())) {
      super.visit(e);
    } else {
      ASTNode obj = add_as_var(e.object());
      result = create.dereference(obj, e.field());
    }
  }
}
