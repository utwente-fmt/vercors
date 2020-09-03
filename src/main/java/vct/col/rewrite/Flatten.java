package vct.col.rewrite;

import hre.ast.MessageOrigin;

import java.util.*;

import vct.col.ast.expr.*;
import vct.col.ast.expr.constant.ConstantExpression;
import vct.col.ast.expr.constant.StructValue;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.*;
import vct.col.ast.stmt.decl.*;
import vct.col.ast.stmt.terminal.AssignmentStatement;
import vct.col.ast.stmt.terminal.ReturnStatement;
import vct.col.ast.type.ClassType;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.type.Type;
import vct.col.ast.util.AbstractRewriter;
import hre.util.LambdaHelper;

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

  /**
   * Whenever the second value on the stack is true, the flattener is not inside an expression.
   * This allows each rewrite step to detect if they are the toplevel expression
   * or nested within an expression (and hence if they should be rewritten
   * into a variable or not)
   *
   * The top of the LevelStack is false by default s.t. everything is moved into a variable by default.
   * Specific statements (assign, return) can set it to true so the first expression (with markAsTopLevel)
   * encountered after that point is not moved out, but the second expression after it is.
    */
    private LinkedList<Boolean> levelStack = new LinkedList<>();

  @Override
  public void pre_visit(ASTNode n) {
    super.pre_visit(n);
    levelStack.push(false);
  }

  @Override
  public void post_visit(ASTNode n) {
    levelStack.pop();
    super.post_visit(n);
  }

  public void markAsTopLevel() {
    this.levelStack.pop();
    this.levelStack.push(true);
  }

  /**
   * New level values get pushed in front of the stack, so to get the second to last value we use index 1 on the stack
   */
  public boolean isInTopLevel() {
    return this.levelStack.get(1);
  }

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
    Debug("call to %s",e.method());
    ASTNode object=rewrite(e.object());
    int N=e.getArity();
    ASTNode args[]=new ASTNode[N];
    for(int i=0;i<N;i++){
      args[i]=e.getArg(i).apply(this);
    }
    String name="__flatten_"+(++counter);
    if (e.getType()==null){
      Abort("result type of call unknown at %s",e.getOrigin());
    }
    if (e.getType().isVoid()||e.getType().isNull()||declaration_block==null || isInTopLevel()){
      result=create.invokation(object,rewrite(e.dispatch()),e.method(),args);
      ((MethodInvokation)result).set_before(copy_rw.rewrite(e.get_before()));
      ((MethodInvokation)result).set_after(copy_rw.rewrite(e.get_after()));
    } else {
      Debug("declaring variable %s (%s)",name,e.getType());
      ASTNode n=create.field_decl(name,e.getType());
      Debug("inserting in %s",declaration_block);
      declaration_block.addStatement(n);
      Debug("assigning result of call");
      MethodInvokation call=create.invokation(object,rewrite(e.dispatch()),e.method(),args);
      call.set_before(copy_pure.rewrite(e.get_before()));
      call.set_after(copy_pure.rewrite(e.get_after()));
      for(NameExpression lbl:e.getLabels()){
        Debug("FLATTEN: copying label %s",lbl);
        call.addLabel(lbl);
      }
      current_block.addStatement(create.assignment(create.local_name(name),call));
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

      current_block.addStatement(
              create.assignment(loc_res, create.expression(StandardOperator.Plus, loc_res, val_res))
      );
      result = copy_rw.rewrite(loc_res);
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
        declaration_block.addStatement(create.field_decl(name,e.getType(),null));
      }
      ASTNode effect=create.assignment(arg_out,create.expression(op,arg_out,create.constant(1)));
      if (expression){
        current_block.addStatement(effect);
        current_block.addStatement(create.assignment(create.local_name(name),arg_out));
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
      declaration_block.addStatement(create.field_decl(name,e.getType(),null));
      current_block.addStatement(create.assignment(create.local_name(name),arg_out));
      current_block.addStatement(create.assignment(arg_out,create.expression(op,arg_out,create.constant(1))));
      result=create.local_name(name);
      return;
    }
    default:
      super.visit(e);
      return;
    }
  }

  @Override
  public void visit(VectorBlock vectorBlock) {
    result = create.vector_block(
            copy_pure.rewrite(vectorBlock.iter()),
            rewrite(vectorBlock.block())
    );
  }

  @Override
  public void visit(ParallelBlock pb){
    ParallelBlock res=create.parallel_block(
            pb.label(),
            rewrite(pb.contract()),
            copy_pure.rewrite(pb.itersJava()),
            rewrite(pb.block()),
            rewrite(pb.deps())
    );
    result=res;
  }

  @Override
  public void visit(ActionBlock actionBlock) {
    Map<String,ASTNode> map = new HashMap<String,ASTNode>();
    actionBlock.foreach(LambdaHelper.fun((key, val) -> map.put(key, rewrite(val))));

    // rewrite all other components of `actionBlock`
    result = create.action_block(
            rewrite(actionBlock.history()),
            rewrite(actionBlock.fraction()),
            rewrite(actionBlock.process()),
            copy_pure.rewrite(actionBlock.action()),
            map,
            rewrite(actionBlock.block())
    );
  }
  
  public void visit(DeclarationStatement s){
    markAsTopLevel();
    Type t=s.getType();
    ASTNode tmp=t.apply(this);
    if (tmp instanceof Type){
      t=(Type)tmp;
    } else {
      throw new Error("type AST rewrote to non-type AST");
    }
    String name = s.name();
    ASTNode init = s.initJava();
    if (init!=null) {
      if (current_block==null){
        Abort("internal error: current block is null");
      }
      current_block.addStatement(create.field_decl(name,t,null));
      init=init.apply(this);
      result=create.assignment(create.local_name(name),init);
    } else {
      result=create.field_decl(name,t,null);
    }
  }
  
  @Override
  public void visit(AssignmentStatement s) {
    markAsTopLevel();
    ASTNode loc=s.location();
    ASTNode val=s.expression();
    if (loc instanceof Dereference
    && !val.getType().equals(ClassType.nullType())
    && !val.getType().equals(ClassType.labelType())){
      loc=rewrite(loc);
      val=add_as_var(val);
    } else {
      loc=rewrite(loc);
      val.accept(this);
      val = this.getResult();
    }
    result=create.assignment(loc,val);
  }

  private ASTNode add_as_var(ASTNode e){
    if(e instanceof NameExpression || e instanceof ConstantExpression) {
      // NameExpression also includes e.g. null
      return e;
    }

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
    declaration_block.addStatement(n);
    ASTNode ee=e.apply(this);
    current_block.addStatement(create.assignment(create.local_name(name),ee));
    ASTNode tmp=create.local_name(name);
    create.leave();
    return tmp;
  }

  public void visit(ReturnStatement s){
    markAsTopLevel();
    ASTNode e=s.getExpression();
    if (e!=null){
      e = e.apply(this);
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
        visit_body(s.getStatement(i));
      }
    } else {
      ASTNode statement = body.apply(this);
      if(!(statement instanceof NameExpression)) {
        /* invokations of methods that return something are flattened, but we want to ignore this value when the
         method is instead used as a statement.*/
        current_block.addStatement(statement);
      }
    }
  }

  private ASTNode visit(IfStatement s, int i) {
    if(s.getGuard(i) == IfStatement.elseGuard()) {
      return rewrite(s.getStatement(i));
    } else {
      IfStatement result = new IfStatement();
      ASTNode guard = rewrite(s.getGuard(i));

      block_stack.push(current_block);
      current_block = create.block();
      visit_body(s.getStatement(i));
      result.addClause(guard, current_block);
      current_block = block_stack.pop();

      if(i < s.getCount()-1) {
        block_stack.push(current_block);
        current_block = create.block();
        current_block.addStatement(visit(s, i+1));
        result.addClause(IfStatement.elseGuard(), current_block);
        current_block = block_stack.pop();
      }

      return result;
    }
  }
  
  public void visit(IfStatement s) {
    result = visit(s, 0);
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
    Method res=create.method_kind(kind, rewrite(m.getReturnType()), rewrite(m.signals), c, name, args, m.usesVarArgs(),null);
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

  public void visit(BindingExpression expr) {
    result = copy_rw.rewrite(expr);
  }

  public ASTNode transformStructValue(Type t, StructValue v) {
    String name = "__flatten_" + (++counter);
    declaration_block.addStatement(create.field_decl(name, t));

    if(t.isPrimitive(PrimitiveSort.Option)) {
      current_block.addStatement(create.assignment(
              create.local_name(name),
              create.expression(StandardOperator.OptionSome, transformStructValue((Type) t.firstarg(), v))
      ));
    } else if(t.isPrimitive(PrimitiveSort.Array)) {
      Type arg = (Type) t.firstarg();

      current_block.addStatement(create.assignment(
              create.local_name(name),
              create.invokation(null, null, RewriteArrayRef.getArrayConstructor(t, 1), constant(v.valuesLength()))
      ));

      boolean derefItem = false;

      if(arg.isPrimitive(PrimitiveSort.Cell)) {
        arg = (Type) arg.firstarg();
        derefItem = true;
      }

      for(int i = 0; i < v.valuesLength(); i++) {
        ASTNode target = create.expression(StandardOperator.Subscript, create.local_name(name), constant(i));
        if(derefItem) target = create.dereference(target, "item");
        current_block.addStatement(create.assignment(target, rewrite(v.value(i))));
      }
    } else if(t.isPrimitive(PrimitiveSort.Sequence) || t.isPrimitive(PrimitiveSort.Set) || t.isPrimitive(PrimitiveSort.Bag) || t.isPrimitive(PrimitiveSort.Map) || t.isPrimitive(PrimitiveSort.Tuple)) {
        // The SilverExpressionMap has separate constructs for explicit seq, set & bag expressions, so we do not rewrite
        // it here.
        current_block.addStatement(create.assignment(
                create.local_name(name),
                v
        ));
    } else {
      Fail("Don't know how to assign a StructValue to %s", t);
    }

    return create.local_name(name);
  }

  public void visit(StructValue s) {
    result = transformStructValue(s.getType(), s);
  }
  
  @Override
  public void visit(LoopStatement s) {
    if(s.getInitBlock() != null) {
      // Flatten the initialization block in the current block: declarations may not overwrite variables in the
      // surrounding block.
      visit_body(s.getInitBlock());
    }

    LoopStatement result = new LoopStatement();

    result.setEntryGuard(copy_pure.rewrite(s.getEntryGuard()));
    result.setExitGuard(copy_pure.rewrite(s.getExitGuard()));

    // Set up a dummy body to be flattened
    BlockStatement dummyBody = create.block();
    dummyBody.addStatement(s.getBody());

    if(s.getUpdateBlock() != null) {
      dummyBody.addStatement(s.getUpdateBlock());
    }

    result.setBody(rewrite(dummyBody));

    result.setOrigin(s.getOrigin());

    result.set_before(copy_pure.rewrite(s.get_before()));
    result.set_after(copy_pure.rewrite(s.get_after()));
    result.appendContract(copy_pure.rewrite(s.getContract()));

    this.result = result;
  }

  private boolean simple_expression(ASTNode n){
    return (n instanceof NameExpression)||(n instanceof ClassType);
  }
  
  @Override
  public void visit(Dereference e){
    if (simple_expression(e.obj())) {
      super.visit(e);
    } else {
      ASTNode obj = add_as_var(e.obj());
      result = create.dereference(obj, e.field());
    }
  }
}
