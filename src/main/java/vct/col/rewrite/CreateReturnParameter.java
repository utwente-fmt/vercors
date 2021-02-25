package vct.col.rewrite;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Objects;

import vct.col.ast.stmt.decl.ASTFlags;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.type.ASTReserved;
import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.stmt.terminal.AssignmentStatement;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.expr.NameExpression;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.stmt.terminal.ReturnStatement;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.util.AbstractRewriter;
import vct.logging.ErrorMapping;
import vct.logging.VerCorsError.ErrorCode;

public class CreateReturnParameter extends AbstractRewriter {

  private static final String RETURN_BRANCH = "return branch";
  public static final String RETURN_VAR = "sys__result";

  public CreateReturnParameter(ProgramUnit source, ErrorMapping map) {
    super(source);
    map.add(RETURN_BRANCH,ErrorCode.AssertFailed,ErrorCode.PostConditionFailed);
  }
  
  public void visit(NameExpression e){
    if (e.isReserved(ASTReserved.Result)){
      result=create.local_name(RETURN_VAR);
    } else {
      super.visit(e);
    }
  }
  
  public void visit(Method m){
    switch(m.kind){
    case Predicate:
    case Pure:
      result=copy_rw.rewrite(m);
      return;
    default:
      break;
    }   
    if (m.getReturnType().isVoid()){
      super.visit(m);
    } else {
      ArrayList<DeclarationStatement> args = new ArrayList<>(Arrays.asList(m.getArgs()));
      for(int i=0; i<args.size(); i++){
        args.set(i, rewrite(args.get(i)));
      }

      DeclarationStatement outArg = create.field_decl(RETURN_VAR, rewrite(m.getReturnType()));
      outArg.setFlag(ASTFlags.OUT_ARG, true);
      args.add(0, outArg);

      result=create.method_decl(
          create.primitive_type(PrimitiveSort.Void),
          rewrite(m.signals),
          rewrite(m.getContract()),
          m.getName(),
          args.toArray(new DeclarationStatement[0]),
          rewrite(m.getBody()));
    }
  }

  private String requireDefinitionError(MethodInvokation methodInvokation) {
    return String.format("cannot process invokation of %s without definition", methodInvokation.method());
  }
  
  public void visit(ReturnStatement s){
    ASTNode expr=s.getExpression();
    BlockStatement res=create.block();
    if (expr!=null){
      if (expr instanceof MethodInvokation) {
        MethodInvokation methodInvokation = (MethodInvokation) expr;
        Method m = methodInvokation.getDefinition();
        Objects.requireNonNull(m, () -> requireDefinitionError(methodInvokation));
        if (m.kind == Method.Kind.Plain) {
          res.add(invokationIntoVariable(methodInvokation, create.local_name(RETURN_VAR)));
        }
      } else {
        res.add(create.assignment(create.local_name(RETURN_VAR),rewrite(expr)));
      }
    }
    for(ASTNode n : s.get_after()){
      res.add(rewrite(n));
    }

    ASTNode post=rewrite(current_method().getContract().post_condition);
    if (current_method().getContract()!=null){
      res.add(create.special(ASTSpecial.Kind.Assert,post).set_branch(RETURN_BRANCH));
    }
    res.add(create.special(ASTSpecial.Kind.Assume,create.constant(false)));
    result=res;
  }
  
  public void visit(MethodInvokation e){
    Method m=e.getDefinition();
    Objects.requireNonNull(m, () -> String.format("unexpected null method definition at %s", e.getOrigin()));
    switch(m.kind){
    case Predicate:
    case Pure:
      super.visit(e);
      return;
    default:
      break;
    }
    if (!m.getReturnType().isVoid()){
      Fail("unexpected invokation of non-void method %s at %s",e.method(),e.getOrigin());
    }
    super.visit(e);
  }
  
  public void visit(AssignmentStatement s){
    if (s.expression() instanceof MethodInvokation){
        MethodInvokation methodInvokation = (MethodInvokation) s.expression();
        Method m = methodInvokation.getDefinition();
        Objects.requireNonNull(m, () -> requireDefinitionError(methodInvokation));
        if (m.kind == Method.Kind.Plain) {
          result = invokationIntoVariable(methodInvokation, s.location());
          return;
        }
    }
    super.visit(s);
  }

  /**
   * Turns a method invokation with a location into a method invokation that assigns the result to the first parameter.
   * Only works for plain methods, throws otherwise. Applies rewrite to both the method invokation and its constituents, and the location.
   */
  private MethodInvokation invokationIntoVariable(MethodInvokation methodInvokation, ASTNode location) {
    Method method=methodInvokation.getDefinition();
    Objects.requireNonNull(method, () -> requireDefinitionError(methodInvokation));
    if (method.kind != Method.Kind.Plain) {
      Abort("MethodInvokation is not plain");
    }
    int numArgs = methodInvokation.getArity();
    ASTNode[] args = new ASTNode[numArgs+1];
    args[0]=rewrite(location);
    for(int i=0;i<numArgs;i++){
      args[i+1]=rewrite(methodInvokation.getArg(i));
    }
    args[0]=rewrite(location);
    MethodInvokation res=create.invokation(rewrite(methodInvokation.object()), rewrite(methodInvokation.dispatch()) , methodInvokation.method() , args );
    for(NameExpression lbl:methodInvokation.getLabels()){
      Debug("CreateReturnParameter: copying label %s",lbl);
      res.addLabel(rewrite(lbl));
    }
    res.set_before(rewrite(methodInvokation.get_before()));
    HashMap<NameExpression,ASTNode> map=new HashMap<NameExpression,ASTNode>();
    map.put(create.reserved_name(ASTReserved.Result),rewrite(location));
    Substitution subst=new Substitution(source(),map);
    res.set_after(subst.rewrite(methodInvokation.get_after()));
    return res;
  }
}
