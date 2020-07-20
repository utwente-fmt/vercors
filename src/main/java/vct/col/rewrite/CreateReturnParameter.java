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
    /* TODO (Bob): In the case of abrupt control flow this is not that useful, since
          there is only one return at the end now. Ideally if no finally is present, the contracts
          should be checked at the actual return sites, and not just the synthetic return site at the end.
     */
    map.add(RETURN_BRANCH,ErrorCode.AssertFailed,ErrorCode.PostConditionFailed);
  }
  
  public void visit(NameExpression e){
    if (e.isReserved(ASTReserved.Result)){
      result=create.unresolved_name(RETURN_VAR);
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
          rewrite(m.getContract()),
          m.getName(),
          args,
          rewrite(m.getBody()));
    }
  }
  
  public void visit(ReturnStatement s){
    ASTNode expr=s.getExpression();
    BlockStatement res=create.block();
    if (expr!=null){
      if (expr instanceof MethodInvokation) {
        MethodInvokation method_invokation = (MethodInvokation) expr;
        Method m = method_invokation.getDefinition();
        Objects.requireNonNull(m, () -> String.format("cannot process invokation of %s without definition", method_invokation.method()));
        if (m.kind == Method.Kind.Plain) {
          res.add(invokation_into_variable(method_invokation, create.local_name("sys__result")));
        }
      } else {
        res.add(create.assignment(create.local_name("sys__result"),rewrite(expr)));
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
        MethodInvokation method_invokation = (MethodInvokation) s.expression();
        Method m = method_invokation.getDefinition();
        Objects.requireNonNull(m, () -> String.format("cannot process invokation of %s without definition", method_invokation.method()));
        if (m.kind == Method.Kind.Plain) {
          result = invokation_into_variable(method_invokation, s.location());
          return;
        }
    }
    super.visit(s);
  }

  /**
   * Turns a method invokation with a location into a method invokation that assigns the result to the first parameter.
   * Only works for plain methods, throws otherwise. Applies rewrite to both the method invokation and its constituents, and the location.
   */
  private MethodInvokation invokation_into_variable(MethodInvokation method_invokation, ASTNode location) {
    Method method=method_invokation.getDefinition();
    Objects.requireNonNull(method, () -> String.format("cannot process invokation of %s without definition", method_invokation.method()));
    if (method.kind != Method.Kind.Plain) {
      Abort("MethodInvokation is not plain");
    }
    int N=method_invokation.getArity();
    ASTNode args[]=new ASTNode[N+1];
    args[0]=rewrite(location);
    for(int i=0;i<N;i++){
      args[i+1]=rewrite(method_invokation.getArg(i));
    }
    args[0]=rewrite(location);
    MethodInvokation res=create.invokation(rewrite(method_invokation.object()), rewrite(method_invokation.dispatch()) , method_invokation.method() , args );
    for(NameExpression lbl:method_invokation.getLabels()){
      Debug("VOIDCALLS: copying label %s",lbl);
      res.addLabel(rewrite(lbl));
    }
    res.set_before(rewrite(method_invokation.get_before()));
    HashMap<NameExpression,ASTNode> map=new HashMap<NameExpression,ASTNode>();
    map.put(create.reserved_name(ASTReserved.Result),rewrite(location));
    Substitution subst=new Substitution(source(),map);
    res.set_after(subst.rewrite(method_invokation.get_after()));
    return res;
  }
}
