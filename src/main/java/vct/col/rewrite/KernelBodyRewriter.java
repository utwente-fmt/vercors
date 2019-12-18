package vct.col.rewrite;

import java.util.ArrayList;

import vct.col.ast.expr.Dereference;
import vct.col.ast.expr.OperatorExpression;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.decl.Contract;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.util.ContractBuilder;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.type.Type;
import vct.col.util.ASTUtils;

class KernelBodyRewriter extends AbstractRewriter {

  public KernelBodyRewriter(ProgramUnit source) {
    super(source);
  }

  @Override
  public void visit(MethodInvokation e) {
    ASTNode arg;
    switch (e.method) {
      case "get_global_id":
        arg = e.getArg(0);
        if (arg.isConstant(0)) {
          //result=create.local_name("opencl_tid");
          result = plus(mult(create.local_name("opencl_gid"), create.local_name("opencl_gsize")),
                  create.local_name("opencl_lid"));
        } else {
          Fail("bad dimension: %s", arg);
        }
        break;
      case "get_local_id":
        arg = e.getArg(0);
        if(arg.isConstant(0)) {
          result = create.local_name("opencl_lid");
        } else {
          Fail("bad dimension: %s", arg);
        }
        break;
      default:
        super.visit(e);
    }

  }
  
  @Override
  public void visit(Method m){
    ArrayList<DeclarationStatement> decls=new ArrayList<DeclarationStatement>();
    DeclarationStatement inner_decl=create.field_decl(
        "opencl_lid",create.primitive_type(PrimitiveSort.Integer),
        create.expression(StandardOperator.RangeSeq,
            create.constant(0),create.local_name("opencl_gsize")));
    DeclarationStatement outer_decl=create.field_decl(
        "opencl_gid",create.primitive_type(PrimitiveSort.Integer),
        create.expression(StandardOperator.RangeSeq,
            create.constant(0),create.local_name("opencl_gcount")));
    ContractBuilder icb=new ContractBuilder(); // thread contract
    ContractBuilder gcb=new ContractBuilder(); // group contract
    gcb.requires(create.constant(true));
    ContractBuilder kcb=new ContractBuilder(); // kernel contract
    kcb.given(create.field_decl("opencl_gcount",create.primitive_type(PrimitiveSort.Integer)));
    kcb.given(create.field_decl("opencl_gsize",create.primitive_type(PrimitiveSort.Integer)));
    Type returns=rewrite(m.getReturnType());
    for(DeclarationStatement d:m.getArgs()){
      decls.add(rewrite(d));
    }
    Contract c=m.getContract();
    rewrite(c,icb);
    gcb.appendInvariant(rewrite(c.invariant));
    kcb.appendInvariant(rewrite(c.invariant));
    Contract ic=rewrite(m.getContract());
    for(ASTNode clause:ASTUtils.conjuncts(ic.pre_condition, StandardOperator.Star)){
      ASTNode group=create.starall(
          create.expression(StandardOperator.Member,
              create.local_name("opencl_lid"),
              create.expression(StandardOperator.RangeSeq,
                  create.constant(0),create.local_name("opencl_gsize"))
          ),
          clause,
          create.field_decl("opencl_lid",create.primitive_type(PrimitiveSort.Integer)));
      gcb.requires(group);
      kcb.requires(create.starall(
          create.expression(StandardOperator.Member,
              create.local_name("opencl_gid"),
              create.expression(StandardOperator.RangeSeq,
                  create.constant(0),create.local_name("opencl_gcount"))
          ),
          group,
          create.field_decl("opencl_gid",create.primitive_type(PrimitiveSort.Integer))));
    }
    BlockStatement body=(BlockStatement)rewrite(m.getBody());
    //body.prepend(create.field_decl("opencl_tid",create.primitive_type(Sort.Integer),
    //    plus(mult(create.local_name("opencl_gid"),create.local_name("opencl_gsize")),create.local_name("opencl_lid"))));
    //icb.given(create.field_decl("opencl_tid",create.primitive_type(Sort.Integer)));
    //icb.requires(create.expression(StandardOperator.EQ,
    //    create.local_name("opencl_tid"),plus(mult(create.local_name("opencl_gid"),create.local_name("opencl_gsize")),create.local_name("opencl_lid"))));
    DeclarationStatement[] iters=new DeclarationStatement[]{inner_decl};
    body=create.block(create.region(null,create.parallel_block("group_block", icb.getContract(),iters, body)));
    iters=new DeclarationStatement[]{outer_decl};
    body=create.block(create.region(null,create.parallel_block("kernel_block", gcb.getContract(),iters, body)));
    result=create.method_decl(returns, kcb.getContract(), m.name(), decls, body);
  }

  @Override
  public void visit(OperatorExpression e) {
    switch(e.operator()) {
      case StructSelect:
        if(e.arg(1).isName("x")) {
          if(e.arg(0).isName("threadIdx")) {
            result = name("opencl_lid");
          } else if(e.arg(0).isName("blockIdx")) {
            result = name("opencl_gid");
          } else if(e.arg(0).isName("blockDim")) {
            result = name("opencl_gsize");
          } else {
            super.visit(e);
          }
        } else {
          super.visit(e);
        }
        break;
      default:
        super.visit(e);
    }
  }
}