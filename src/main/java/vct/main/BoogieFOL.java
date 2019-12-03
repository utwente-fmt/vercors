package vct.main;

import hre.debug.HeapDump;
import hre.io.PrefixPrintWriter;
import hre.util.CompositeReport;
import hre.util.TestReport;
import vct.boogie.BoogieReport;
import vct.col.ast.stmt.decl.ASTClass;
import vct.col.ast.stmt.decl.ASTClass.ClassKind;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.ASTSpecial.Kind;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.decl.Contract;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.expr.OperatorExpression;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.util.RecursiveVisitor;
import vct.col.ast.expr.StandardOperator;
import vct.col.rewrite.AbstractRewriter;
import vct.col.util.ASTFactory;
import vct.col.util.ASTUtils;

import java.io.PrintWriter;

/**
 * This class scans a given AST for assertions and checks each assertion as if it were
 * a First Order Logic formula using Boogie.
 * 
 * @author Stefan Blom
 *
 */
public class BoogieFOL {

  /**
   * This class extends the abstract scanner to scan for methods.
   * It will then scan those methods for assertions.
   *  vct --passes="resolv,boogie-fol" BoogieTemp.java 
   *
   * @author Stefan Blom
   *
   */
  private static class MethodFinder extends RecursiveVisitor<Object> {
    
    public MethodFinder(){
      super(null,null);
    }
    
    CompositeReport report=new CompositeReport();
    
    /** 
     * Executed when the abstract scanner finds a method.
     */
    public void visit(Method m){
      PrintWriter infoLog = hre.lang.System.getLogLevelOutputWriter(hre.lang.System.LogLevel.Info);
      PrefixPrintWriter out=new PrefixPrintWriter(infoLog);
      PrintWriter err = hre.lang.System.getLogLevelErrorWriter(hre.lang.System.LogLevel.Info);
      ASTNode body=m.getBody();
      Contract c=m.getContract();
      out.printf("starting%n");
      if (c==null) Fail("method %s has no contract",m.getName());
      out.printf("begin precondition%n");
      HeapDump.tree_dump(out,c.pre_condition,ASTNode.class);
      out.printf("end precondition%n");
      if (body instanceof BlockStatement){
        /* In Java the body of a method always is a block.
         * However, the AST also allows expressions as Method bodies.
         */
        BlockStatement block=(BlockStatement)body;
        int N=block.getLength();
        for(int i=0;i<N;i++){
          out.printf("========%d=======%n",i);
          HeapDump.tree_dump(out,block.getStatement(i),ASTNode.class);
          out.printf("end block%n");
          if (block.getStatement(i) instanceof OperatorExpression){
            OperatorExpression e=(OperatorExpression)block.getStatement(i);
            if (e.isSpecial(Kind.Assert)) continue;
            DeclarationStatement args[]=m.getArgs();
            ASTNode formula=e.arg(0);
            err.printf("checking formula at %s%n",formula.getOrigin());
            vct.util.Configuration.getDiagSyntax().print(new PrintWriter(infoLog),formula);
            for(ASTNode part:ASTUtils.conjuncts(formula,StandardOperator.And)){
              err.print("conjuct: ");
              vct.util.Configuration.getDiagSyntax().print(new PrintWriter(infoLog),part);
            }
            BoogieReport res=check_boogie(args,formula);
            err.printf("formula at %s: %s%n",e.getOrigin(),res.getVerdict());
            report.addReport(res);
            for(ASTNode part:ASTUtils.conjuncts(formula,StandardOperator.And)){
              err.print("conjuct ");
              vct.util.Configuration.getDiagSyntax().print(new PrintWriter(infoLog),part);
              err.println();
              res=check_boogie(args,part);
            }
          }
        }
      } else {
        err.printf("skipping non-block body of method %s at %s%n",m.getName(),m.getOrigin());
      }
      out.printf("begin precondition2%n");
      HeapDump.tree_dump(out,c.pre_condition,ASTNode.class);
      out.printf("end precondition2%n");
      out.close();
      err.close();
    }
  }

  /** find all assertions in the given program and check them 
   * with Boogie as if they were first order logic formulas.
   * @param program The program to scan for assertions.
   * @return The result of checking assertions in the form of a test report.
   */
  public static TestReport main(ASTClass program) {
    MethodFinder finder=new MethodFinder();
    program.accept(finder);
    return finder.report;
  }


  /**
   * Check a formula using Boogie.
   * @param args The variables used in the formula. These are implicitly universally quantified.
   * @param formula The formula to be checked.
   */
  public static BoogieReport check_boogie(DeclarationStatement args[],ASTNode formula){
    ASTFactory<?> create=new ASTFactory<Object>();
    create.setOrigin(formula.getOrigin());
    
    AbstractRewriter copy_rw=new AbstractRewriter((ProgramUnit)null);
    
    ASTClass program=create.ast_class("dummy",ClassKind.Plain,null,null,null);
    for (ASTNode n:args){
      program.add_static(n.apply(copy_rw));
    }
    ASTNode assertion=create.special(Kind.Assert,formula.apply(copy_rw));
    BlockStatement body=create.block(assertion);
    program.add_static(create.method_decl(
        create.primitive_type(PrimitiveSort.Void),
        null,
        "formula",
        new DeclarationStatement[0],
        body));
    //hre.debug.HeapDump.tree_dump(new hre.io.PrefixPrintWriter(err),program,ASTNode.class);
    ProgramUnit pgm=new ProgramUnit();
    pgm.add(program);
    return vct.boogie.Main.TestBoogie(pgm);
  }
}
