package vct.col.ast.print;

import java.io.PrintWriter;

import vct.col.ast.expr.NameExpression;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.composite.IfStatement;
import vct.col.ast.stmt.composite.LoopStatement;
import vct.col.ast.stmt.decl.*;
import vct.col.ast.stmt.terminal.AssignmentStatement;
import vct.col.ast.stmt.terminal.ReturnStatement;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.type.PrimitiveType;
import vct.col.ast.type.Type;
import vct.col.ast.syntax.CSyntax;
import hre.ast.TrackingOutput;
import hre.ast.TrackingTree;

import static hre.lang.System.DebugException;

public class CPrinter extends AbstractPrinter {

	public CPrinter(TrackingOutput out) {
		super(CSyntax.getCML(), out);
	}

	public void pre_visit(ASTNode node) {
		super.pre_visit(node);
		for (NameExpression lbl : node.getLabels()) {
			nextExpr();
			lbl.accept(this);
			out.printf(":");
			// out.printf("[");
		}
	}
	
	public void post_visit(ASTNode node) {
	  super.post_visit(node);
	}

	@Override
	public void visit(PrimitiveType t){
	  switch(t.sort){
	  case Pointer:
	    t.firstarg().accept(this);
	    out.printf("*");
	    break;
	  default:
	    super.visit(t);
	    break;
	  }
	}
	
	public void visit(ASTClass cl){
		//if (cl.getDynamicCount()>0) {
		//	Fail("dynamic entries are illegal in C");
		//}
    for(ASTNode item:cl.dynamicMembers()){
      item.accept(this);
    }
    for(ASTNode item:cl.staticMembers()){
      item.accept(this);
    }
	}
	
	public void visit(DeclarationStatement decl){
	  Type t=decl.getType();
	  if (t.isPrimitive(PrimitiveSort.CVarArgs)){
	    out.printf("...");
	  } else {
  	  t.accept(this);
  		out.printf(" %s", decl.name());
  		if (decl.initJava() != null) {
  			out.printf("=");
  			nextExpr();
  			decl.initJava().accept(this);
  		}
	  }
		if (current_method()==null) {
			out.printf(";%n%n");
		}
	}
	
	@Override
	public void visit(Contract c){
	  out.println("/*@");
	  out.incrIndent();
    out.print("requires ");
    c.pre_condition.accept(this);
    out.println(";");
    out.print("ensures ");
    c.post_condition.accept(this);
    out.println(";");
	  out.decrIndent();
	  out.println("@*/");
	}
	public void visit(Method m){
	  Contract c=m.getContract();
	  if (c!=null) visit(c); //c.accept(this);
		nextExpr();
		m.getReturnType().accept(this);
		out.printf(" %s(",m.getName());
		String sep="";
		for(DeclarationStatement decl:m.getArgs()){
			out.printf("%s",sep);
			sep=",";
			nextExpr();
			decl.accept(this);
		}
		out.printf(")");
		if (m.getBody()!=null){
			m.getBody().accept(this);
			out.printf("%n%n");
		} else {
			out.printf(";%n%n");
		}
		
	}
	  public void visit(IfStatement s){
		      int N=s.getCount();
		      out.printf("if (");
		      nextExpr();
		      s.getGuard(0).accept(this);
		      out.printf(") ");
		      s.getStatement(0).accept(this);
		      for(int i=1;i<N;i++){
		        if (i==N-1 && s.getGuard(i)==IfStatement.elseGuard()){
		          out.printf(" else ");
		        } else {
		          out.printf(" else if (");
		          nextExpr();
		          s.getGuard(i).accept(this);
		          out.printf(")");
		        }
		        s.getStatement(i).accept(this);
		      }
		      out.lnprintf("");
		    }

	public void visit(AssignmentStatement s){
		nextExpr();
		s.location().accept(this);
		out.printf("=");
		nextExpr();
		s.expression().accept(this);
		if (in_expr){
			out.printf(";%n");
		}
	}
	
	@Override
	public void visit(LoopStatement l){
	  if (l.getInitBlock()==null){
	    out.printf("while(");
	    nextExpr();
	    l.getEntryGuard().accept(this);
	    out.printf(")");
	    l.getBody().accept(this);
	  } else {
	    super.visit(l);
	  }
	}
	
/*
 	public void visit(ConstantExpression ce){
    if (ce.value instanceof StringValue){
      out.print("\""+org.apache.commons.lang3.StringEscapeUtils.escapeJava(ce.toString())+"\"");
    } else {
      out.print(ce.toString());
    }
  }
*/
	
	public void visit(ReturnStatement s){
		if (s.getExpression()==null){
			out.printf("return");
		} else {
			out.printf("return ");
			nextExpr();
			s.getExpression().accept(this);
		}		
	}
	public void visit(BlockStatement block) {
    boolean block_expr=in_expr;
    if (block_expr) {
    	out.printf("{");
    } else {
    	out.lnprintf("{");
    	out.incrIndent();
    }
    int N=block.getLength();
    for(int i=0;i<N;i++){
      ASTNode S=block.getStatement(i);
      S.accept(this);
      if (block_expr) out.printf(";"); else out.lnprintf(";");
    }
    if (!block_expr) {
    	out.decrIndent();
    }
    out.printf("}");
	}

	public static TrackingTree dump_expr(PrintWriter out, ASTNode node) {
		TrackingOutput track_out = new TrackingOutput(out, false);
		CPrinter printer = new CPrinter(track_out);
		printer.setExpr();
		node.accept(printer);
		return track_out.close();
	}

	public static TrackingTree dump(PrintWriter out, ProgramUnit program) {
		hre.lang.System.Debug("Dumping C code...");
		try {
			TrackingOutput track_out = new TrackingOutput(out, false);
			CPrinter printer = new CPrinter(track_out);
			for (ASTDeclaration cu: program.get()) {
				cu.accept(printer);
			}
			return track_out.close();
		} catch (Exception e) {
			DebugException(e);
			throw new Error("abort");
		}
	}

	public static void dump(PrintWriter out, ASTNode cl) {
		TrackingOutput track_out = new TrackingOutput(out, false);
		CPrinter printer = new CPrinter(track_out);
		cl.accept(printer);
		track_out.close();
	}

}
