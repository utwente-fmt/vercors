package vct.col.util;

import vct.col.ast.expr.OperatorExpression;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.util.RecursiveVisitor;

public class WandScanner extends RecursiveVisitor<Object> {

	public WandScanner(ProgramUnit source) {
		super(source);
	}

	public void visit(OperatorExpression e){
		switch(e.operator()){
		case Wand:{
			Warning("found magic wand.");
			break;
		}
		default:
			super.visit(e);
		}
	}
}
