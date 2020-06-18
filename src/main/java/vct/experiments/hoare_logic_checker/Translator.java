package vct.experiments.hoare_logic_checker;

import hre.ast.FileOrigin;
import hre.ast.Origin;
import hre.io.PrefixPrintWriter;

import java.io.PrintWriter;
import java.lang.reflect.Field;
import java.util.*;

import vct.col.ast.stmt.composite.IfStatement;
import vct.col.ast.stmt.decl.ASTClass;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.ASTSpecial.Kind;
import vct.col.ast.type.*;
import vct.col.ast.util.AbstractVisitor;
import vct.col.ast.stmt.terminal.AssignmentStatement;
import vct.col.ast.expr.BindingExpression;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.expr.constant.ConstantExpression;
import vct.col.ast.stmt.decl.Contract;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.stmt.composite.LoopStatement;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.expr.NameExpression;
import vct.col.ast.expr.OperatorExpression;
import vct.col.ast.util.RecursiveVisitor;
import vct.col.ast.stmt.terminal.ReturnStatement;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.expr.StandardProcedure;
import vct.col.rewrite.Substitution;
import vct.col.ast.util.ASTFactory;
import vct.col.ast.stmt.decl.ASTSpecial;

/**
 * @author Remco Swenker
 * this class delivers the AST to the Brain of the Hoare Logic Checker in bite sized chunks.
 * bin\vct "--passes=assign,boogie-fol" remco\Test.java
 */
@SuppressWarnings("all") // Because this code is not actively maintained.
public class Translator {
	
	private ASTClass theTree;
	private static Brain thisParent;
	private List<String> variablelenLijst;
	private List<String> hoareTriple;
	
  private PrefixPrintWriter outputToString;
	private int currentWorkingTriple;
	public boolean abort=false;
	private int currentSet = 0;
	private int lastHoarePredicate = 0;
	
	
	public Translator(ASTClass programme, Brain parent){
		GeneratingZ3LogicFinder finder=new GeneratingZ3LogicFinder();
	    theTree = programme;
	    thisParent= parent;
	    theTree.accept(finder);
	    //variablelenLijst = new String[2];
	    //MethodFinder method = new MethodFinder();
	    //theTree.accept(method);
	    //StringPrinter visitor = new StringPrinter();
	    //theTree.accept(visitor);
	}
	private class GeneratingZ3LogicFinder extends RecursiveVisitor<Object> {
	  
	  public GeneratingZ3LogicFinder(){
	    super(null,null);
	  }
		public void visit(Method m){
			boolean ans = startGeneratingZ3Logic(m);
		}
	}
	
	public boolean generateLogic(ASTNode pre, ASTNode state, ASTNode post, String logic_type){
		boolean ans = false;
		if(logic_type.equals("Z3")){
			ans = generateZ3Logic(pre,state,post);
		}else{
			throw new Error("I do not understand logic type: "+logic_type);
		}
		return ans;
	}
	
	public boolean startGeneratingZ3Logic(ASTNode state){
		boolean ans = true;
		Method m;
		if(state instanceof Method){
			m = (Method) state;
		}else{
			throw new Error("I do not understand: "+state.toString());
		}
		PrintWriter out = hre.lang.System.getLogLevelOutputWriter(hre.lang.System.LogLevel.Info);
		outputToString = new PrefixPrintWriter(out);
		variablelenLijst = new ArrayList<String>();
		hoareTriple = new ArrayList<String>();
		currentWorkingTriple = 0;
		DeclarationStatement arguments[]=m.getArgs();
	    if(!getZ3Type(m.getReturnType()).equals("void")){
	    	variablelenLijst.add("result");
	    	variablelenLijst.add(getZ3Type(m.getReturnType()));
	    }
	    //outputToString.printf("%s%n", arguments);
	    hoareTriple.add(currentWorkingTriple, "");
	    for(int i=0;arguments.length > i;i++){
	    	generateZ3Logic(null, arguments[i], null);
	    }
		Contract contract=m.getContract();
		if (contract!=null){
			ans = ans && preconditionHandler(contract);
		}
		if (m.getBody()!= null){
			ans = ans && generateZ3Logic(contract.pre_condition , state, contract.post_condition);
		}
		if (contract!= null){
			ans = ans && postconditionHandler(contract);
		}
		if(contract == null){
			ans = ans && !thisParent.checkWithZ3(makeReadyForZ3(), variablelenLijst, state);
		}

		if(ans){
			out.println();
			out.printf("No errors were found in: %s%n",m.getOrigin());
			out.println();
		}

		out.close();
		return ans;
	}
	
	private boolean preconditionHandler(Contract contract){
		boolean ans = true;
		/*outputToString.printf("Found precondition%n");
    	outputToString.enter();
    	outputToString.prefixAdd("  ");*/
    	currentWorkingTriple++;
    	hoareTriple.add(currentWorkingTriple, "");
		ans = generateZ3Logic(null ,contract.pre_condition, null);
		//outputToString.leave();
    	currentWorkingTriple++;
    	hoareTriple.add(currentWorkingTriple, "");
		return ans;
	}
	
	private boolean postconditionHandler(Contract contract){
		boolean ans = true;
		/*outputToString.printf("Found postcondition%n");
    	outputToString.enter();
    	outputToString.prefixAdd("  ");*/
    	currentWorkingTriple++;
    	hoareTriple.add(currentWorkingTriple, "");
		ans = generateZ3Logic(null ,contract.post_condition, null);
		//outputToString.leave();
		return ans;
	}
	
	public boolean generateZ3Logic(ASTNode pre, ASTNode state, ASTNode post){
		boolean ans = false;
		if(state instanceof BlockStatement){
			BlockStatement s = (BlockStatement)state;
			ans = handleZ3BlockStatement(pre,s,post);
		}else if(state instanceof IfStatement){
			IfStatement s = (IfStatement) state;
			ans = handleZ3IfStatement(pre,s,post);
		}else if(state instanceof LoopStatement){
			LoopStatement s = (LoopStatement) state;
			ans = handleZ3LoopStatement(pre,s,post);
		}else if(state instanceof AssignmentStatement){
			AssignmentStatement s = (AssignmentStatement) state;
			ans = handleZ3AssignmentStatement(pre,s,post);
		}else if(state instanceof ReturnStatement){
			ReturnStatement s = (ReturnStatement) state;
			ans = handleZ3ReturnStatement(pre,s,post);
		}else if(state instanceof OperatorExpression){
			OperatorExpression e = (OperatorExpression) state;
			ans = handleZ3OperatorExpression(pre,e,post);
		}else if(state instanceof DeclarationStatement){
			DeclarationStatement s = (DeclarationStatement) state;
			ans = handleZ3DeclarationStatement(pre,s,post);
		}else if(state instanceof NameExpression){
			NameExpression e = (NameExpression) state;
			ans = handleZ3NameExpression(pre,e,post);
		}else if(state instanceof ConstantExpression){
			ConstantExpression e = (ConstantExpression) state;
			ans = handleZ3ConstantExpression(pre,e,post);
		}else if(state instanceof Method){
			Method m = (Method)state;
			Contract contract=m.getContract();
			ASTNode body=m.getBody();
			if(contract !=null && body != null){
				ans = generateZ3Logic(contract.pre_condition , body, contract.post_condition);
			}else{
				throw new Error("Method has nobody or contract");
			}
		}else if(state == null){
			ans = true;
		}else{
			throw new Error("I do not understand: "+state.toString());
		}
		return ans;
	}
	
	private boolean handleZ3ConstantExpression(ASTNode pre, ConstantExpression e, ASTNode post) {
		//outputToString.printf("Found constant %s of type %s%n",e.getValue(),e.getType());
		hoareTriple.set(currentWorkingTriple, hoareTriple.get(currentWorkingTriple).concat(" "));
		hoareTriple.set(currentWorkingTriple, hoareTriple.get(currentWorkingTriple).concat(e.value().toString()));
		return true;
	}

	private boolean handleZ3NameExpression(ASTNode pre, NameExpression e, ASTNode post) {
		//outputToString.printf("Found name %s with type %s%n",e.getName(),e.getType());
		hoareTriple.set(currentWorkingTriple, hoareTriple.get(currentWorkingTriple).concat(" "));
		hoareTriple.set(currentWorkingTriple, hoareTriple.get(currentWorkingTriple).concat(e.getName()));
		return true;
	}

	private boolean handleZ3DeclarationStatement(ASTNode pre, DeclarationStatement s, ASTNode post) {
		Type t=s.getType();
		String name = s.name();
		ASTNode init = s.initJava();
		boolean ans = true;
		//outputToString.printf("Found declaration %s of type %s %n",name,t);
		variablelenLijst.add(name);
		variablelenLijst.add(getZ3Type(t));
		if (init != null){
			ans = generateZ3Logic(pre,init,post);
		}
		return ans;
	}

	private boolean handleZ3OperatorExpression(ASTNode pre, OperatorExpression e, ASTNode post) {
		boolean ans = true;
		int N=e.operator().arity();
		/*outputToString.printf("Found operator %s with arity %s %n",e.getOperator(),N);
    	outputToString.enter();
    	outputToString.prefixAdd("  ");*/
    	if(pre!=null){
    		generateZ3Logic(null,pre,null);
    		currentWorkingTriple++;
    		hoareTriple.add(currentWorkingTriple, "");
    	}
    	if(!e.isSpecial(ASTSpecial.Kind.HoarePredicate)){
	    	hoareTriple.set(currentWorkingTriple, hoareTriple.get(currentWorkingTriple).concat(" ("));
	    	hoareTriple.set(currentWorkingTriple, hoareTriple.get(currentWorkingTriple).concat(getCommand(e,"Z3")));
    	}
	    for(int i=0;i<N && ! abort;i++){
	    	ans = generateZ3Logic(null, e.arg(i), null);
	    }
    	//outputToString.leave();
    	if(!e.isSpecial(ASTSpecial.Kind.HoarePredicate)){
    		hoareTriple.set(currentWorkingTriple, hoareTriple.get(currentWorkingTriple).concat(")"));
    	}
    	if(post!=null){
    		currentWorkingTriple++;
    		hoareTriple.add(currentWorkingTriple, "");
    		generateZ3Logic(null,post,null);
    		ans = !thisParent.checkWithZ3(makeReadyForZ3(), variablelenLijst, post);
    		currentWorkingTriple++;
    		hoareTriple.add(currentWorkingTriple, "");
    	}
		return ans;
	}

	private boolean handleZ3ReturnStatement(ASTNode pre, ReturnStatement s, ASTNode post) {
		boolean ans = true;
		ASTNode expr=s.getExpression();
		/*outputToString.printf("Found return statement at %s%n",s.getExpression());
		outputToString.enter();
    	outputToString.prefixAdd("  ");*/
    	if(pre!=null){
    		generateZ3Logic(null,pre,null);
    		currentWorkingTriple++;
    		hoareTriple.add(currentWorkingTriple, "");
    	}
    	hoareTriple.set(currentWorkingTriple, hoareTriple.get(currentWorkingTriple).concat(" ("));
    	hoareTriple.set(currentWorkingTriple, hoareTriple.get(currentWorkingTriple).concat(getCommand(s,"Z3")));
	    if (expr!=null){
	    	ans = generateZ3Logic(null, expr, null);
	    }
	    //outputToString.leave();
	    hoareTriple.set(currentWorkingTriple, hoareTriple.get(currentWorkingTriple).concat(")"));
    	if(post!=null){
    		currentWorkingTriple++;
    		hoareTriple.add(currentWorkingTriple, "");
    		generateZ3Logic(null,post,null);
    		ans = !thisParent.checkWithZ3(makeReadyForZ3(), variablelenLijst, post);
    		currentWorkingTriple++;
    		hoareTriple.add(currentWorkingTriple, "");
    	}
		return ans;
	}

	private boolean handleZ3AssignmentStatement(ASTNode pre, AssignmentStatement s, ASTNode post){
		boolean ans = true;
		/*outputToString.printf("Found Assignment at %s with %s %n",s.getLocation(),s.getExpression());
		outputToString.enter();
    	outputToString.prefixAdd("  ");*/
    	if(pre!=null){
    		ans = generateZ3Logic(null,pre,null);
    		currentWorkingTriple++;
    		hoareTriple.add(currentWorkingTriple, "");
    	}
    	hoareTriple.set(currentWorkingTriple, hoareTriple.get(currentWorkingTriple).concat(" ("));
    	hoareTriple.set(currentWorkingTriple, hoareTriple.get(currentWorkingTriple).concat(getCommand(s,"Z3")));
    	//rewrite s.getLocation() variable <name> -> __<name>
    	ans = ans && generateZ3Logic(null, rewriteVariables(s.location(),s.location()), null);
    	ans = ans && generateZ3Logic(null, s.expression(), null);
		//outputToString.leave();
		hoareTriple.set(currentWorkingTriple, hoareTriple.get(currentWorkingTriple).concat(")"));
    	if(post!=null){
    		//rewrite post variable <name> -> __<name>
    		currentWorkingTriple++;
    		hoareTriple.add(currentWorkingTriple, "");
    		generateZ3Logic(null,rewriteVariables(s.location(),post),null);
    		ans = !thisParent.checkWithZ3(makeReadyForZ3(), variablelenLijst, post);
    		currentWorkingTriple++;
    		hoareTriple.add(currentWorkingTriple, "");
    	}
		return ans;
	}

	private boolean handleZ3LoopStatement(ASTNode pre, LoopStatement s, ASTNode post) {
		boolean ans = false;
		/*outputToString.printf("Found Loop at %s %n",s.getOrigin());
		outputToString.enter();
    	outputToString.prefixAdd("  ");*/
		ASTNode temp = null;
		ASTNode loopGuard = null;
		int i = 0;
		for(ASTNode inv:s.getInvariants()){
	    	if(i > 0){
	    		temp = combineTreeParts(inv,temp,StandardOperator.And);
	    	}else{
	    		temp = inv;
	    		i = 1;
	    	}
	    }
		//outputToString.printf("Checking Guard%n");
		ans = checkSkipZ3(pre,temp);
	    if (s.getEntryGuard()!=null){
	    	//outputToString.printf("Found Loopstatement Guard%n");
	    	loopGuard = combineTreeParts(s.getEntryGuard(),temp,StandardOperator.And);
	    }
	    if (s.getBody()!=null){
	    	//outputToString.printf("Found Loopstatement Body%n");
	    	ans = ans && generateZ3Logic(loopGuard,s.getBody(),temp);
	    }
	    ans = ans && checkSkipZ3(combineTreeParts(negate(s.getEntryGuard()),temp,StandardOperator.And),post);
	    /*outputToString.leave();
	    outputToString.printf("End LoopStatement%n");*/
		return ans;
	}

	private boolean handleZ3IfStatement(ASTNode pre, IfStatement s, ASTNode post) {
		boolean ans = true;
		int N=s.getCount();
		/*outputToString.printf("Found IfStatement%n");
		outputToString.enter();
    	outputToString.prefixAdd("  ");*/
	    for(int i=0;i<N;i++){
	    	/*outputToString.printf("Found IfStatement guard %s%n",s.getGuard(i));
	    	outputToString.printf("Found IfStatement statement %s%n",s.getStatement(i));*/
	    	ans = ans && generateZ3Logic(combineTreeParts(getIfElseGuard(s,i),pre,StandardOperator.And), s.getStatement(i), post);
	    }
	    /*outputToString.leave();
	    outputToString.printf("End IfStatement%n");*/
		return ans;
	}

	private boolean handleZ3BlockStatement(ASTNode pre, BlockStatement s, ASTNode post) {
		ASTNode blockPre = pre;
		ASTNode blockState = null;
		/*outputToString.printf("Found BlockStatement %s with origin %s %n",s.toString(), s.getOrigin().toString());
		outputToString.enter();
    	outputToString.prefixAdd("  ");*/
		boolean ans = true;
		int N=s.getLength();
	    for (int i=0;i<N && ans;i++){
	    	//outputToString.printf("Found BlockStatement Item %s at %s %n",s.getStatement(i).toString(),i+1);
	    	if(s.isSpecial(Kind.HoarePredicate)){
	    		if(blockState == null){
	    			ans = ans && checkSkipZ3(blockPre, s.getStatement(i));
	    		}else{
	    			ans = ans && generateZ3Logic(blockPre, blockState, s.getStatement(i));
	    		}
    			blockPre = s.getStatement(i);
    			blockState = null;
	    	}else{
	    		if(!(s.getStatement(i) instanceof DeclarationStatement) && !(s.getStatement(i) instanceof NameExpression) && !(s.getStatement(i) instanceof ConstantExpression)){
	    			blockState = s.getStatement(i);
	    		}else{
	    			ans = ans && generateZ3Logic(null, s.getStatement(i), null);
	    		}
	    	}
	    }
    	if(blockState == null){
    		ans = ans && checkSkipZ3(blockPre, post);
	    }else{
			ans = generateZ3Logic(blockPre, blockState, post);
		}
	    /*outputToString.leave();
	    outputToString.printf("End BlockStatement %s with origin %s %n",s.toString(), s.getOrigin().toString());*/
		return ans;
	}
	
	private ASTNode rewriteVariables(ASTNode location, ASTNode astNode) {
		ASTFactory astfactory = new ASTFactory();
		HashMap<NameExpression,ASTNode> map = new HashMap<NameExpression,ASTNode>();
		ASTNode ans = astNode;
		if(location instanceof NameExpression){
			String name = "__"+((NameExpression) location).getName();
			astfactory.setOrigin(location.getOrigin());
			ASTNode newName = astfactory.field_name(name);
			map.put(((NameExpression) location), newName);
			Substitution sub = new Substitution(null,map);
			ans = ans.apply(sub);
			if(!variablelenLijst.contains(name)){
				int i = variablelenLijst.indexOf(((NameExpression) location).getName());
				variablelenLijst.add(name);
				variablelenLijst.add(variablelenLijst.get(i+1));
			}
			//outputToString.printf("Substituting %s with %s %n",((NameExpression) location).getName(), name);
		}
		return ans;
	}
	
	private boolean checkSkipZ3(ASTNode one, ASTNode two) {
		generateZ3Logic(null,one,null);
		currentWorkingTriple++;
		hoareTriple.add(currentWorkingTriple, "");
		generateZ3Logic(null,two,null);
		boolean ans = !thisParent.checkWithZ3(makeReadyForSkipZ3(), variablelenLijst, two);
		currentWorkingTriple++;
		hoareTriple.add(currentWorkingTriple, "");
		return ans;
	}

	private String getCommand(Object m, String setting){
		String ans = "";
		if(setting.equals("Z3")){
			if(m instanceof AssignmentStatement){
				ans = "=";
			}else if(m instanceof ReturnStatement){
				ans = "= result";
			}else if(m instanceof OperatorExpression){
				return getLayout((OperatorExpression) m);
			}else{
				throw new Error("Unknown Oject: "+m.toString());
			}
		}else{
			throw new Error("Unknown Setting: "+setting);
		}
		return ans;
	}
	
	private String getLayout(OperatorExpression e) {
		String ans = "";
		if(e.operator().equals(StandardOperator.Plus)){
			ans = "+";
		}else if(e.operator().equals(StandardOperator.Minus)){
			ans = "-";
		}else if(e.operator().equals(StandardOperator.Mult)){
			ans = "*";
		}else if(e.operator().equals(StandardOperator.GT)){
			ans = ">";
		}else if(e.operator().equals(StandardOperator.GTE)){
			ans = ">=";
		}else if(e.operator().equals(StandardOperator.LT)){
			ans = "<";
		}else if(e.operator().equals(StandardOperator.LTE)){
			ans = "<=";
		}else if(e.operator().equals(StandardOperator.EQ)){
			ans = "=";
		}else if(e.operator().equals(StandardOperator.Not)){
			ans = "not";
		}else if(e.operator().equals(StandardOperator.Or)){
			ans = "or";
		}else if(e.operator().equals(StandardOperator.And)){
			ans = "and";
		}else if(e.operator().equals(StandardOperator.UMinus)){
			ans = "-";
		}else{
			ans = e.operator().toString();
		}
		return ans;
	}
	
	private String getZ3Type(Type type) {
		String ans = "";
		if(type.isInteger()){
			ans = "Int";
		}else if(type.isBoolean()){
			ans = "Bool";
		}else if(type.isDouble()){
			ans = "Double";
		}else {
			ans = "void";
		}
		return ans;
	}
	
	private List<String> makeReadyForZ3() {
		ArrayList<String> ans = new ArrayList<String>();
		for(int i = lastHoarePredicate; hoareTriple.size()-1 > i; i++){
			ans.add(hoareTriple.get(i));
		}
		String counter = " (not";
		counter = counter.concat(hoareTriple.get(hoareTriple.size()-1));
		counter = counter.concat(")");
		ans.add(counter);
		lastHoarePredicate = hoareTriple.size()-1;
		return ans;
	}
	private List<String> makeReadyForSkipZ3() {
		ArrayList<String> ans = new ArrayList<String>();
		ans.add(hoareTriple.get(hoareTriple.size()-2));
		String counter = " (not";
		counter = counter.concat(hoareTriple.get(hoareTriple.size()-1));
		counter = counter.concat(")");
		ans.add(counter);
		lastHoarePredicate = hoareTriple.size()-1;
		return ans;
	}
	
	private ASTNode getIfElseGuard(IfStatement s, int i) {
		ASTNode o = s.getGuard(0);
		if(i>0){
			ASTNode guards[] = new ASTNode[2];
			guards[0] = o;
			for(int j = 1;i>j;j++){
				guards[1] = s.getGuard(j);
				ASTFactory assFactory = new ASTFactory();
				o = assFactory.expression(s.getOrigin(),StandardOperator.Or,guards);
				guards[0] = o;
			}
			ASTFactory assFactory = new ASTFactory();
			guards[0] = assFactory.expression(s.getOrigin(),StandardOperator.Not,o);
			guards[1] = s.getGuard(i);
			o = assFactory.expression(s.getOrigin(),StandardOperator.And,guards);
		}
		return o;
	}
	/**
	 * 
	 * @param one
	 * @param two
	 * @param operator an operator with arety 2
	 * @return ASTNode with origin of one that is the combination of one and two with the operator.
	 */
	private ASTNode combineTreeParts(ASTNode one, ASTNode two, StandardOperator operator) {
		ASTFactory astFactory = new ASTFactory();
		ASTNode guards[] = new ASTNode[2];
		Origin ori = one.getOrigin();
		guards[0] = one;
		guards[1] = two;
		if(one.getOrigin() instanceof FileOrigin && one.getOrigin() instanceof FileOrigin){
			ori = ((FileOrigin) one.getOrigin()).merge((FileOrigin) two.getOrigin());
		}
		return astFactory.expression(ori,operator,guards);
	}
	private ASTNode negate(ASTNode entryGuard){
		ASTFactory astFactory = new ASTFactory();
		ASTNode guards[] = new ASTNode[1];
		guards[0] = entryGuard;
		return astFactory.expression(entryGuard.getOrigin(),StandardOperator.Not,guards);
	}

	/**
	   * This class extends the abstract scanner to scan for methods.
	   * It will then scan those methods for assertions.
	   *  vct --passes="resolv,boogie-fol" BoogieTemp.java 
	   *
	   * @author Stefan Blom
	   *
	   */
	private static class MethodFinder extends RecursiveVisitor {
	  public MethodFinder(){
	    super(null,null);
	  }
	    /** 
	     * Executed when the abstract scanner finds a method.
	     */
		public void visit(Method m){
			PrefixPrintWriter out=new PrefixPrintWriter(hre.lang.System.getLogLevelOutputWriter(hre.lang.System.LogLevel.Info));
			ASTNode body=m.getBody();
			Contract c=m.getContract();
			out.printf("starting%n");
			if (c==null) Fail("method %s has no contract",m.getName());
			out.printf("=====begin precondition=====%n");
			printTree(out,c.pre_condition,ASTNode.class);
			out.printf("+++++end precondition+++++%n");
			if (body instanceof BlockStatement){
			    /* In Java the body of a method always is a block.
			     * However, the AST also allows expressions as Method bodies.
			     */
			    BlockStatement block=(BlockStatement)body;
			    int N=block.getLength();
			    for(int i=0;i<N;i++){
			          out.printf("========block %d=======%n",i);
			          printTree(out,block.getStatement(i),ASTNode.class);
			          out.printf("+++++end block++++++%n");
			          if (block.getStatement(i) instanceof OperatorExpression){
			              OperatorExpression e=(OperatorExpression)block.getStatement(i);
			              out.printf("checking formula at %s%n",e.getOrigin());
			              if (e.isSpecial(Kind.Assert));
			              DeclarationStatement args[]=m.getArgs();
			              ASTNode formula=e.arg(0);
			              //BoogieReport res=check_boogie(args,formula);
			              out.printf("formula at %s:%s%n",e.getOrigin(),args,formula);
			          }
			    }
			} else {
				out.printf("skipping non-block body of method %s at %s%n",m.getName(),m.getOrigin());
			}
			out.printf("=====begin postcondition=====%n");
			printTree(out,c.post_condition,ASTNode.class);
			out.printf("+++++end postcondition+++++%n");
			out.close();
		}
	}
	
  private static class StringPrinter extends AbstractVisitor<Boolean> {
		private int currentWorkingTriple;
		private int treeDepth;
		private List<String> variablelenLijst;
		private List<String> hoareTriple;
		private AssignmentStatement assignment;
		private PrefixPrintWriter outputToString;
		private String setting;
		public void setResult(Boolean b){
			abort=b.booleanValue();
		}

		public Boolean getResult(){
			return Boolean.valueOf(abort);
		}
		  
		/** Return from the current scan if set. */
		public boolean abort=false;

		@Override
		public void visit(StandardProcedure p) {
		    // has no children
			outputToString.printf("StandardProcedure%n");
		}
/*
		@Override
		public void visit(Instantiation i) {
		    i.getSort().accept(this);
		    outputToString.printf("Found Instantiation at %s with %n",i.toString());
		    int N=i.getArity();
		    for(int j=0;j<N && ! abort;j++){
		    	i.getArg(j).accept(this);
		    }
		}
*/
		@Override
		public void visit(ConstantExpression e) {
		    // Constants have no children.
			outputToString.printf("Found constant %s of type %s%n",e.value(),e.getType());
			hoareTriple.set(currentWorkingTriple, hoareTriple.get(currentWorkingTriple).concat(" "));
			hoareTriple.set(currentWorkingTriple, hoareTriple.get(currentWorkingTriple).concat(e.value().toString()));
		}

		@Override
		public void visit(OperatorExpression e) {
			int N=e.operator().arity();
			if(e.isSpecial(ASTSpecial.Kind.HoarePredicate)){
				ASTFactory astfactory = new ASTFactory();
				OperatorExpression e2 = astfactory.expression(e.getOrigin(),StandardOperator.Not,e.arg(0));
				e2.accept(this);
				//thisParent.checkWithZ3(hoareTriple, variablelenLijst);
			}
			outputToString.printf("Found operator %s with arity %s %n", e.operator(), N);
	    	outputToString.enter();
	    	outputToString.prefixAdd("  ");
	    	treeDepth++;
	    	if(treeDepth < 2){
	    		currentWorkingTriple++;
	    		hoareTriple.add(currentWorkingTriple, "");
	    	}
	    	if(!e.isSpecial(ASTSpecial.Kind.HoarePredicate)){
		    	hoareTriple.set(currentWorkingTriple, hoareTriple.get(currentWorkingTriple).concat(" ("));
		    	hoareTriple.set(currentWorkingTriple, hoareTriple.get(currentWorkingTriple).concat(getCommand(e)));
	    	}
		    for(int i=0;i<N && ! abort;i++){
		    	e.arg(i).accept(this);
		    }
	    	outputToString.leave();
	    	treeDepth--;
	    	if(!e.isSpecial(ASTSpecial.Kind.HoarePredicate)){
	    		hoareTriple.set(currentWorkingTriple, hoareTriple.get(currentWorkingTriple).concat(")"));
	    	}
		}

		@Override
		public void visit(NameExpression e) {
		    // Names have no children.
			outputToString.printf("Found name %s with type %s%n",e.getName(),e.getType());
			hoareTriple.set(currentWorkingTriple, hoareTriple.get(currentWorkingTriple).concat(" "));
			hoareTriple.set(currentWorkingTriple, hoareTriple.get(currentWorkingTriple).concat(e.getName()));
		}

		@Override
		public void visit(ClassType t) {
		    // Class types have no children.
			outputToString.printf("Class type%n");
		}

		@Override
		public void visit(FunctionType t) {
		    throw new Error("missing case in Abstract Scanner: "+t.getClass());
		}

		@Override
		public void visit(PrimitiveType t) {
		    // Primitive types have no children.
			outputToString.printf("Primitive type%n");
		}

		@Override
		public void visit(RecordType t) {
			throw new Error("missing case in Abstract Scanner: "+t.getClass());
		}

		@Override
		public void visit(MethodInvokation e) {
			outputToString.printf("Found MethodeInvocation %s%n",e.toString());
		    if (e.object!=null) e.object.accept(this);
		    outputToString.printf("%s",e.method);
		    int N=e.getArity();
		    for(int i=0;i<N;i++){
		    	e.getArg(i).accept(this);
		    }
		}

		@Override
		public void visit(BlockStatement s) {
			outputToString.printf("Found BlockStatement %s %n",s.toString());
			outputToString.enter();
	    	outputToString.prefixAdd("  ");
			int N=s.getLength();
		    for (int i=0;i<N;i++){
		    	s.getStatement(i).accept(this);
		    }
		    outputToString.leave();
		}

		@Override
		public void visit(IfStatement s) {
			int N=s.getCount();
			outputToString.printf("Found IfStatement%n");
			outputToString.enter();
	    	outputToString.prefixAdd("  ");
		    for(int i=0;i<N;i++){
		    	outputToString.printf("Found IfStatement guard %s%n",s.getGuard(i));
		    	if(s.getGuard(i).toString().equals("true")){
		    		getElseGuard(s,i).accept(this);
		    	}else{
		    		s.getGuard(i).accept(this);
		    	}
		    	outputToString.printf("Found IfStatement statement %s%n",s.getStatement(i));
		    	s.getStatement(i).accept(this);
		    }
		    outputToString.leave();
		}

		private ASTNode getElseGuard(IfStatement s, int i) {
			ASTNode o = s.getGuard(0);
			ASTNode guards[] = new ASTNode[2];
			guards[0] = o;
			for(int j = 1;i>j;j++){
				guards[1] = s.getGuard(j);
				ASTFactory assFactory = new ASTFactory();
				o = assFactory.expression(s.getOrigin(),StandardOperator.And,guards);
				guards[0] = o;
			}
			ASTFactory assFactory = new ASTFactory();
			o = assFactory.expression(s.getOrigin(),StandardOperator.Not,o);
			return o;
		}

		@Override
		public void visit(ReturnStatement s) {
			ASTNode expr=s.getExpression();
			outputToString.printf("Found return statement at %s%n",s.getExpression());
			outputToString.enter();
	    	outputToString.prefixAdd("  ");
	    	treeDepth++;
	    	if(treeDepth < 2){
	    		currentWorkingTriple++;
	    		hoareTriple.add(currentWorkingTriple, "");
	    	}
	    	hoareTriple.set(currentWorkingTriple, hoareTriple.get(currentWorkingTriple).concat(" ("));
	    	hoareTriple.set(currentWorkingTriple, hoareTriple.get(currentWorkingTriple).concat(getCommand(s)));
		    if (expr!=null) expr.accept(this);
		    outputToString.leave();
		    treeDepth--;
		    hoareTriple.set(currentWorkingTriple, hoareTriple.get(currentWorkingTriple).concat(")"));
		}

		@Override
		public void visit(AssignmentStatement s) {
			outputToString.printf("Found Assignment at %s with %s %n", s.location(), s.expression());
			outputToString.enter();
	    	outputToString.prefixAdd("  ");
	    	treeDepth++;
	    	if(treeDepth < 2){
	    		currentWorkingTriple++;
	    		hoareTriple.add(currentWorkingTriple, "");
	    	}
	    	hoareTriple.set(currentWorkingTriple, hoareTriple.get(currentWorkingTriple).concat(" ("));
	    	hoareTriple.set(currentWorkingTriple, hoareTriple.get(currentWorkingTriple).concat(getCommand(s)));
			s.location().accept(this);
			s.expression().accept(this);
			outputToString.leave();
			treeDepth--;
			hoareTriple.set(currentWorkingTriple, hoareTriple.get(currentWorkingTriple).concat(")"));
		}

		@Override
		public void visit(DeclarationStatement s) {
			Type t=s.getType();
			String name = s.name();
			ASTNode init = s.initJava();
			outputToString.printf("Found declaration %s of type %s %n",name,t);
			variablelenLijst.add(name);
			variablelenLijst.add(getZ3Type(t));
			if (init != null) init.accept(this);
		}

		@Override
		public void visit(LoopStatement s) {
		    for(ASTNode inv:s.getInvariants()){
		    	inv.accept(this);
		    }
		    ASTNode tmp;
		    tmp=s.getInitBlock();
		    if (tmp!=null) tmp.accept(this);
		    tmp=s.getEntryGuard();
		    if (tmp!=null) tmp.accept(this);
		    tmp=s.getBody();
		    if (tmp!=null) tmp.accept(this);
		    tmp=s.getExitGuard();
		    if (tmp!=null) tmp.accept(this);
		}

		@Override
		public void visit(Method m) {
			PrintWriter out = hre.lang.System.getLogLevelOutputWriter(hre.lang.System.LogLevel.Info);

			outputToString = new PrefixPrintWriter(out);
			variablelenLijst = new ArrayList<String>();
			hoareTriple = new ArrayList<String>();
			currentWorkingTriple = 0;
			treeDepth = 0;
			setting = "Z3";
			thisParent.newProblem();
		    DeclarationStatement arguments[]=m.getArgs();
		    if(!getZ3Type(m.getReturnType()).equals("void")){
		    	variablelenLijst.add("result");
		    	variablelenLijst.add(getZ3Type(m.getReturnType()));
		    }
		    outputToString.printf("%s%n", (Object[])arguments);
		    hoareTriple.add(currentWorkingTriple, "");
		    for(int i=0;arguments.length > i;i++){
		    	this.visit(arguments[i]);
		    }
		    String name=m.getName();
		    int N=m.getArity();
		    String args[]=new String[N];
		    for(int i=0;i<N;i++){
		    	args[i]=m.getArgument(i);
		    	outputToString.printf("found argument %s%n",(Object[])m.getArgs());
		    }
		    Contract contract=m.getContract();
		    if(contract !=null){
		    	outputToString.enter();
		    	outputToString.prefixAdd("  ");
		    	treeDepth++;
		    	contract.pre_condition.accept(this);
		    	outputToString.leave();
		    	treeDepth--;
		    }
		    ASTNode body=m.getBody();
		    if (body!=null){
		    	outputToString.enter();
		    	outputToString.prefixAdd("  ");
		    	body.accept(this);
		    	outputToString.leave();
		    }
		    if(contract !=null){
		    	currentWorkingTriple++;
		    	hoareTriple.add(currentWorkingTriple, "");
		    	outputToString.enter();
		    	outputToString.prefixAdd("  ");
		    	treeDepth++;
		    	ASTFactory astFactory = new ASTFactory();
				OperatorExpression e2 = astFactory.expression(m.getOrigin(),StandardOperator.Not, contract.post_condition);
				e2.accept(this);
		    	outputToString.leave();
		    	treeDepth--;
		    }
			//thisParent.checkWithZ3(hoareTriple, variablelenLijst);
		    for(int i = 0; variablelenLijst.size() > i; i++){
		    	out.println(variablelenLijst.get(i));
		    }
		    for(int i = 0; hoareTriple.size() > i;i++){
		    	out.println(hoareTriple.get(i));
		    }

		    out.close();
		}
		private String getZ3Type(Type type) {
			String ans = "";
			if(type.isInteger()){
				ans = "Int";
			}else if(type.isBoolean()){
				ans = "Bool";
			}else if(type.isDouble()){
				ans = "Double";
			}else {
				ans = "void";
			}
			return ans;
		}
		
		private String getCommand(Object m){
			String ans = "";
			if(setting.equals("Z3")){
				if(m instanceof AssignmentStatement){
					ans = "=";
				}else if(m instanceof ReturnStatement){
					ans = "= result";
				}else if(m instanceof OperatorExpression){
					return getLayout((OperatorExpression) m);
				}else{
					throw new Error("Unknown Oject: "+m.toString());
				}
			}else{
				throw new Error("Unknown Setting: "+setting);
			}
			return ans;
		}

		@Override
		public void visit(ASTClass c) {
			int N=c.getStaticCount();
		    for(int i=0;i<N;i++){
		    	c.getStatic(i).accept(this);
		    }
		    int M=c.getDynamicCount();
		    for(int i=0;i<M;i++){
		    	c.getDynamic(i).accept(this);
		    }
		}
		  
		public void visit(BindingExpression e){
			outputToString.printf("Found Binding Expression %s %n",e.toString());
		    e.select.accept(this);
		    if (abort) return;
		    e.main.accept(this);
		}
		

		private String getLayout(OperatorExpression e) {
			String ans = "";
			if(e.operator().equals(StandardOperator.Plus)){
				ans = "+";
			}else if(e.operator().equals(StandardOperator.Minus)){
				ans = "-";
			}else if(e.operator().equals(StandardOperator.Mult)){
				ans = "*";
			}else if(e.operator().equals(StandardOperator.GT)){
				ans = ">";
			}else if(e.operator().equals(StandardOperator.GTE)){
				ans = ">=";
			}else if(e.operator().equals(StandardOperator.LT)){
				ans = "<";
			}else if(e.operator().equals(StandardOperator.LTE)){
				ans = "<=";
			}else if(e.operator().equals(StandardOperator.EQ)){
				ans = "=";
			}else if(e.operator().equals(StandardOperator.Not)){
				ans = "not";
			}else if(e.operator().equals(StandardOperator.Or)){
				ans = "or";
			}else if(e.operator().equals(StandardOperator.And)){
				ans = "and";
			}else{
				ans = e.operator().toString();
			}
			return ans;
		}
	}
	  
	public static void printTree(){}
		
	public static void printTree(PrefixPrintWriter out, Object tree, Class ... base_classes){
		printTree(out,new HashSet<Object>(),tree,base_classes);
	}
	
	private static void printTree(PrefixPrintWriter out, Set<Object> visited, Object tree, Class ... base_classes){
			if (visited.contains(tree)) return;
		    visited.add(tree);
		    Class tree_class=tree.getClass();
		    //out.printf("<%s>\n",tree_class.getName());
		    out.enter();
		    out.prefixAdd("  ");
		    for(Field field:tree_class.getDeclaredFields()){
		    	field.setAccessible(true);
		    	try {
		    		Object val=field.get(tree);
		    		if (subtype(field.getType(),base_classes)) {
		    			if (val==null) {
		    				if(!field.getName().equals("site")){
		    					out.printf("null field %s%n",field.getName());
		    				}
		    			} else {
		    				printTree(out,visited,val,base_classes);
		    			}
		    		} else if (val!=null && val instanceof Collection) {
		    			for(Object i:(Collection)val){
		    				if (i!=null && is_instance(i,base_classes)){
		    					printTree(out,visited,i,base_classes);
		    				}
		    			}
		    		} else if (val !=null && field.getType().isArray()) {
		    			for(Object i:(Object[])val){
		    				if (i!=null && is_instance(i,base_classes)){
		    					printTree(out,visited,i,base_classes);
		    				}
		    			}
		    		} else if(field.getName().equals("name")){
		    			out.printf("%s%n",val);
		    		}else if(field.getName().equals("value")){
		    			out.printf("%s%n",val);
		    		}else if(field.getName().equals("op")){
		    			out.printf("Operator is %s%n",val);
		    		}else if(field.getName().equals("sort")){
		    			out.printf("Sort is %s%n",val);
		    		}else if(val == null || field.getName().equals("kind")){
		    			
		    		}else {
		    			//out.printf("skipping field %s it is %s%n",field.getName(),val);
		    			
		    		}         
		    	} catch (IllegalAccessException e){
		    	  out.printf("unreadable field %s%n",field.getName());
		    	}
		    }
		    out.leave();
		    //out.printf("</%s>%n",tree_class.getName());
	}
		
	public static boolean is_instance(Object o,Class ... base_classes){
		Class o_class=o.getClass();
		for(Class base:base_classes){
			if (base.isInstance(o)) return true; 
		}
		return false;
	}
			  
	public static boolean subtype(Class c,Class ... base_classes){
		for(Class base:base_classes){
			for(Class i=c;i!=null;i=i.getSuperclass()){
			    if (i==base) return true;
			}
		}
		return false;    
	}
		
	/** find all assertions in the given program.
	 * @param program The program to scan for assertions.
	 */
	public static void main(ASTClass program) {
		//Translator t = new Translator(program);
	}
}
