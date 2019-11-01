package vct.main;

import java.util.*;

import hre.io.Message;
import hre.io.MessageProcess;

/**
 * @author Remco Swenker
 * this class controls the interaction towards the SMTsolver. at the moment only Z3 tool is supported.
 */
@SuppressWarnings("all")
public class SMTinter {
	public SMTinter(){
		/*String[] string1 = new String[1];
		String[] string2 = new String[4];
		string1[0] = "(assert (and p (not q)))";
		string2[0] = "p";
		string2[1] = "Bool";
		string2[2] = "q";
		string2[3] = "Bool";
		SMTresult resulting = checkTripleZ3(string1,string2,"");
		*/
	}
	
	/**
	 * function for checking to SMT transformed hoare triples with Z3
	 * @param smtTriple a String array of the logic statements to be checked by SMT in SMT format.
	 * @param variables a String array of variable names and there type example [x,Integer,b,Boolean].
	 * @param logicType a Sting that tells witch SMT logic Z3 should use.
	 * @return the output form the SMT Solver in the form of a SMTresult class
	 */
	public SMTresult checkTripleZ3(String[] smtTriple, String[] variables, String logicType){
		// TODO: it appears the command below depends on a script that no longer exists. See also Z3FOL.
		MessageProcess z3=new MessageProcess(new String[] {"vct-z3","/smt2","/in"});
		List<String> ans = new ArrayList<String>();
		boolean keepgoing = true;
		boolean satisfied = false;
		boolean noAnswer = true;
		z3.send("(set-option :produce-models true)");
		z3.send("(set-option :print-success false)");
		logicType = "(set-logic QF_NIA)";
		z3.send(logicType);
		//Variable declaration
		for(int x = 0; variables.length > x; x++){
			String msg = "(declare-fun ";
			msg = msg.concat(variables[x]);
			x++;
			msg = msg.concat(" () ");
			msg = msg.concat(variables[x]);
			msg = msg.concat(")");
			z3.send(msg);
		}
		//entering of SMT statements
		for(int x = 0; smtTriple.length > x; x++){
			z3.send(smtTriple[x]);
		}
		z3.send("(check-sat)");
		while(keepgoing){
			Message res=z3.recv();
			ans.add(String.format(res.getFormat(),res.getArgs()));
			if(res.getFormat().equals("stdout: %s")){
				if(((String)res.getArg(0)).equals("sat")){
					satisfied = true;
					keepgoing = false;
					noAnswer = false;
				}else if(((String)res.getArg(0)).equals("unsat")){
					satisfied = false;
					keepgoing = false;
					noAnswer = false;
				}
			}
		}
		//getting counter examples
		if(satisfied){
			for(int x = 0; variables.length > x; x=x+2){
				String msg = "(get-value (";
				msg = msg.concat(variables[x]);
				msg = msg.concat("))");
				z3.send(msg);
			}
		}
		z3.send("(exit)");
		keepgoing = true;
		while(keepgoing){
			Message res=z3.recv();ans.add(String.format(res.getFormat(),res.getArgs()));
			if(res.getFormat().equals("exit %d")){
				keepgoing = false;
			}
		}
		return new SMTresult(ans, satisfied, noAnswer);
	}
	public static void main(String[] args){
		hre.lang.System.setOutputStream(System.out, hre.lang.System.LogLevel.Info);
		hre.lang.System.setErrorStream(System.err, hre.lang.System.LogLevel.Info);

		SMTinter eennieuwe = new SMTinter();
	}
}
