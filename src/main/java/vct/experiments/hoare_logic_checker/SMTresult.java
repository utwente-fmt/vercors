package vct.experiments.hoare_logic_checker;

import java.util.List;

/**
 * @author Remco Swenker
 * a class to store the result from the SMT solver in.
 */
public class SMTresult {
	/* the example as given by an SMT solver */
	private List<String> example;
	/* the result of an SMT solver */
	private boolean satisfying;
	/* tells if the solver got an answer. */
	private boolean noAnswer;
	
	public SMTresult(List<String> givenExample, boolean givenSatisfying, boolean gotNoAnswer){
		example = givenExample;
		satisfying = givenSatisfying;
		noAnswer = gotNoAnswer;
	}
	
	/**
	 * @return true if answer was given by the SMT solver.
	 */
	 public boolean gotAnswer(){
		return !noAnswer;
	 }
	
	/**
	 * @return true if answer was satisfying. false if not.
	 */
	public boolean getSatisfying(){
		return satisfying;
	}
	/** Return an example. Requires that this.getSatisfying() == true
	 * @return a satisfying interpretation from the SMT solver.
	 */
	public List<String> getExample(){
		return example;
	}
}
