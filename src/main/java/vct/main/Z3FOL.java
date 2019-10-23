package vct.main;

import hre.io.Message;
import hre.io.MessageProcess;

import static hre.lang.System.Debug;


public class Z3FOL {
  
  public static void test(){
    MessageProcess z3=new MessageProcess(new String[] {"vct-z3","/smt2","/in"});
    z3.send("(set-option :produce-models true)");
    z3.send("(set-option :print-success false)");
    // TODO: replace by String logic argument
    z3.send("(set-logic QF_UF)");
    // TODO: replace by loop over declaration
    z3.send("(declare-fun p () Bool)");
    z3.send("(declare-fun q () Bool)");
    // TODO: replace by AST2String
    z3.send("(assert (and p (not p)))");
    z3.send("(check-sat)");
    for(;;){
      Message res=z3.recv();
      Debug(res.getFormat(),res.getArgs());
      // TODO: take unsat and exit possibility into account.
      if (res.getFormat().equals("stdout: %s")){
        if(((String)res.getArg(0)).equals("sat")) break;
        //if(((String)res.getArg(0)).equals("unsat")) break;
      }
    }
    // TODO: replace by loop over declaration
    z3.send("(get-value (p))");
    z3.send("(get-value (q))");
    z3.send("(exit)");
    for(;;){
      Message res=z3.recv();
      Debug(res.getFormat(),res.getArgs());
      if (res.getFormat().equals("exit %d")) break;
    }    
  }
}
