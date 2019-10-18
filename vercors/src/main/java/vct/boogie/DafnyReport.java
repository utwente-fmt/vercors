// -*- tab-width:2 ; indent-tabs-mode:nil -*-
package vct.boogie;

import hre.ast.Origin;
import hre.ast.TrackingTree;
import hre.io.Message;
import hre.io.MessageProcess;

import static hre.lang.System.Debug;
import static hre.lang.System.Warning;

/**
 * This class contains a parser for the output of the Chalice tool.
 * 
 * @author sccblom
 *
 */
public class DafnyReport extends hre.util.TestReport {
  
  public DafnyReport(MessageProcess shell, TrackingTree tree){
    try {
      String line;
      for(;;){
        Message msg=shell.recv();
        Debug(msg.getFormat(),msg.getArgs());
        if (msg.getFormat().equals("exit %d")) break;
        if (msg.getFormat().equals("stderr: %s") || msg.getFormat().equals("stdout: %s")){
          line=(String)msg.getArg(0);
        } else {
          continue;
        }
        if (line.matches(".*Error.*")){
        	Warning("error line: %s",line);
        	int start=line.indexOf("(");
        	int comma=line.indexOf(",");
        	int end=line.indexOf(")");
        	int line_no=Integer.parseInt(line.substring(start+1,comma));
        	int col_no=Integer.parseInt(line.substring(comma+1,end));
        	String message=line.substring(line.indexOf("Error:")+7);
        	Debug("%d,%d: %s",line_no,col_no,message);
        	Origin origin=tree.getOrigin(line_no,col_no);
        	origin.report("error",message);
        	continue;
        }
        if (line.equals("")) continue;
        if (line.startsWith("Unhandled Exception") || line.startsWith("  at ")){
          Warning("%s",line);
          continue;
        }
        if (line.startsWith("Dafny program verifier version")){
          continue;
        }
        if (line.startsWith("Dafny program verifier finished")){
          String words[]=line.split(" ");
          @SuppressWarnings("unused")
          int verified_count=-1;
          int error_count=-1;
          int timeout_count=0;
          for(int i=1;i<words.length;i++){
            if (words[i].matches("verified.*")) verified_count=Integer.parseInt(words[i-1]);
            if (words[i].matches("error.*")) error_count=Integer.parseInt(words[i-1]);
            if (words[i].equals("time")) timeout_count=Integer.parseInt(words[i-1]);
          }
          if (error_count==0 && timeout_count==0){
            setVerdict(Verdict.Pass);
          } else if (error_count==0) {
        	Warning("Dafny finished with %d timeouts",timeout_count);
        	setVerdict(Verdict.Inconclusive);
          } else {
            setVerdict(Verdict.Fail);
          }
          continue;
        }
        Warning("unclaimed output in Dafny report: %s",line);
      }
    } catch (Exception e) {
      Warning("unexpected exception %s",e);
      setException(e);
    }
  }
}


