// -*- tab-width:2 ; indent-tabs-mode:nil -*-
package vct.boogie;

import hre.ast.Origin;
import hre.ast.TrackingTree;
import hre.io.Message;
import hre.io.MessageProcess;

import java.util.*;

import static hre.lang.System.Debug;
import static hre.lang.System.Warning;

/**
 * This class contains a parser for the output of the Chalice tool.
 * 
 * @author sccblom
 *
 */
public class ChaliceReport extends hre.util.TestReport {
  
  public ChaliceReport(MessageProcess shell, HashSet<Origin> must_refute, TrackingTree tree){
    HashSet<Origin> has_refuted=new HashSet<Origin>();
    int real_errors=0;
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
        if (line.equals("")) continue;
        if (line.startsWith("Unhandled Exception") || line.startsWith("  at ")){
          Warning("%s",line);
          continue;
        }
        if (line.startsWith("  ")){
          int dot=line.indexOf(".");
          int colon=line.indexOf(":");
          int line_no=Integer.parseInt(line.substring(2,dot));
          int col_no=Integer.parseInt(line.substring(dot+1,colon));
          Origin origin=tree.getOrigin(line_no,col_no);
          if (!vct.util.Configuration.detailed_errors.get()){
            Debug("line is [%s]",line);
            int sentence=line.indexOf(". ",colon);
            String message;
            if (sentence >0){
              message=line.substring(colon+2,sentence+1);
            } else {
              message=line.substring(colon+2);
            }
            message=message.replaceAll(" at [0-9]+[.][0-9]+ "," ");
            Debug("error at %s: %s",origin,message);
            if (must_refute.contains(origin)){
              Warning("expected error found: %s",message);
              has_refuted.add(origin);
            } else {
              Warning("unexpected error found: %s",message);
              origin.report("error",message);
              real_errors++;
            }
          } else {
            ArrayList<String> error=new ArrayList<String>();
            String parts[]=line.substring(colon+2).split("[0-9]+[.][0-9]+");
            error.add(parts[0]);
            Debug("error at %s: %s",origin,parts[0]);
            int current=colon+2+parts[0].length();
            for(int i=1;i<parts.length;i++){
              int end=line.indexOf(parts[i],current);
              dot=line.indexOf(".",current);
              line_no=Integer.parseInt(line.substring(current,dot));
              col_no=Integer.parseInt(line.substring(dot+1,end));
              Origin msg_origin=tree.getOrigin(line_no,col_no);
              Debug("%s%s",msg_origin,parts[i]);
              error.add(msg_origin+parts[i]);
              current=end+parts[i].length();
            }
            if (must_refute.contains(origin)){
              Warning("expected error found: %s",error);
              has_refuted.add(origin);
            } else {
              Warning("unexpected error found: %s",error);
              origin.report("error",error);
              real_errors++;
            }
          }
          continue;
        }
        if (line.startsWith("Boogie program verifier version")){
          continue;
        }
        if (line.startsWith("Boogie program verifier finished")){
          Warning("checking if all refutes have succeeded");
          for(Origin ref:must_refute){
            if (!has_refuted.contains(ref)){
              ref.report("error","failed to refute property");
              real_errors++;
            }
          }
          String words[]=line.split(" ");
          @SuppressWarnings("unused")
          int verified_count=-1;
          @SuppressWarnings("unused")
          int error_count=-1;
          int timeout_count=0;
          for(int i=1;i<words.length;i++){
            if (words[i].matches("verified.*")) verified_count=Integer.parseInt(words[i-1]);
            if (words[i].matches("error.*")) error_count=Integer.parseInt(words[i-1]);
            if (words[i].equals("time")) timeout_count=Integer.parseInt(words[i-1]);
          }
          if (real_errors==0 && timeout_count==0){
            setVerdict(Verdict.Pass);
          } else if (timeout_count>0) {
        	  Warning("Chalice/Boogie finished with %d timeouts",timeout_count);
        	  setVerdict(Verdict.Inconclusive);
          } else {
            setVerdict(Verdict.Fail);
          }
          continue;
        }
        Warning("unclaimed output in Chalice report: %s",line);
      }
    } catch (Exception e) {
      Warning("unexpected exception %s",e);
      setException(e);
    }
  }
}


