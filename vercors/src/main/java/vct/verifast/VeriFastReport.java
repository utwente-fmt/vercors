package vct.verifast;

import hre.ast.TrackingTree;
import hre.ast.Origin;
import hre.io.Message;
import hre.io.MessageProcess;

import static hre.lang.System.Debug;
import static hre.lang.System.Warning;

public class VeriFastReport  extends hre.util.TestReport {

  public VeriFastReport(MessageProcess shell, TrackingTree tree) {
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
        if (line.equals("0 errors found")) {
          setVerdict(Verdict.Pass);
          Debug("Verdict is Pass");
          do {
            msg=shell.recv();
            Debug(msg.getFormat(),msg.getArgs());
          } while (!msg.getFormat().equals("exit %d"));
          break;
        }
        if (line.matches(".*[(][0-9]+[,][0-9-]*[)][:].*")){
          setVerdict(Verdict.Fail);
          Debug("Verdict is Fail");
          int open=line.indexOf('(');
          int comma=line.indexOf(',',open);
          int dash=line.indexOf('-',comma);
          int close=line.indexOf(')',dash);
          int line_no=Integer.parseInt(line.substring(open+1,comma));
          int col_no=Integer.parseInt(line.substring(comma+1,dash));
          Origin msg_origin=tree.getOrigin(line_no,col_no);
          String error=msg_origin.toString()+line.substring(close+1);
          msg_origin.report("error",error);
        }
      }
    } catch (Exception e) {
      Warning("unexpected exception %s",e);
      setException(e);
    }
  }

}
