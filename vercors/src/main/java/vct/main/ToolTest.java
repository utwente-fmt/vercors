package vct.main;

import hre.io.Message;
import hre.io.MessageProcess;
import hre.util.TestReport;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import vct.silver.SilverBackend;
import vct.util.Configuration;

import static hre.lang.System.Verdict;

public class ToolTest {
  private final static Pattern TIME_PATTERN = Pattern.compile("^(\\s*\\[[^\\]]*\\])*\\s*([a-zA-Z_ ]+) took\\s*([0-9]+)\\s*ms$");

  public static void fail(VCTResult res,String msg){
    Verdict("failure: %s",msg);
    res.verdict=TestReport.Verdict.Error;
  }
  public VCTResult run(String ... args) {
    StackTraceElement[] stackTraceElements = Thread.currentThread().getStackTrace();
    int idx=0;
    while(!stackTraceElements[idx].getMethodName().equals("run")){
      idx++;
    }
    idx++;
    String test_name=stackTraceElements[idx].getMethodName();
    VCTResult res=new VCTResult();
    String OS=System.getProperty("os.name");
    MessageProcess p=null;
    MessageProcess sh=null;
    res.verdict=TestReport.Verdict.Inconclusive;
    switch(args[0]){
    case "vct":
      ArrayList<String> command = new ArrayList<>();

      Collections.addAll(command, "java", "-Xss128M", "-cp", System.getProperty("java.class.path"), "vct.main.Main");

      if (args[1].equals("--syntax")){
        command.add("--passes=standardize,check,java");
      }

      command.add(args[1]);
      command.add("--progress");

      sh=Configuration.getShell();
      res.verdict=null;

      if (CommandLineTesting.savedir.used()){
        Path dir=Paths.get(CommandLineTesting.savedir.get()).toAbsolutePath();
        String ext="";
        if (args[1].startsWith("--silver")){
          ext=".sil";
        } else if (args[1].startsWith("--chalice")) {
          ext=".chalice";
        } else if (args[1].startsWith("--boogie")) {
          ext=".bpl";
        } else if (args[1].startsWith("--dafny")) {
          ext=".dfy";
        }
        command.add("--encoded="+dir+File.separator+test_name+ext);
      }
      if (SilverBackend.silver_module.used()){
        command.add("--silver-module="+SilverBackend.silver_module.get());
      }

      command.add(args[2]);

      p=new MessageProcess(command.toArray(new String[0]));
      break;
    case "z3":
      sh=Configuration.getShell(vct.boogie.Main.z3_module.get());
      break;
    case "boogie":
      sh=Configuration.getShell(
          vct.boogie.Main.z3_module.get(),
          vct.boogie.Main.boogie_module.get());
      break;
    case "carbon":
      sh=Configuration.getShell(
          vct.boogie.Main.z3_module.get(),
          vct.boogie.Main.boogie_module.get(),
          vct.silver.SilverBackend.silver_module.get());
     
      break;
    case "dafny":
      sh=Configuration.getShell(
          vct.boogie.Main.dafny_module.get());
      break;
    case "silicon":
    case "silicon_qp":
      String z3;
      //if (vct.boogie.Main.z3_module.used()){
      //  z3=vct.boogie.Main.z3_module.get();
      //} else {
        z3="z3/4.3.2";
      //}
      sh=Configuration.getShell(
          z3,
          vct.silver.SilverBackend.silver_module.get());
      break;
    case "chalice":
      sh=Configuration.getShell(
          vct.boogie.Main.z3_module.get(),
          vct.boogie.Main.boogie_module.get(),
          vct.boogie.Main.chalice_module.get());
      /*
        because Chalice assumes that every argument that starts with / is an option,
        we translate absolute path to relative paths.
       */
      for(int i=1;i<args.length;i++){
        if (args[i].startsWith("/") && new File(args[i]).isFile()){
          Path path=sh.getWorkingDirectory().relativize(Paths.get(args[i]));
          args[i]=path.toString();
        }
      }
      break;
    default:
      fail(res,"unknown executable: "+args[0]);
      return res;
    }
    if (sh!=null){
      String cmd=args[0];
      for(int i=1;i<args.length;i++){
        cmd+=" "+args[i];
      }
      sh.send("%s",cmd);
      sh.send("exit");
      p=sh;
    }
    for(;;){
      Message msg=p.recv();
      if (msg==null){
        fail(res,"unexpected null message");
      }
      if (msg.getFormat().equals("stderr: %s")||msg.getFormat().equals("stdout: %s")){
        String line=msg.getArgs()[0].toString();
        Matcher lineMatcher = TIME_PATTERN.matcher(line);
        if (lineMatcher.find()){
          String key = lineMatcher.group(2);
          int value = Integer.parseInt(lineMatcher.group(3));
          res.times.put(key, value);
        }
      }
      res.log.add(msg);
      if (msg.getFormat().equals("exit %d")){
        int n=(Integer)msg.getArg(0);
        if (n>0){
          res.verdict=TestReport.Verdict.Error;
        }
        break;
      }
      if (((String)msg.getArg(0)).contains("The final verdict is Pass")){
        if (res.verdict!=null && res.verdict != TestReport.Verdict.Pass) fail(res,"inconsistent repeated verdict ("+res.verdict+")");
        else res.verdict=TestReport.Verdict.Pass;
      }
      if (((String)msg.getArg(0)).contains("The final verdict is Fail")){
        if (res.verdict!=null && res.verdict != TestReport.Verdict.Fail) fail(res,"inconsistent repeated verdict ("+res.verdict+")");
        else res.verdict=TestReport.Verdict.Fail;
      }
      if (((String)msg.getArg(0)).startsWith("method verdict")){
        String line[]=((String)msg.getArg(0)).split(" ");
        switch(line[3]){
        case "PASS":
          res.pass_methods.add(line[2]);
          break;
        case "FAIL":
          res.fail_methods.add(line[2]);
          break;
        default:
          fail(res,"bad method verdict");
        }
      }
    }
    if (res.verdict==null) fail(res,"missing verdict");
    return res;
  }

}
