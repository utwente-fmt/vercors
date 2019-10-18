package vct.verifast;

import java.io.*;

import hre.io.MessageProcess;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.syntax.JavaDialect;
import vct.col.syntax.JavaSyntax;
import vct.util.Configuration;
import hre.ast.TrackingOutput;
import hre.ast.TrackingTree;
import hre.config.StringSetting;
import hre.io.SplittingOutputStream;

public class Main {

  public static StringSetting verifast_module;
  
  static {
    verifast_module=new StringSetting("verifast/13.11.14");
  }

  
  public static VeriFastReport TestVerifast(ProgramUnit arg){
    MessageProcess shell=Configuration.getShell(verifast_module.get());
    try {
      File input_file=File.createTempFile("verifast-input",".java",shell.getWorkingDirectory().toFile());
      input_file.deleteOnExit();
      final PrintWriter input;
      
      if (vct.util.Configuration.backend_file.get()==null){
        input=new PrintWriter(input_file);
      } else {
        OutputStream temp=new FileOutputStream(input_file);
        File encoded_file=new File(vct.util.Configuration.backend_file.get());
        OutputStream encoded=new FileOutputStream(encoded_file);
        input=new PrintWriter(new SplittingOutputStream(temp,encoded));
      }
      TrackingOutput vf_input=new TrackingOutput(input,true);
      JavaSyntax syntax=JavaSyntax.getJava(JavaDialect.JavaVeriFast);
      syntax.print(vf_input,arg);
      TrackingTree tree=vf_input.close();
      shell.send("verifast -shared %s",input_file.getName());
      shell.send("exit");
      VeriFastReport output=new VeriFastReport(shell,tree);
      return output;
    } catch (Exception e) {
      hre.lang.System.DebugException(e);
      hre.lang.System.Abort("internal error");
      return null;
    }
  }

}
