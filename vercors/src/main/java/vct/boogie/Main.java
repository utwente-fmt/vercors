// -*- tab-width:2 ; indent-tabs-mode:nil -*-
package vct.boogie;

import hre.ast.TrackingOutput;
import hre.ast.TrackingTree;
import hre.config.IntegerSetting;
import hre.config.StringSetting;
import hre.io.Message;
import hre.io.MessageProcess;
import hre.io.SplittingOutputStream;

import java.io.*;

import vct.col.ast.stmt.decl.ProgramUnit;
import vct.util.*;
import static hre.lang.System.*;

/**
 * This class contains the main procedures of the Boogie/Chalice back ends.
 * 
 * @author Stefan Blom
 *
 */
public class Main {
  // The default timeout is set to half an hour, to give Z3 plenty of time yet avoid run-aways.
  public static IntegerSetting boogie_timeout=new IntegerSetting(1800);

  /**
   * Generate Boogie code and optionally verify a class.
   * @param arg The class for which code must be generated.
   */
  public static BoogieReport TestBoogie(ProgramUnit arg){
    int timeout=boogie_timeout.get();
    MessageProcess shell=Configuration.getShell(z3_module.get(),boogie_module.get());
    try {
      File boogie_input_file=File.createTempFile("boogie-input",".bpl",shell.getWorkingDirectory().toFile());
      boogie_input_file.deleteOnExit();
      final PrintWriter boogie_input;
      
      if (vct.util.Configuration.backend_file.get()==null){
        boogie_input=new PrintWriter(boogie_input_file);
      } else {
        OutputStream temp=new FileOutputStream(boogie_input_file);
        File encoded_file=new File(vct.util.Configuration.backend_file.get());
        OutputStream encoded=new FileOutputStream(encoded_file);
        boogie_input=new PrintWriter(new SplittingOutputStream(temp,encoded));
      }
      TrackingOutput boogie_code=new TrackingOutput(boogie_input,true);
      BoogieSyntax.getBoogie().print(boogie_code,arg);
      TrackingTree tree=boogie_code.close();
      File boogie_xml_file=File.createTempFile("boogie-output",".xml",shell.getWorkingDirectory().toFile());
      boogie_xml_file.deleteOnExit();
      //shell.send("which boogie");
      //shell.send("pwd");
      //shell.send("ls -al");
      shell.send("boogie /timeLimit:%s /xml:%s %s",timeout,boogie_xml_file.getName(),boogie_input_file.getName());
      //shell.send("ls -al");
      shell.send("exit");
      BoogieReport output=new BoogieReport("Boogie",shell,boogie_xml_file,tree);
      return output;
    } catch (Exception e) {
      DebugException(e);
      hre.lang.System.Abort("internal error");
      return null;
    }
  }

  /**
   * Generate Dafny code and optionally verify a class.
   * @param arg The class for which code must be generated.
   */
  public static DafnyReport TestDafny(ProgramUnit arg){
    int timeout=boogie_timeout.get();
    MessageProcess shell=Configuration.getShell(z3_module.get(),dafny_module.get());
    try {
      File boogie_input_file=File.createTempFile("dafny-input",".dfy",shell.getWorkingDirectory().toFile());
      boogie_input_file.deleteOnExit();
      final PrintWriter boogie_input;
      
      if (vct.util.Configuration.backend_file.get()==null){
        boogie_input=new PrintWriter(boogie_input_file);
      } else {
        OutputStream temp=new FileOutputStream(boogie_input_file);
        File encoded_file=new File(vct.util.Configuration.backend_file.get());
        OutputStream encoded=new FileOutputStream(encoded_file);
        boogie_input=new PrintWriter(new SplittingOutputStream(temp,encoded));
      }
      TrackingOutput boogie_code=new TrackingOutput(boogie_input,true);
      BoogieSyntax.getDafny().print(boogie_code,arg);
      TrackingTree tree=boogie_code.close();
      //File boogie_xml_file=File.createTempFile("dafny-output",".xml",shell.shell_dir.toFile());
      //boogie_xml_file.deleteOnExit();
      //shell.send("which boogie");
      //shell.send("pwd");
      //shell.send("ls -al");
      //shell.send("dafny /compile:0 /timeLimit:%s /xml:%s %s",timeout,boogie_xml_file.getName(),boogie_input_file.getName());
      shell.send("dafny /compile:0 /timeLimit:%s %s",timeout,boogie_input_file.getName());
      //shell.send("ls -al");
      shell.send("exit");
      DafnyReport output=new DafnyReport(shell,tree);
      return output;
    } catch (Exception e) {
      DebugException(e);
      hre.lang.System.Abort("internal error");
      return null;
    }
  }
  /**
   * Pretty print a Chalice program and optionally verify it.
   * 
   * @param program AST of the Chalice program.
   *
   */
  public static ChaliceReport TestChalice(final ProgramUnit program){
    int timeout=boogie_timeout.get();
    MessageProcess shell=Configuration.getShell(z3_module.get(),boogie_module.get(),chalice_module.get());
    File shell_dir=shell.getWorkingDirectory().toFile();
    Output("Checking with Chalice");
    try {
      File chalice_input_file=File.createTempFile("chalice-input",".chalice",shell_dir);
      //if (!vct.util.Configuration.keep_temp_files.get()){
        chalice_input_file.deleteOnExit();
      //}
      final PrintWriter chalice_input;
      
      if (vct.util.Configuration.backend_file.get()==null){
        chalice_input=new PrintWriter(chalice_input_file);
      } else {
        OutputStream temp=new FileOutputStream(chalice_input_file);
        File encoded_file=new File(vct.util.Configuration.backend_file.get());
        OutputStream encoded=new FileOutputStream(encoded_file);
        chalice_input=new PrintWriter(new SplittingOutputStream(temp,encoded));
      }
      final TrackingOutput chalice_code=new TrackingOutput(chalice_input,true);
      AbstractBoogiePrinter printer=BoogieSyntax.getChalice().print(chalice_code, program);
      /*
      final ChalicePrinter printer=new ChalicePrinter(chalice_code);
      
      for(ASTClass cl:program.classes()){
        if (cl.getStaticCount()>0){
          throw new Error("class "+cl.getName()+" has static entries");
        } else {
          cl.accept(printer);
        }
      }
      */
      TrackingTree tree=chalice_code.close();
        //shell.send("which chalice");
        //shell.send("pwd");
        //shell.send("ls -al");
        shell.send("chalice -boogieOpt:timeLimit:%d -noTermination %s",timeout,chalice_input_file.getName());
        //shell.send("ls -al");
        shell.send("exit");
        ChaliceReport output=new ChaliceReport(shell,((ChalicePrinter)printer).refutes,tree);
        return output;
    } catch (Exception e) {
      Warning("error: ");
      DebugException(e);
      return null;
    }
  }

  /**
   * Pretty print a Chalice program and optionally verify it.
   * 
   * @param program AST of the Chalice program.
   *
   */
  public static SiliconReport TestSilicon(final ProgramUnit program){
    MessageProcess shell=Configuration.getShell(z3_module.get(),silicon_module.get());
    File shell_dir=shell.getWorkingDirectory().toFile();
    Output("Checking with chalice2sil and silicon");
    try {
      File chalice_input_file=File.createTempFile("chalice-input",".chalice",shell_dir);
      //if (!vct.util.Configuration.keep_temp_files.get()){
        chalice_input_file.deleteOnExit();
      //}
      final PrintWriter chalice_input;
      
      if (vct.util.Configuration.backend_file.get()==null){
        chalice_input=new PrintWriter(chalice_input_file);
      } else {
        OutputStream temp=new FileOutputStream(chalice_input_file);
        File encoded_file=new File(vct.util.Configuration.backend_file.get());
        OutputStream encoded=new FileOutputStream(encoded_file);
        chalice_input=new PrintWriter(new SplittingOutputStream(temp,encoded));
      }
      final TrackingOutput chalice_code=new TrackingOutput(chalice_input,true);
      BoogieSyntax.getChalice().print(chalice_code, program);
      TrackingTree tree=chalice_code.close();
      File silicon_xml_file=File.createTempFile("silicon-output",".xml",shell.getWorkingDirectory().toFile());
      silicon_xml_file.deleteOnExit();
      shell.send("chalice --xml %s %s",silicon_xml_file.getName(),chalice_input_file.getName());
      shell.send("exit");
      SiliconReport output=new SiliconReport(shell,silicon_xml_file,tree);
      return output;
    } catch (Exception e) {
      Warning("error: ");
      DebugException(e);
      return null;
    }

  }
}


