// -*- tab-width:2 ; indent-tabs-mode:nil -*-
package vct.boogie;

import hre.ast.TrackingOutput;
import hre.ast.TrackingTree;
import hre.config.IntegerSetting;
import hre.config.StringSetting;
import hre.io.Message;
import hre.io.MessageProcess;
import hre.io.MessageProcessEnvironment;
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
    try {
      MessageProcessEnvironment shell = Configuration.getBoogie();
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
      shell.addArg("/timeLimit:" + timeout);
      shell.addArg("/xml:" + boogie_xml_file.getName());
      shell.addArg(boogie_input_file.getName());
      MessageProcess process = shell.startProcess();
      return new BoogieReport("Boogie", process, boogie_xml_file, tree);
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
    try {
      MessageProcessEnvironment shell = Configuration.getDafny();
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
      shell.addArg("/compile:0");
      shell.addArg("/timeLimit:" + timeout);
      shell.addArg(boogie_input_file.getName());
      return new DafnyReport(shell.startProcess(), tree);
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
    try {
      MessageProcessEnvironment shell=Configuration.getChalice();
      File shell_dir=shell.getWorkingDirectory().toFile();
      Output("Checking with Chalice");
      File chalice_input_file=File.createTempFile("chalice-input",".chalice",shell_dir);
      chalice_input_file.deleteOnExit();
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
      TrackingTree tree=chalice_code.close();
      shell.addArg("-boogieOpt:timeLimit:" + timeout);
      shell.addArg("-noTermination");
      shell.addArg(chalice_input_file.getName());
      return new ChaliceReport(shell.startProcess(),((ChalicePrinter)printer).refutes,tree);
    } catch (Exception e) {
      Warning("error: ");
      DebugException(e);
      return null;
    }
  }
}