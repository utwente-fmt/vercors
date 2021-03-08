package vct.silver;

import hre.ast.HREOrigins;
import vct.col.ast.stmt.decl.ProgramUnit;
import hre.ast.AssertOrigin;
import hre.ast.RestOfContractOrigin;
import viper.api.*;

import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.text.SimpleDateFormat;
import java.util.*;

import hre.ast.Origin;
import hre.lang.HREError;
import vct.logging.MessageFactory;
import vct.logging.PassAddVisitor;
import vct.logging.PassReport;
import vct.logging.TaskBegin;
import hre.config.Configuration;

import static hre.lang.System.DebugException;
import static hre.lang.System.Output;

public class SilverBackend {
  public static ViperAPI<Origin, ?,?,?,?,?,?>
  getVerifier(String tool) {
    return getViperVerifier(tool);
  }
  
  public static ViperAPI<Origin, ?,?,?,?,?,?>
  getViperVerifier(String tool){
    ViperAPI<Origin, ?,?,?,?,?,?> verifier = null;
    switch(tool.trim()) {
    case "carbon":
      verifier = new CarbonVerifier<>(new HREOrigins());
      break;
    case "silicon":
      verifier = new SiliconVerifier<>(new HREOrigins());
      break;
    case "parser":
      verifier = new SilverImplementation<>(new HREOrigins());
      break;
    default:
      throw new HREError("cannot guess the main class of %s",tool);    
    }
    return verifier;
  }
  
  public static
  PassReport TestSilicon(PassReport given, String tool) {
    ViperAPI<Origin, ?, ?, ?, ?, ?, ?> verifier=getVerifier(tool);
    // We redirect through a new method, because we need to convince java the program type is consistent. The most brief
    // way to capture a  wildcard ("?") type is via a method.
    return TestSilicon(given, tool, verifier);
  }

  public static <Program> PassReport TestSilicon(PassReport given, String tool, ViperAPI<Origin, ?, ?, ?, ?, ?, Program> verifier) {
    //hre.System.Output("verifying with %s backend",silver_module.get());
    ProgramUnit arg=given.getOutput();
    PassReport report=new PassReport(arg);
    report.add(new PassAddVisitor(given));
    report.setOutput(given.getOutput());
    MessageFactory log=new MessageFactory(new PassAddVisitor(report));
    TaskBegin verification=log.begin("Viper verification");

    hre.lang.System.Progress("verifying with %s %s backend", "builtin", tool);
    //verifier.set_detail(Configuration.detailed_errors.get());
    VerCorsViperAPI vercors=VerCorsViperAPI.get();
    Program program = vercors.prog.convert(verifier,arg);
    log.phase(verification,"Backend AST conversion");
    String fname= Configuration.backend_file.get();
    if (fname!=null){
      PrintWriter pw=null;
      try {
        pw = new java.io.PrintWriter(new java.io.File(fname));
        Date now = new Date();
        pw.print("// Generated on ");
        pw.print(new SimpleDateFormat("yyyy-MM-dd").format(now));
        pw.print(" at ");
        pw.println(new SimpleDateFormat("HH:mm:ss").format(now));
        verifier.write_program(pw,program);
      } catch (FileNotFoundException e) {
        DebugException(e);
      } finally {
        if (pw!=null) pw.close();
      }
    }

    Properties settings=new Properties();
    if (tool.startsWith("silicon")){
      //settings.setProperty("smt.soft_timeout",silicon_z3_timeout.get()+"");
    }
    ViperControl control=new ViperControl(log);
    try {
      // Call into Viper to verify!
      List<? extends ViperError<Origin>> rawErrors = verifier.verify(
              Configuration.getZ3Path().toPath(),
              settings,
              program,
              control
      );
      // Put it in a new list so we can add ViperErrors to it ourselves
      List<ViperError<Origin>> errors = new ArrayList<>(rawErrors);

      // Filter SatCheck errors that are to be expected
      // Also collect restOfContractOrigins to determine which contract failed other checks
      // (i.e. maybe an array access was out of bounds, causing an error to be signaled from
      // the contract. this error in turn hides the error from the assert, which is fine,
      // but we need to know when this happens)
      HashSet<AssertOrigin> satCheckAssertsSeen = new HashSet<>();
      HashSet<RestOfContractOrigin> restOfContractsSeen = new HashSet<>();
      errors.removeIf(e -> {
        for (int i = 0; i < e.getExtraCount(); i++) {
          Origin origin = e.getOrigin(i);
          if (origin instanceof AssertOrigin) {
            satCheckAssertsSeen.add((AssertOrigin) origin);
            return true;
          } else if (origin instanceof RestOfContractOrigin) {
            restOfContractsSeen.add((RestOfContractOrigin) origin);
            return true;
          }
        }
        return false;
      });

      // For each satCheckAssert that did not error, if its contract was besides that well formed
      // (i.e. we didn't get an error from the contract inhale),
      // it means the contract is unsatisfiable.
      // Therefore, we warn the user that their contracts are unsound
      HashSet<Origin> expectedSatCheckAsserts = vercors.getSatCheckAsserts();
      for (Origin expectedSatCheckAssert : expectedSatCheckAsserts) {
        AssertOrigin assertOrigin = (AssertOrigin) expectedSatCheckAssert;
        if (!satCheckAssertsSeen.contains(assertOrigin) && !restOfContractsSeen.contains(assertOrigin.restOfContractOrigin)) {
          ViperErrorImpl<Origin> error = new ViperErrorImpl<Origin>(expectedSatCheckAssert, "method.precondition.unsound:method.precondition.false");
          errors.add(error);
        }
      }

      if (errors.size() == 0) {
        Output("Success!");
      } else {
        Output("Errors! (%d)", errors.size());
        for(ViperError<Origin> e:errors){
          log.error(e);
        }
      }
    } catch (Exception e){
      log.exception(e);
      Output(e.toString());
    } finally {
      control.done();
    }
    log.end(verification);
    return report;
  }

}
