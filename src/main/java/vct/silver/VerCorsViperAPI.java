package vct.silver;

import hre.ast.HREOrigins;
import hre.ast.MessageOrigin;
import hre.ast.Origin;

import java.io.PrintWriter;
import java.nio.file.Path;
import java.util.*;

import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.Axiom;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.type.Type;
import vct.col.ast.util.ASTFactory;
import viper.api.*;

public class VerCorsViperAPI extends ViperAPI<
    Origin, Type, ASTNode, ASTNode,
    Method, Axiom, ProgramUnit> {

  public Hashtable<String,Set<Origin>> refuted=new Hashtable<String,Set<Origin>>();

  private VerCorsProgramFactory programFactory;

  HashSet<Origin> getSatCheckAsserts() {
    return new HashSet<>(programFactory.satCheckAsserts);
  }
  
  private VerCorsViperAPI(HREOrigins origin, VerCorsTypeFactory type,
                          VerCorsExpressionFactory expr, VerCorsStatementFactory stat,
                          VerCorsProgramFactory prog) {
    super(origin, type, expr, stat, prog);
    programFactory = prog;
  }

  public static VerCorsViperAPI get() {
    HREOrigins origin=new HREOrigins();
    ASTFactory<Object> create=new ASTFactory<Object>();
    create.setOrigin(new MessageOrigin("VerCorsViperAPI"));
    VerCorsTypeFactory type=new VerCorsTypeFactory(create);
    VerCorsExpressionFactory expr=new VerCorsExpressionFactory(create);
    VerCorsStatementFactory stat=new VerCorsStatementFactory(create);
    VerCorsProgramFactory prog=new VerCorsProgramFactory(create);
    VerCorsViperAPI res=new VerCorsViperAPI(origin, type, expr, stat, prog);
    prog.refuted=res.refuted;
    return res;
  }

  @Override
  public List<? extends ViperError<Origin>> verify(Path z3Path,
      Properties z3Settings, ProgramUnit program,
      VerificationControl<Origin> control) {
    throw new Error("Using VerCors backends for Viper is not implemented.");
  }

  @Override
  public void write_program(PrintWriter pw, ProgramUnit program) {
    pw.printf("%s",program);
  }

}
