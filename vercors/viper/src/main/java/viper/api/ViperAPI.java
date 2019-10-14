package viper.api;

import java.io.PrintWriter;
import java.nio.file.Path;
import java.util.List;
import java.util.Properties;

public abstract class ViperAPI<O,Err,T,E,S,DFunc,DAxiom,P> {

  public final OriginFactory<O> origin;
  public final TypeFactory<T> _type;
  public final ExpressionFactory<O,T,E> expr;
  public final StatementFactory<O,T,E,S> stat;
  public final ProgramFactory<O,Err,T,E,S,DFunc,DAxiom,P> prog;
  
  public ViperAPI(
      OriginFactory<O> origin,
      TypeFactory<T> type,
      ExpressionFactory<O,T,E> expr,
      StatementFactory<O,T,E,S> stat,
      ProgramFactory<O,Err,T,E,S,DFunc,DAxiom,P> prog){
    this.origin=origin;
    this._type=type;
    this.expr=expr;
    this.stat=stat;
    this.prog=prog;
  }
  
  /**
   * Verify a program.
   * @param tool_home The root directory of the third party tools.
   * @param program The program to be verified.
   * @return test report
   */
  public abstract List<? extends ViperError<O>> verify(
      Path z3Path,
      Properties z3Settings,
      P program,
      java.util.Set<O> reachable,
      VerificationControl<O> control);
  
  public abstract void write_program(PrintWriter pw,P program);


}
