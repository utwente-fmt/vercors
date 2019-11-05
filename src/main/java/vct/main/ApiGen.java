package vct.main;

import vct.antlr4.parser.Parsers;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.syntax.JavaDialect;
import vct.col.syntax.JavaSyntax;

import java.io.PrintWriter;

public class ApiGen {

  public static void main(String args[]){
    hre.lang.System.setOutputStream(System.out, hre.lang.System.LogLevel.Info);
    hre.lang.System.setErrorStream(System.err, hre.lang.System.LogLevel.Info);

    ProgramUnit api=Parsers.parseFile(args[0]);
    JavaSyntax.getJava(JavaDialect.JavaVerCors).print(hre.lang.System.getLogLevelOutputWriter(hre.lang.System.LogLevel.Info),api);
  }
}
