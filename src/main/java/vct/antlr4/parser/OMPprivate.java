package vct.antlr4.parser;

import java.util.Arrays;

class OMPprivate extends OMPoption {

  public final String vars[];
  
  public OMPprivate(String vars[]) {
    super(Kind.Private);
    this.vars=Arrays.copyOf(vars, vars.length);
  }
  
}