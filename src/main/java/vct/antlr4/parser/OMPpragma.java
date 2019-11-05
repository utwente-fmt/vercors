package vct.antlr4.parser;

import java.util.Arrays;

public class OMPpragma implements OMPelement {

  public enum Kind { Parallel , ParallelFor , For , Simd, Sections, Section};
  
  public final Kind kind;
  
  public final OMPoption options[];
  
  public OMPpragma(Kind kind,OMPoption ... options){
    this.kind=kind;
    this.options=Arrays.copyOf(options,options.length);
  }
  
}
