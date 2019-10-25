package vct.antlr4.parser;

public class MissingCase extends Error {

  /**
   * 
   */
  private static final long serialVersionUID = 5651221269025765106L;

  public MissingCase(String format,Object ... args){
    super(String.format(format,args));
  }
}
