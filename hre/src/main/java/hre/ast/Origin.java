// -*- tab-width:2 ; indent-tabs-mode:nil -*-
package hre.ast;

import static hre.lang.System.Output;
import static hre.lang.System.Debug;

/** 
 * This interface allows tracking the origin of
 * AST nodes through transformations to the original file(s).
 */
public abstract class Origin {
  public abstract void report(String level, Iterable<String> message);
  public abstract void report(String level, String ... message);
  public void report(String level, String format, Object ... args){
    report(level,String.format(format, args));
  }
}

