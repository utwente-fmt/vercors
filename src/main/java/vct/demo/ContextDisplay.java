package vct.demo;

import hre.ast.FileOrigin;

import static hre.lang.System.Output;

/**
 * This class demonstrates how to use the context class.
 * 
 * @author sccblom
 *
 */
public class ContextDisplay {

  public static void main(String args[]){
    hre.lang.System.setOutputStream(System.out, hre.lang.System.LogLevel.Info);
    hre.lang.System.setErrorStream(System.err, hre.lang.System.LogLevel.Info);

    FileOrigin.add(args[0],true);
    int begin=Integer.parseInt(args[1]);
    int end=Integer.parseInt(args[2]);
    FileOrigin o=new FileOrigin(args[0],begin,1,end,1);
    Output("origin %s is",o);
    o.printContext(2,2);
  }
}
