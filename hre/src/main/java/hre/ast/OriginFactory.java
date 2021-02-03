package hre.ast;

public interface OriginFactory<O> {

  public O message(String fmt,Object ... args);
  
  public O file(String file,int line,int col);
  
  public O file(String file,int ln1,int c1, int ln2, int c2);
  
}
