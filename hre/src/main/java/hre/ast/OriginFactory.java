package hre.ast;

import java.nio.file.Path;

public interface OriginFactory<O> {

  public O message(String fmt,Object ... args);
  
  public O file(Path file, int line, int col);
  
  public O file(Path file,int ln1,int c1, int ln2, int c2);
  
}
