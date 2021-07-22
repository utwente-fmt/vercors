package hre.io;

import java.io.IOException;
import java.io.InputStream;

public interface Container {

  boolean contains(String name);
  
  InputStream read(String name) throws IOException;
  
  long size(String name);

  String findFile(String name);
  
}
