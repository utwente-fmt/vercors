package hre.io;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;

public class UnionContainer implements Container {

  private static Container empty[]=new Container[0];
  
  private Container path[];
  
  public UnionContainer(Container ... path){
    this.path=path;
  }

  public UnionContainer(ArrayList<Container> path) {
	this.path=path.toArray(empty);
  }

@Override
  public boolean contains(String name) {
    for(int i=0;i<path.length;i++){
      if (path[i].contains(name)) return true;
    }
    return false;
  }
  
  @Override
  public InputStream read(String name) throws IOException {
    for(int i=0;i<path.length;i++){
      if (path[i].contains(name)) return path[i].read(name);
    }
    return null;
  }
  
  @Override
  public long size(String name) {
    for(int i=0;i<path.length;i++){
      if (path[i].contains(name)) return path[i].size(name);
    }
    return 0;
  }

@Override
public String findFile(String name) {
    for(int i=0;i<path.length;i++){
    	String res=path[i].findFile(name);
    	if (res!=null) return res;
    }	
	return null;
}

}
