package hre.io;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;

public class DirContainer implements Container {

  private File dir;
  
  public DirContainer(File dir){
    this.dir=dir;
  }
  
  @Override
  public boolean contains(String name) {
    File f=new File(dir,name);
    return f.exists() && f.isFile();
  }
  @Override
  public String findFile(String name) {
    File f=new File(dir,name);
    if (f.exists() && f.isFile()){
    	return f.getAbsolutePath();
    } else {
    	return null;
    }
  }

  @Override
  public InputStream read(String name) throws FileNotFoundException {
    File f=new File(dir,name);
    return new FileInputStream(f);
  }

  @Override
  public long size(String name) {
    File f=new File(dir,name);
    return f.length();
  }

}
