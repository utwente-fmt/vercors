package hre.io;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

import static hre.lang.System.*;

public class JarContainer implements Container {
  
  private JarFile jar;

  public JarContainer(File jarFile){
    try {
      jar = new JarFile(jarFile);
    } catch (IOException e) {
      Fail("Could not open jar file %s: %s",jarFile,e);
    }
  }

  @Override
  public boolean contains(String name) {
    return jar.getEntry(name)!=null;
  }

  @Override
  public InputStream read(String name) throws IOException {
    JarEntry entry = jar.getJarEntry(name);
    if (entry==null){
      Fail("cannot read: jar file %s does not contain %s",jar.getName(),name);
    }
    return jar.getInputStream(entry);
  }

  @Override
  public long size(String name) {
    JarEntry entry = jar.getJarEntry(name);
    if (entry==null){
      Fail("cannot get size: jar file %s does not contain %s",jar.getName(),name);
    }
    return entry.getSize();
  }

@Override
public String findFile(String name) {
	return null;
}

}
