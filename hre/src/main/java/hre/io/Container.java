package hre.io;

import java.io.IOException;
import java.io.InputStream;

public interface Container {

    public boolean contains(String name);

    public InputStream read(String name) throws IOException;

    public long size(String name);

    public String findFile(String name);

}
