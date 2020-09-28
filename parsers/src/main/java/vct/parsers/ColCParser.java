package vct.parsers;

import static hre.lang.System.*;

import java.io.*;
import java.nio.file.Path;
import java.nio.file.Paths;

import hre.config.Configuration;
import hre.lang.HREExitException;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import scala.NotImplementedError;
import vct.col.ast.stmt.decl.ProgramUnit;

/**
 * Parse specified code and convert the contents to COL.
 */
public class ColCParser extends ColIParser {

    @Override
    public ProgramUnit parse(CharStream input, String file_name) {
        throw new NotImplementedError(
                "Cannot parse a C file from a CharStream: " +
                "the preprocessor requires a byte stream.");
    }

    @Override
    public ProgramUnit parse(InputStream stream, String file_name) {
        try {
            Runtime runtime = Runtime.getRuntime();

            StringBuilder command = new StringBuilder(Configuration.cpp_command.get());
            command.append(" -nostdinc -nocudainc -nocudalib -isystem ").append(Configuration.getCIncludePath().getAbsolutePath());

            Path filePath = Paths.get(file_name).toAbsolutePath().getParent();

            if(filePath != null) {
                // Not quite correct, because this allows <> includes to see local files as well as "" includes.
                command.append(" -I").append(filePath.toString());
            }

            for (String p : Configuration.cpp_include_path) {
                command.append(" -I").append(p);
            }
            for (String p : Configuration.cpp_defines) {
                command.append(" -D").append(p);
            }
            command.append(" -");

            Progress("pre-processing command line: %s", command.toString());

            final Process process = runtime.exec(command.toString());

            new Thread(() -> {
                try {
                    byte[] buf = new byte[4096];
                    int read = stream.read(buf);

                    while(read != -1) {
                        process.getOutputStream().write(buf, 0, read);
                        read = stream.read(buf);
                    }

                    process.getOutputStream().close();
                } catch(IOException e) {
                    DebugException(e);
                }
            }).start();

            Thread t = new Thread(() -> {
                BufferedReader err = new BufferedReader(new InputStreamReader(process.getErrorStream()));
                String s;
                try {
                    while ((s = err.readLine()) != null) {
                        Output("%s", s);
                    }
                } catch (IOException e) {
                    DebugException(e);
                }
            });
            t.setDaemon(true);
            t.start();
            return super.parse(CharStreams.fromStream(process.getInputStream()), file_name);
        } catch (Exception e) {
            DebugException(e);
            Abort("Exception %s while parsing %s", e.getClass(), file_name);
        }
        return null;
    }
}

