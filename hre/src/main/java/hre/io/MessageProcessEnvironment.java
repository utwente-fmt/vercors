package hre.io;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

public class MessageProcessEnvironment {
  private HashMap<String, String> environment = new HashMap<>();
  private List<String> path = new ArrayList<>();
  private Path workingDirectory;
  private List<String> args = new ArrayList<>();
  private String process;

  public MessageProcessEnvironment(String process) {
    this.process = process;
  }

  public MessageProcessEnvironment copy() {
    MessageProcessEnvironment result = new MessageProcessEnvironment(process);
    result.environment.putAll(environment);
    result.path.addAll(path);
    result.workingDirectory = workingDirectory;
    result.args.addAll(args);
    return result;
  }

  public String getProcess() {
    return process;
  }

  public void setEnvironmentVar(String key, String value) {
    environment.put(key, value);
  }

  public void addPath(String newPath) {
    path.add(newPath);
  }

  public void setWorkingDirectory(Path workingDirectory) {
    this.workingDirectory = workingDirectory;
  }

  public void setTemporaryWorkingDirectory() throws IOException {
    this.workingDirectory = Files.createTempDirectory(null);
  }

  public Path getWorkingDirectory() {
    return this.workingDirectory;
  }

  public void addArg(String arg) {
    this.args.add(arg);
  }

  public void addArg(String arg, String argArg) {
    this.addArg(arg);
    this.addArg(argArg);
  }

  public List<String> getArgs() {
    return this.args;
  }

  public MessageProcessEnvironment withArgs(String... args) {
    MessageProcessEnvironment newEnv = this.copy();

    for(String arg : args) {
      newEnv.addArg(arg);
    }

    return newEnv;
  }

  public MessageProcess startProcess() {
    String path = String.join(File.pathSeparator, this.path);

    environment.put("PATH", path);

    String[] env = environment.entrySet().stream()
            .map((e) -> String.format("%s=%s", e.getKey(), e.getValue()))
            .toArray(String[]::new);

    List<String> argv = new LinkedList<>(this.args);
    argv.add(0, this.process);

    return new MessageProcess(workingDirectory, argv.toArray(new String[0]), env);
  }
}
