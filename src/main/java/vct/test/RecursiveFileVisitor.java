package vct.test;

import hre.util.TestReport;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

import static hre.lang.System.*;

public class RecursiveFileVisitor extends SimpleFileVisitor<Path> {
  public static final String OPTION_START = "//::";
  public static final Set<String> TESTCASE_EXTENSIONS = new HashSet<>(Arrays.asList("c", "java", "pvl", "sil", "cu"));

  public final HashMap<String, Case> testsuite = new HashMap<String, Case>();
  public HashSet<Path> unmarked = new HashSet<Path>();
  public boolean delayedFail = false;

  public static String extension(Path path) {
    Path file = path.getFileName();
    String name = file.toString();
    int dot = name.lastIndexOf('.');
    if (dot < 0) {
      return "";
    } else {
      return name.substring(dot + 1);
    }
  }


  @Override
  public FileVisitResult visitFile(Path file, BasicFileAttributes attrs)
          throws IOException {
    if (!TESTCASE_EXTENSIONS.contains(extension(file))) {
      return FileVisitResult.CONTINUE;
    }

    BufferedReader is = new BufferedReader(new InputStreamReader(new FileInputStream(file.toFile())));
    String line;
    HashSet<String> cases = new HashSet<String>();
    while ((line = is.readLine()) != null) {
      line = line.trim();
      if (!line.startsWith(OPTION_START)) continue;
      String[] cmds = line.substring(OPTION_START.length()).trim().split("[ ]+");
      switch (cmds[0]) {
        case "case":
        case "cases":
          cases.clear();
          for (int i = 1; i < cmds.length; i++) {
            cases.add(cmds[i]);
            Case test = testsuite.get(cmds[i]);
            if (test == null) {
              test = new Case();
              testsuite.put(cmds[i], test);
            }
            test.files.add(file);
          }
          break;
        case "tool":
        case "tools":
          for (String test : cases) {
            Case tc = testsuite.get(test);
            if (!tc.tools.isEmpty()) {
              Warning("%s: tools for test %s already set.", file, test);
              delayedFail = true;
            }
            for (int i = 1; i < cmds.length; i++) {
              tc.tools.add(cmds[i]);
            }
          }
          break;
        case "verdict":
          for (String test : cases) {
            Case tc = testsuite.get(test);

            if (cmds.length != 2) {
              Warning("In file %s \"verdict\" has trailing words. The verdict keyword can only be followed by Pass, Fail, or Error", file.toAbsolutePath());
              delayedFail = true;
            }

            switch(cmds[1]) {
              case "Pass":
                tc.verdict = TestReport.Verdict.Pass;
                break;
              case "Fail":
                tc.verdict = TestReport.Verdict.Fail;
                break;
              case "Error":
                tc.verdict = TestReport.Verdict.Error;
                break;
              default:
                Warning("Invalid verdict %s", cmds[1]);
                delayedFail = true;
            }
          }
          break;
        case "suites":
        case "suite":
          for (String test : cases) {
            Case tc = testsuite.get(test);
            for (int i = 1; i < cmds.length; i++) {
              tc.suites.add(cmds[i]);
            }
          }
          break;
        case "option":
        case "options":
          for (int i = 1; i < cmds.length; i++) {
            for (String test : cases) {
              Case tc = testsuite.get(test);
              tc.options.add(cmds[i]);
            }
          }
          break;
        case "pass":
          for (int i = 1; i < cmds.length; i++) {
            for (String test : cases) {
              Case tc = testsuite.get(test);
              if(cmds[i].equals("any")) {
                tc.pass_non_fail = true;
              } else {
                tc.pass_methods.add(cmds[i]);
              }
            }
          }
          break;
        case "fail":
          for (int i = 1; i < cmds.length; i++) {
            for (String test : cases) {
              Case tc = testsuite.get(test);
              tc.fail_methods.add(cmds[i]);
              tc.verdict = TestReport.Verdict.Fail;
            }
          }
          break;
        default:
          Warning("ignoring %s in %s: %s", cmds[0], file, line);
      }
    }
    is.close();
    if (cases.isEmpty()) {
      unmarked.add(file);
    }
    return FileVisitResult.CONTINUE;
  }

  @Override
  public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs)
          throws IOException {
    // If the last path element is exactly "private", disregard the directory.
    if(dir.endsWith("private")) {
      return FileVisitResult.SKIP_SUBTREE;
    } else {
      return FileVisitResult.CONTINUE;
    }
  }

  @Override
  public FileVisitResult postVisitDirectory(Path dir, IOException e)
          throws IOException {
    if (e == null) {
      return FileVisitResult.CONTINUE;
    } else {
      // directory iteration failed
      throw e;
    }
  }


}
