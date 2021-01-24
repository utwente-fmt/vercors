package vct.test;

import hre.util.TestReport;

import java.lang.reflect.Array;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

public class Case {
  public HashSet<String> tools=new HashSet<String>();
  public ArrayList<String> options=new ArrayList<String>();
  public HashSet<Path> files=new HashSet<Path>();
  public TestReport.Verdict verdict = null;
  public HashSet<String> suites=new HashSet<String>();

  public HashSet<String> pass_methods=new HashSet<String>();
  public HashSet<String> fail_methods=new HashSet<String>();
  public boolean pass_non_fail = false;

  public List<String> toArgs() {
    ArrayList<String> args = new ArrayList<>();
    for (String tool : tools) {
      args.add("--" + tool);
    }
    for (String option : options) {
      args.add(option);
    }
    for (Path file : files) {
      args.add(file.toString());
    }
    return args;
  }
}