package vct.main;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashSet;

class Testcase {
  public HashSet<String> tools=new HashSet<String>();
  public ArrayList<String> options=new ArrayList<String>();
  public HashSet<Path> files=new HashSet<Path>();
  public String verdict="Pass";
  public HashSet<String> suites=new HashSet<String>();
  public HashSet<String> pass_methods=new HashSet<String>();
  public HashSet<String> fail_methods=new HashSet<String>();
}