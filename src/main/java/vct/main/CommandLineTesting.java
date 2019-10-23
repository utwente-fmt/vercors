package vct.main;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.file.FileVisitOption;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map.Entry;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.concurrent.Callable;

import hre.io.MessageProcess;
import hre.io.MessageProcessEnvironment;
import puptol.PuptolConfig;
import rise4fun.Rise4funConfig;
import hre.config.BooleanSetting;
import hre.config.IntegerSetting;
import hre.config.Option;
import hre.config.OptionParser;
import hre.config.StringListSetting;
import hre.config.StringSetting;
import hre.util.TestReport.Verdict;
import vct.util.Configuration;

import static hre.lang.System.*;

public class CommandLineTesting {
    private static StringListSetting includes = new StringListSetting();
    private static Option include_option;
    private static StringListSetting excludes = new StringListSetting();
    private static Option exclude_option;

    private static StringListSetting langs = new StringListSetting();
    private static Option lang_option;
    private static StringListSetting backends = new StringListSetting();
    private static Option backend_option;
    private static StringListSetting selftest = new StringListSetting();
    private static Option append_option;
    protected static StringSetting savedir = new StringSetting(null);
    public static IntegerSetting workers = new IntegerSetting(1);
    public static StringSetting puptol_file = new StringSetting(null);
    private static Option puptolupdate =
        puptol_file.getAssign("update experiments in puptol file");
    public static BooleanSetting rise4fun = new BooleanSetting(false);
    private static Option rise4fun_option = rise4fun.getEnable("yield rise4fun experiments as JSON");

    private Rise4funConfig rise4funConfig = null;
    private PuptolConfig puptolConfig = null;
    private final TestcaseVisitor testcaseVisitor = new TestcaseVisitor();
    private final HashMap<String, Integer> times = new HashMap<String, Integer>();
    private final HashMap<String, Testcase> untested = new HashMap<String, Testcase>();
    private final HashMap<String, String> failures = new HashMap<String, String>();

    private MessageProcessEnvironment z3;
    private MessageProcessEnvironment boogie;
    private MessageProcessEnvironment chalice;
    private MessageProcessEnvironment dafny;
    private MessageProcessEnvironment carbon;
    private MessageProcessEnvironment silicon;
    private MessageProcessEnvironment vercors;

    public CommandLineTesting() throws IOException {
        z3 = Configuration.getZ3();
        boogie = Configuration.getBoogie();
        chalice = Configuration.getChalice();
        dafny = Configuration.getDafny();
        carbon = Configuration.getCarbon();
        silicon = Configuration.getSilicon();
        vercors = Configuration.getThisVerCors();
    }

    public static void add_options(OptionParser clops) {
        append_option = selftest.getAppendOption("execute test suites from the command line. " +
            "Each test suite is a folder which is scanned for valid test inputs");
        clops.add(append_option, "test");
        clops.add(backend_option = backends.getAppendOption("select the back ends to run tests for"), "tool");
        clops.add(lang_option = langs.getAppendOption("select test input languages"), "lang");
        clops.add(savedir.getAssign("save intermediate files to given directory"), "save-intermediate");
        clops.add(include_option = includes.getAppendOption("include test suites"), "include-suite");
        clops.add(exclude_option = excludes.getAppendOption("exclude test suites"), "exclude-suite");
        clops.add(puptolupdate, "puptol-config");
        clops.add(rise4fun_option, "rise4fun-json");
        clops.add(workers.getAssign("set the number of parallel tests"), "test-workers");
    }

    public static boolean enabled() {
        return append_option.used();
    }

    private void registerSelfTests(HashMap<String, Runnable> tasks) {
        for (String dir : selftest) {
            if (dir.equals("")) {
                tasks.put("self-sat", () -> {
                    VCTResult res = runtest(Verdict.Inconclusive, z3.withArgs("-smt2", Configuration.getSelfTestPath("test-sat.smt").getAbsolutePath()));
                    res.mustSay("p true");
                    res.mustSay("q true");
                    check(res, "z3", "sat");
                });

                tasks.put("self-unsat", () -> {
                    VCTResult res = runtest(Verdict.Inconclusive, z3.withArgs("-smt2", Configuration.getSelfTestPath("test-unsat.smt").getAbsolutePath()));
                    res.mustSay("unsat");
                    check(res, "z3", "unsat");
                });

                tasks.put("self-pass", () -> {
                    VCTResult res = runtest(Verdict.Inconclusive, boogie.withArgs(Configuration.getSelfTestPath("test-pass.bpl").getAbsolutePath()));
                    res.mustSay("Boogie program verifier finished with 1 verified, 0 errors");
                    check(res, "boogie", "passing");
                });

                tasks.put("self-fail", () -> {
                    VCTResult res = runtest(Verdict.Inconclusive, boogie.withArgs(Configuration.getSelfTestPath("test-fail.bpl").getAbsolutePath()));
                    res.mustSay("Boogie program verifier finished with 0 verified, 1 error");
                    check(res, "boogie", "failing");
                });

                tasks.put("self-pass-chalice", () -> {
                    VCTResult res = runtest(Verdict.Inconclusive, chalice.withArgs(Configuration.getSelfTestPath("test-pass.chalice").getAbsolutePath()));
                    res.mustSay("Boogie program verifier finished with 3 verified, 0 errors");
                    check(res, "chalice", "passing");
                });

                tasks.put("self-fail-chalice", () -> {
                    VCTResult res = runtest(Verdict.Inconclusive, chalice.withArgs(Configuration.getSelfTestPath("test-fail.chalice").getAbsolutePath()));
                    res.mustSay("Boogie program verifier finished with 2 verified, 1 error");
                    check(res, "chalice", "failing");
                });

                tasks.put("self-pass-dafny", () -> {
                    VCTResult res = runtest(Verdict.Inconclusive, dafny.withArgs("/compile:0", Configuration.getSelfTestPath("test-pass.dfy").getAbsolutePath()));
                    res.mustSay("Dafny program verifier finished with 2 verified, 0 errors");
                    check(res, "dafny", "passing");
                });

                tasks.put("self-fail-dafny", () -> {
                    VCTResult res = runtest(Verdict.Error, dafny.withArgs("/compile:0", Configuration.getSelfTestPath("test-fail.dfy").getAbsolutePath()));
                    res.mustSay("Dafny program verifier finished with 1 verified, 1 error");
                    check(res, "dafny", "failing");
                });

                tasks.put("self-pass-carbon", () -> {
                    VCTResult res = runtest(Verdict.Inconclusive, carbon.withArgs(Configuration.getSelfTestPath("test-pass.sil").getAbsolutePath()));
                    res.mustSay("No errors found.");
                    check(res, "carbon", "passing");
                });

                tasks.put("self-fail-carbon", () -> {
                    VCTResult res = runtest(Verdict.Error, carbon.withArgs(Configuration.getSelfTestPath("test-fail.sil").getAbsolutePath()));
                    res.mustSay("Assignment might fail. Divisor 0 might be zero.");
                    check(res, "carbon", "failing");
                });

                tasks.put("self-pass-silicon", () -> {
                    VCTResult res = runtest(Verdict.Inconclusive, silicon.withArgs(Configuration.getSelfTestPath("test-pass.sil").getAbsolutePath()));
                    res.mustSay("No errors found.");
                    check(res, "silicon", "passing");
                });

                tasks.put("self-fail-silicon", () -> {
                    VCTResult res = runtest(Verdict.Error, silicon.withArgs(Configuration.getSelfTestPath("test-fail.sil").getAbsolutePath()));
                    res.mustSay("Assignment might fail. Divisor 0 might be zero.");
                    check(res, "silicon", "failing");
                });
            } else {
                try {
                    EnumSet<FileVisitOption> opts = EnumSet.of(FileVisitOption.FOLLOW_LINKS);
                    Files.walkFileTree(Paths.get(dir), opts, 10, testcaseVisitor);
                } catch (IOException e) {
                    DebugException(e);
                }
            }
        }
    }

    private void registerTests(HashMap<String, Runnable> tasks) {
        // if enabled, construct new puptol configuration
        if (puptol_file.used()) {
            puptolConfig = new PuptolConfig();
        }

        // if enabled, construct new rise4fun configuration
        if (rise4fun.get()) {
            rise4funConfig = new Rise4funConfig();
        }

        for (Entry<String, Testcase> item : testcaseVisitor.testsuite.entrySet()) {
            String name = item.getKey();
            Testcase test = item.getValue();
            if (test.tools.isEmpty()) {
                untested.put(name, test);
            }
            if (lang_option.used()) {
                boolean possible = true;
                for (Path p : test.files) {
                    String lang = TestcaseVisitor.extension(p);
                    if (!langs.contains(lang)) {
                        possible = false;
                        break;
                    }
                }
                if (!possible) continue;
            }
            if (include_option.used()) {
                boolean possible = false;
                for (String suite : test.suites) {
                    if (includes.contains(suite)) {
                        possible = true;
                        break;
                    }
                }
                if (!possible) continue;
            }
            if (exclude_option.used()) {
                boolean possible = true;
                for (String suite : test.suites) {
                    if (excludes.contains(suite)) {
                        possible = false;
                        break;
                    }
                }
                if (!possible) continue;
            }
            for (String tool : test.tools) {
                if (backend_option.used() && !backends.contains(tool)) {
                    // skip tests for back ends that are not selected.
                    continue;
                }

                if (rise4fun.get()) {
                    // for now we only support single-file example programs
                    if (test.files.size() != 1) {
                        Output("cannot configure %s/%s in rise4fun: too many files", name, tool);
                        continue;
                    }

                    // retrieve example file name
                    Path file = null;
                    for (Path f : test.files) file = f;

                    // add example to the rise4fun suite
                    rise4funConfig.addExample(name, file.toString());

                    // skip the actual test execution
                    continue;
                }

                if (puptol_file.used()) {
                    if (test.files.size() != 1) {
                        Output("cannot configure %s/%s in puptol: too many files",
                            name, tool);
                        continue;
                    }
                    Path file = null;
                    for (Path f : test.files) file = f;
                    Progress("test %s/%s", name, tool);
                    Iterator<Path> iter = file.iterator();
                    try {
                        while (!iter.next().toString().equals("shared")) {
                        }
                    } catch (NoSuchElementException e) {
                        Warning("path element shared not found");
                        continue;
                    }
                    String experiment = iter.next().toString();
                    String filename = iter.next().toString();
                    ArrayList<String> path = new ArrayList<String>();
                    while (iter.hasNext()) {
                        path.add(filename);
                        filename = iter.next().toString();
                    }
                    Debug("  path: %s", path);
                    Debug("  file: %s", filename);
                    for (String opt : test.options) {
                        Debug("  option: %s", opt);
                    }
                    Debug("to be added to %s", experiment);
                    puptolConfig.add(experiment, path, name, tool, filename, test.options);
                    continue;
                }
                MessageProcessEnvironment env = vercors.copy();
                switch (tool) {
                    case "silicon":
                    case "carbon":
                        env.addArg("--silver=" + tool);
                        break;
                    default:
                        env.addArg("--" + tool);
                }
                for (String opt : test.options) env.addArg(opt);
                for (Path f : test.files) env.addArg(f.toAbsolutePath().toString());
                tasks.put("suite-" + item.getKey(), () -> {
                    try {
                        TestResult tr = new TestResult(env, test, name, tool).call();
                        times.put(tr.name + "/" + tr.tool, tr.res.times.get("entire run"));
                        if (tr.res.verdict == null) {
                            tr.res.verdict = Verdict.Error;
                        }
                        if (tr.res.verdict.toString().equals(tr.test.verdict)) {
                            boolean ok = true;
                            for (String method : tr.test.pass_methods) {
                                if (method.equals("any")) continue;
                                if (!contains_method(tr.res.pass_methods, method)) {
                                    failures.put(tr.name + "/" + tr.tool + "/" + method, String.format(
                                        "method did not pass"));
                                    ok = false;
                                }
                            }
                            for (String method : tr.test.fail_methods) {
                                if (!contains_method(tr.res.fail_methods, method)) {
                                    failures.put(tr.name + "/" + tr.tool + "/" + method, String.format(
                                        "method did not fail"));
                                    ok = false;
                                }
                            }
                            if (tr.test.pass_methods.contains("any")) {
                                for (String failed : tr.res.fail_methods) {
                                    if (!allowed_method(tr.test.fail_methods, failed)) {
                                        failures.put(tr.name + "/" + tr.tool + "/" + failed, String.format(
                                            "method failed unexpectedly"));
                                        ok = false;
                                    }
                                }
                            }
                            if (ok) {
                                Progress("%s/%s: Pass", tr.name, tr.tool);
                            } else {
                                Progress("%s/%s: Fail (method list)", tr.name, tr.tool);
                            }
                        } else {
                            Progress("%s/%s: Fail (%s/%s)", tr.name, tr.tool, tr.res.verdict, tr.test.verdict);
                            failures.put(tr.name + "/" + tr.tool, String.format(
                                "verdict is %s instead of %s", tr.res.verdict, tr.test.verdict));
                        }
                    } catch (Exception e) {
                        DebugException(e);
                        System.exit(1);
                    }
                });
            }
        }
    }

    private void runTests(HashMap<String, Runnable> tasks) {
        ArrayList<String> sortedTaskKeys = new ArrayList<>(tasks.keySet());
        Collections.sort(sortedTaskKeys);

        int splitDivisor = 1, splitModulus = 0;

        String split = System.getenv("SPLIT");
        if (split != null) {
            String[] parts = split.split("/");

            if (parts.length != 2) {
                Warning("%s", "SPLIT environment variable in incorrect format, will ignore and run all tests.");
            }

            try {
                // Temp, because we want to parse both integers first before setting the variables.
                int temp = Integer.parseInt(parts[0]);
                splitDivisor = Integer.parseInt(parts[1]);
                splitModulus = temp;
            } catch (NumberFormatException e) {
                Warning("%s", "SPLIT environment variable in incorrect format, will ignore and run all tests.");
            }
        }

        for (int i = splitModulus; i < sortedTaskKeys.size(); i += splitDivisor) {
            tasks.get(sortedTaskKeys.get(i)).run();
        }
    }

    private void printResults() {

        // if rise4fun configuration is enabled, write the config data as JSON to stderr
        if (rise4fun.get()) {
            Debug("%s", rise4funConfig.toJson());
        }

        if (puptol_file.used()) {
            puptolConfig.update(puptol_file.get());
        }
        boolean pass = true;
        for (String file : testcaseVisitor.files_by_name.keySet()) {
            Set<Path> items = testcaseVisitor.files_by_name.get(file);
            if (items.size() > 1) {
                Warning("Warning: there are multiple instance of %s:", file);
                for (Path p : items) {
                    Warning(" %s", p);
                }
            }
        }
        if (!untested.isEmpty()) {
            Warning("Warning: the following %d tests have been disabled:", untested.size());
            for (Entry<String, Testcase> item : untested.entrySet()) {
                String name = item.getKey();
                Testcase test = item.getValue();
                String line = "  " + name + " ";
                String before = "(";
                for (Path f : test.files) {
                    line += before + f.toString();
                    before = " ";
                }
                line += ")";
                Warning("%s", line);
            }
        }
        Output("verification times (ms):");
        ArrayList<String> list = new ArrayList<String>(times.keySet());
        Collections.sort(list);
        for (String t : list) {
            Output("%35s: %10d", t, times.get(t));
        }
        int successes = 0;
        if (failures.isEmpty()) {
            Verdict("all %d tests passed", successes);
        } else {
            pass = false;
            Output("the following tests failed");
            for (Entry<String, String> t : failures.entrySet()) {
                Output("  %s: %s", t.getKey(), t.getValue());
            }
            Verdict("total %s successes and %d failures", successes, failures.size());
        }
        if (testcaseVisitor.unmarked.size() > 0) {
            pass = false;
            Warning("there were %d unmarked files:", testcaseVisitor.unmarked.size());
            for (Path p : testcaseVisitor.unmarked) {
                Warning("  %s", p);
            }
        }
        if (pass) {
            System.exit(0);
        } else {
            System.exit(1);
        }
    }

    public void runAll() {
        HashMap<String, Runnable> tasks = new HashMap<>();

        registerSelfTests(tasks);
        registerTests(tasks);
        runTests(tasks);
        printResults();
    }

    private static boolean allowed_method(HashSet<String> fail_methods, String failed) {
        for (String m : fail_methods) {
            String tmp[] = m.split("\\.");
            String coded = "";
            for (int i = 0; i < tmp.length; i++) {
                coded += "_" + tmp[i];
            }
            if (failed.contains(coded)) return true;
        }
        return false;
    }

    private static boolean contains_method(HashSet<String> pass_methods, String method) {
        String tmp[] = method.split("\\.");
        String coded = "";
        for (int i = 0; i < tmp.length; i++) {
            coded += "_" + tmp[i];
        }
        for (String s : pass_methods) {
            if (s.contains(coded)) return true;
        }
        return false;
    }

    private static void check(VCTResult res, String tool, String test) {
        if (res.verdict != Verdict.Pass) {
            Fail("%s did not pass the %s test%s", tool, test);
        }
    }

    private static VCTResult runtest(Verdict expect, MessageProcessEnvironment env) {
        Progress("executing");
        VCTResult res = new ToolTest().run(env);
        if (res.verdict == expect) {
            res.verdict = Verdict.Pass;
        } else {
            Fail("%s did not execute properly", env.getProcess());
        }
        return res;
    }
}

class TestResult implements Callable<TestResult> {

    public TestResult(MessageProcessEnvironment env, Testcase test, String name, String tool) {
        this.env = env;
        this.test = test;
        this.name = name;
        this.tool = tool;
    }

    String name;
    String tool;
    Testcase test;
    VCTResult res;
    MessageProcessEnvironment env;

    @Override
    public TestResult call() throws Exception {
        res = new ToolTest().run(env);
        return this;
    }

}
