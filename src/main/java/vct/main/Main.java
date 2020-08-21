// -*- tab-width:2 ; indent-tabs-mode:nil -*-

package vct.main;

import java.io.*;
import java.time.Instant;
import java.util.*;
import java.util.Map.Entry;
import java.util.concurrent.LinkedBlockingDeque;

import hre.ast.FileOrigin;
import hre.config.*;
import hre.lang.HREExitException;
import hre.tools.TimeKeeper;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.ASTDeclaration;
import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.stmt.decl.SpecificationFormat;
import vct.col.ast.expr.StandardOperator;
import vct.col.features.RainbowVisitor;
import vct.col.util.FeatureScanner;
import vct.experiments.learn.SpecialCountVisitor;
import vct.logging.PassReport;
import vct.silver.ErrorDisplayVisitor;
import vct.test.CommandLineTesting;
import hre.config.Configuration;

import static hre.lang.System.*;

/**
 * VerCors Tool main verifier.
 *
 * @author Stefan Blom
 */
public class Main {
    public static Map<String, SpecialCountVisitor> counters = new HashMap<String, SpecialCountVisitor>();

    private PassReport report;
    private TimeKeeper tk;

    private final BooleanSetting version = new BooleanSetting(false);
    private final BooleanSetting help_passes = new BooleanSetting(false);

    private final ChoiceSetting logLevel = new ChoiceSetting(new String[]{"silent", "abort", "result", "warning", "info", "progress", "debug", "all"}, "info");
    private final CollectSetting debugFilters = new CollectSetting();
    private final StringListSetting show_before = new StringListSetting();
    private final StringListSetting show_after = new StringListSetting();
    private final CollectSetting debugBefore = new CollectSetting();
    private final CollectSetting debugAfter = new CollectSetting();
    private final StringSetting show_file = new StringSetting(null);

    private final StringListSetting pass_list = new StringListSetting();
    private final Option pass_list_option = pass_list.getAppendOption("add to the custom list of compilation passes");
    private final BooleanSetting boogie = new BooleanSetting(false);
    private final BooleanSetting chalice = new BooleanSetting(false);
    private final StringSetting silver = new StringSetting("silver");
    private final BooleanSetting dafny = new BooleanSetting(false);

    private final StringListSetting stop_after = new StringListSetting();

    private final BooleanSetting check_defined = new BooleanSetting(false);
    private final BooleanSetting check_axioms = new BooleanSetting(false);
    private final BooleanSetting check_history = new BooleanSetting(false);
    private final BooleanSetting separate_checks = new BooleanSetting(false);
    private final BooleanSetting sequential_spec = new BooleanSetting(false);
    private final BooleanSetting explicit_encoding = new BooleanSetting(false);
    private final BooleanSetting global_with_field = new BooleanSetting(false);
    private final BooleanSetting infer_modifies = new BooleanSetting(false);
    private final BooleanSetting no_context = new BooleanSetting(false);
    private final BooleanSetting gui_context = new BooleanSetting(false);
    private final BooleanSetting sat_check = new BooleanSetting(true);
    private final IntegerSetting trigger_generation = new IntegerSetting(0);
    private final BooleanSetting learn = new BooleanSetting(false);

    private String[] parseOptions(String[] args) {
        OptionParser clops = new OptionParser();
        clops.add(clops.getHelpOption(), 'h', "help");
        clops.add(version.getEnable("Output the current version and exit"), "version");

        clops.add(logLevel.getSetOption("Set the logging level"), "verbosity");
        clops.add(logLevel.getExplicitOption("progress", "Show progress through the passes"), "progress", 'v');
        clops.add(logLevel.getExplicitOption("silent", "Never output anything"), "silent", 'q');

        clops.add(debugFilters.getAddOption("Add a class to debug, or specify a line with Class:lineno"), "debug");

        clops.add(boogie.getEnable("select Boogie backend"), "boogie");
        clops.add(chalice.getEnable("select Chalice backend"), "chalice");
        clops.add(silver.getAssign("select Silver backend (silicon/carbon)"), "silver");
        clops.add(silver.getAssign("select Silicon backend", "silicon"), "silicon");
        clops.add(silver.getAssign("select Carbon backend", "carbon"), "carbon");
        clops.add(dafny.getEnable("select Dafny backend"), "dafny");

        clops.add(check_defined.getEnable("check if defined processes satisfy their contracts."), "check-defined");
        clops.add(check_axioms.getEnable("check if defined processes satisfy their contracts."), "check-axioms");
        clops.add(check_history.getEnable("check if defined processes satisfy their contracts."), "check-history");

        clops.add(separate_checks.getEnable("validate classes separately"), "separate");
        clops.add(help_passes.getEnable("print help on available passes"), "help-passes");
        clops.add(sequential_spec.getEnable("sequential specification instead of concurrent"), "sequential");
        clops.add(pass_list_option, "passes");
        clops.add(show_before.getAppendOption("Show source code before given passes"), "show-before");
        clops.add(show_after.getAppendOption("Show source code after given passes"), "show-after");
        clops.add(show_file.getAssign("redirect show output to files instead of stdout"), "save-show");
        clops.add(debugBefore.getAddOption("Dump the COL AST before a pass is run"), "debug-before");
        clops.add(debugAfter.getAddOption("Dump the COL AST after a pass is run"), "debug-after");
        clops.add(stop_after.getAppendOption("Stop after given passes"), "stop-after");

        clops.add(explicit_encoding.getEnable("explicit encoding"), "explicit");
        clops.add_removed("the inline option was removed in favor of the inline modifer", "inline");

        clops.add(global_with_field.getEnable("Encode global access with a field rather than a parameter. (expert option)"), "global-with-field");

        clops.add(infer_modifies.getEnable("infer modifies clauses"), "infer-modifies");
        clops.add(no_context.getEnable("disable printing the context of errors"), "no-context");
        clops.add(gui_context.getEnable("enable the gui extension of the context"), "gui");

        clops.add(sat_check.getDisable("Disable checking if method pre-conditions are satisfiable"), "disable-sat");
        clops.add(trigger_generation.getOptionalAssign("Try to simplify universal quantifiers and generate triggers for them."), "triggers");
        clops.add(learn.getEnable("Learn unit times for AST nodes."), "learn");

        CommandLineTesting.addOptions(clops);
        Configuration.add_options(clops);

        return clops.parse(args);
    }

    private void setupLogging() {
        hre.lang.System.LogLevel level = hre.lang.System.LogLevel.Info;

        switch (logLevel.get()) {
            case "silent":
                level = hre.lang.System.LogLevel.Silent;
                break;
            case "abort":
                level = hre.lang.System.LogLevel.Abort;
                break;
            case "result":
                level = hre.lang.System.LogLevel.Result;
                break;
            case "warning":
                level = hre.lang.System.LogLevel.Warning;
                break;
            case "info":
                level = hre.lang.System.LogLevel.Info;
                break;
            case "progress":
                level = hre.lang.System.LogLevel.Progress;
                break;
            case "debug":
                level = hre.lang.System.LogLevel.Debug;
                break;
            case "all":
                level = hre.lang.System.LogLevel.All;
                break;
        }

        if (!debugFilters.get().isEmpty() && level.getOrder() < hre.lang.System.LogLevel.Debug.getOrder()) {
            level = hre.lang.System.LogLevel.Debug;
        }

        for (String filter : debugFilters.get().keySet()) {
            if (filter.contains(":") /* With line number */) {
                hre.lang.System.addDebugFilterByLine(filter);
            } else {
                hre.lang.System.addDebugFilterByClassName(filter);
            }
        }

        hre.lang.System.setOutputStream(System.out, level);
        hre.lang.System.setErrorStream(System.err, level);

        System.setErr(new hre.io.ForbiddenPrintStream(System.err));
        System.setOut(new hre.io.ForbiddenPrintStream(System.out));
    }

    private void checkOptions() {
        if (version.get()) {
            Output("%s %s", BuildInfo.name(), BuildInfo.version());
            Output("Built by sbt %s, scala %s at %s", BuildInfo.sbtVersion(), BuildInfo.scalaVersion(), Instant.ofEpochMilli(BuildInfo.builtAtMillis()));
            if (!BuildInfo.currentBranch().equals("master")) {
                Output(
                        "On branch %s, commit %s, %s",
                        BuildInfo.currentBranch(),
                        BuildInfo.currentShortCommit(),
                        BuildInfo.gitHasChanges()
                );
            }
            throw new HREExitException(0);
        }

        if (help_passes.get()) {
            Output("The following passes are available:");
            for (Entry<String, AbstractPass> entry : Passes.BY_KEY_JAVA().entrySet()) {
                Output(" %-12s : %s", entry.getKey(), entry.getValue().description());
            }
            throw new HREExitException(0);
        }

        if (!(CommandLineTesting.enabled() ||
                boogie.get() ||
                chalice.get() ||
                silver.used() ||
                dafny.get() ||
                pass_list.iterator().hasNext())) {
            Fail("no back-end or passes specified");
        }

        if (silver.used()) {
            switch (silver.get()) {
                case "silicon_qp":
                    Warning("silicon_qp has been merged into silicon, using silicon instead");
                    silver.set("silicon");
                    break;
                case "silicon":
                case "carbon":
                    break;
                default:
                    Fail("unknown silver backend: %s", silver.get());
            }
        }
    }

    private void parseInputs(String[] inputPaths) {
        Progress("parsing inputs...");
        report = new PassReport(new ProgramUnit());
        report.setOutput(report.getInput());
        report.add(new ErrorDisplayVisitor());
        tk.show();
        for (String name : inputPaths) {
            File f = new File(name);
            if (!no_context.get()) {
                FileOrigin.add(name, gui_context.get());
            }
            report.getOutput().add(Parsers.parseFile(f.getPath()));
        }
        Progress("Parsed %d file(s) in: %dms", inputPaths.length, tk.show());

        if (boogie.get() || sequential_spec.get()) {
            report.getOutput().setSpecificationFormat(SpecificationFormat.Sequential);
        }
    }

    private void getPassesForBoogie(Deque<String> passes) {
        passes.add("java_resolve"); // inspect class path for retreiving signatures of called methods. Will add files necessary to understand the Java code.
        passes.add("standardize"); // a rewriter s.t. only a subset of col will have to be supported
        passes.add("check"); // type check col. Add annotations (the types) to the ast.
        passes.add("rewrite_arrays"); // array generation and various array-related rewrites
        passes.add("check");
        passes.add("flatten"); // expressions that contain method calls (possibly having side-effects) are put into separate statements.
        passes.add("assign");  // '(x = y ==> assign(x,y);). Has not been merged with standardize because flatten needs to be done first.
        passes.add("finalize_args"); // declare new variables to never have to change the arguments (which isn't allowed in silver)
        passes.add("reorder"); // silver requires that local variables are declared at the top of methods (and loop-bodies?) so they're all moved to the top
        if (infer_modifies.get()) {
            passes.add("standardize");
            passes.add("check");
            passes.add("modifies"); // modifies is mandatory. This is how to automatically add it
        }
        passes.add("standardize");
        passes.add("check");
        passes.add("voidcalls"); // all methods in Boogie are void, so use an out parameter instead of 'return..'
        passes.add("standardize");
        passes.add("check");
        passes.add("flatten");
        passes.add("reorder");
        passes.add("standardize");
        passes.add("check");
        passes.add("strip_constructors"); // somewhere in the parser of Java, constructors are added implicitly. They need to be taken out again.
        passes.add("standardize");
        passes.add("check");
        passes.add("boogie"); // run backend
    }

    private void getPassesForDafny(Deque<String> passes) {
        passes.add("java_resolve");
        passes.add("standardize");
        passes.add("check");
        passes.add("voidcalls");
        passes.add("standardize");
        passes.add("check");
        passes.add("dafny"); // run backend
    }

    private void getPassesForSilver(Deque<String> passes, FeatureScanner features) {
        passes.add("java_resolve");

        if (silver.used() &&
                (features.usesSpecial(ASTSpecial.Kind.Lock)
                        || features.usesSpecial(ASTSpecial.Kind.Unlock)
                        || features.usesSpecial(ASTSpecial.Kind.Fork)
                        || features.usesSpecial(ASTSpecial.Kind.Join)
                        || features.usesOperator(StandardOperator.PVLidleToken)
                        || features.usesOperator(StandardOperator.PVLjoinToken)
                )) {
            passes.add("pvl-encode"); // translate built-in statements into methods and fake method calls.
        }

        passes.add("standardize");
        passes.add("java-check"); // marking function: stub
        passes.add("rainbow");

//        if (features.usesOperator(StandardOperator.AddrOf)) {
//            passes.add("lift_declarations");
//        }
//
//        passes.add("check");
//        passes.add("infer_adt_types");
//
//        passes.add("check");
//        passes.add("adt_operator_rewrite");
//
//        passes.add("check");
//        passes.add("standardize");
//
//        passes.add("java-check");
//        passes.add("pointers_to_arrays");
//        passes.add("java-check");
//        passes.add("desugar_valid_pointer");
//        passes.add("java-check");
//        passes.add("array_null_values"); // rewrite null values for array types into None
//        passes.add("java-check");
//        if (silver.used()) {
//            // The new encoding does not apply to Chalice yet.
//            // Maybe it never will.
//            passes.add("java-encode"); // disambiguate overloaded stuff, copy inherited functions and specifications
//            passes.add("standardize");
//            passes.add("check");
//        }
//
//        if (sat_check.get()) {
//            passes.add("sat_check"); // sanity check to avoid uncallable methods (where False is required)
//            passes.add("standardize");
//            passes.add("check");
//        }
//
//        if (features.usesIterationContracts() || features.usesPragma("omp")) {
//            passes.add("openmp2pvl"); // Converts *all* parallel loops! (And their compositions) Into ordered set of parallel blocks in pvl.
//            passes.add("standardize");
//            passes.add("check");
//        }
//
//        passes.add("propagate-invariants"); // desugar \invariant (see \invariant documentation)
//        passes.add("standardize");
//        passes.add("check");
//        if (features.usesOperator(StandardOperator.Wand)) {
//            passes.add("magicwand"); // translate magicwand uses (proof oblications) into method calls
//            passes.add("standardize");
//            passes.add("check");
//        }
//
//        passes.add("inline"); // inline predicates that are marked as inline (so 'fold/unfold' needn't be done)
//        passes.add("standardize");
//        passes.add("check");
//
//        if (features.usesCSL()) {
//            passes.add("csl-encode"); //
//            passes.add("standardize");
//            passes.add("check");
//        }
//
//        if (features.hasVectorBlocks() || features.usesPragma("omp")) {
//            passes.add("vector-encode"); // vector operations for supporting Saeed's paper
//            passes.add("standardize");
//            passes.add("check");
//        }
//
//        if (silver.used()) {
//            if (features.usesIterationContracts() || features.usesParallelBlocks() || features.usesCSL() || features.usesPragma("omp")) {
//                passes.add("parallel_blocks"); // pvl parallel blocks are put in separate methods that can be verified seperately. Method call replaces the contract of this parallel block.
//                passes.add("standardize");
//            }
//            passes.add("check");
//            passes.add("simplify_quant"); // reduce nesting of quantifiers
//            passes.add("simplify_quant_relations");
//            if (features.usesSummation() || features.usesIterationContracts()) {
//                passes.add("check");
//                passes.add("simplify_sums"); // set of rewrite rules for removing summations
//            }
//            passes.add("standardize");
//            passes.add("check");
//        }
//
//        if (features.usesKernels()) {// 8 feb 2018: is this now dead code (to be)? (SB)
//            passes.add("kernel-split");
//            passes.add("check");
//            passes.add("simplify_quant");
//            passes.add("standardize");
//            passes.add("check");
//        }
//
//        boolean has_type_adt = false;
//        if (silver.used()) {
//            if (features.usesOperator(StandardOperator.Instance)
//                    || features.usesInheritance()
//                    || features.usesOperator(StandardOperator.TypeOf)
//            ) {
//                passes.add("add-type-adt"); // add java's types of the programs as silicon's axiomatic datatypes
//                passes.add("standardize");
//                passes.add("check");
//                has_type_adt = true;
//            }
//        }
//
//        if (!silver.used() && features.usesInheritance()) { // 8 feb 2018: SB nominates this block for removal
//            // reason: chalice's types and inheritance mechanism isn't the same as Java's, so why not translate everything the same way and ignore chalice's mechanism
//            passes.add("standardize");
//            passes.add("check");
//            passes.add("ds_inherit"); // Use the old inheritance encoding for Chalice.
//            passes.add("standardize");
//            passes.add("check");
//        }
//
//        // histories and futures
//        // three verification runs:
//        // 3) verify program: promises wrt process algebra need to be met
//        // 2) verify the process algebra axioms: check whether paralel compositions are 'correct'
//        // 1) auxiliarry definitions in the process algebra should match their contracts.
//        if (check_defined.get()) {
//            passes.add("check-defined"); // add checks
//            passes.add("standardize");
//            passes.add("check");
//        } else if (check_axioms.get()) {
//            passes.add("check-axioms");
//            passes.add("standardize");
//            passes.add("check");
//        } else if (features.usesProcesses() || check_history.get()) {
//            passes.add("access"); // pre-process for check-history?
//            passes.add("standardize");
//            passes.add("check");
//            passes.add("check-history");
//            passes.add("standardize");
//            passes.add("check");
//        }
//
//        if (explicit_encoding.get()) {
//            passes.add("explicit_encoding"); // magic wand paper: for passing around predicates witnesses. In chalice predicates do not have arguments, except 'this'. So we're making data-types to pass around witnesses. Not necessary for silicon.
//            passes.add("standardize");
//            passes.add("check");
//        }
//
//        if (silver.used()) {
//            passes.add("current_thread"); // add argument 'current thread' to all methods
//            passes.add("standardize");
//            passes.add("check");
//        }
//
//        passes.add("rewrite_arrays"); // array generation and various array-related rewrites
//        passes.add("check");
//        passes.add("generate_adt_functions");
//        passes.add("check");
//        passes.add("flatten");
//        passes.add("assign");
//        passes.add("reorder");
//        passes.add("standardize");
//
//        passes.add("check");
//        passes.add("simplify_quant");
//        passes.add("standardize");
//        passes.add("check");
//
//        if (silver.used()) {
//            if (trigger_generation.get() > 0) {
//                passes.add("simple_triggers=" + trigger_generation.get());
//                passes.add("check");
//            }
//            passes.add("silver-class-reduction"); // remove the class (since all names are now unique), only one class remains
//            passes.add("standardize");
//            passes.add("check");
//        } else {
//            passes.add("globalize"); // create a separate class to contain all statics (class probably called 'Global', needs to be given as argument to the other methods)
//            passes.add("standardize");
//            passes.add("check");
//            passes.add("rm_cons"); // replace constructors by init-methods
//            passes.add("standardize");
//            passes.add("check");
//        }
//
//
//        if (has_type_adt) {
//            passes.add("voidcallsthrown"); // like voidcalls, but also exceptions are put into an out-argument
//        } else {
//            passes.add("voidcalls");
//        }
//        passes.add("standardize");
//        passes.add("check");
//
//        if (silver.used()) {
//            passes.add("ghost-lift"); // change ghost code into real code so it can is used in the check
//            passes.add("standardize");
//            passes.add("check");
//        }
//
//        passes.add("flatten");
//        passes.add("reorder"); // leaves declarations at the top of blocks (within loops and branches)
//        passes.add("flatten_before_after"); // method calls can have 'before/after' blocks of ghost-code attached. Put it around all method calls.
//        if (silver.used()) {
//            passes.add("silver-reorder"); // no declarations in branches (only in loops)
//        }
//        passes.add("standardize");
//        passes.add("check");
//
//        if (!silver.used()) {
//            passes.add("finalize_args");
//            passes.add("reorder");
//            passes.add("standardize");
//            passes.add("check");
//        }
//
//        if (silver.used()) {
//            passes.add("scale-always"); // syntax: in silicon [p]predicate() is mandatory, so change pred() to [1]pred()
//            passes.add("check"); // the rewrite system needs a type check
//            passes.add("silver-optimize"); // rewrite to things that silver likes better
//            passes.add("check"); // the rewrite system needs a type check
//            passes.add("quant-optimize"); // some illegal-quantifier constructions need to be written differently (plus optimize)
//            passes.add("standardize-functions"); // pure methods do not need to be 'methods', try turning them into functions so silver and chalice can reason more intelligently about them. Pure methods can be used in specifications through this.
//            passes.add("standardize");
//            passes.add("check");
//
//            passes.add("silver=" + silver.get());
//        } else { //CHALICE
//            passes.add("check"); // rewrite system needs a type check
//            passes.add("chalice-optimize");
//            passes.add("standardize-functions");
//            passes.add("standardize");
//            passes.add("check");
//
//            passes.add("chalice-preprocess");
//            passes.add("standardize");
//            passes.add("check");
//            passes.add("chalice");
//        }
//
//        if (learn.get()) {
//            passes.addFirst("count=" + silver.get() + "_before_rewrite");
//            passes.add("learn=" + wallStart);
//        }
    }

    private Deque<String> getPasses() {
        FeatureScanner features = new FeatureScanner();
        report.getOutput().accept(features);

        Deque<String> passes = new LinkedBlockingDeque<>();
        if (pass_list_option.used()) {
            for (String s : pass_list) {
                passes.add(s);
            }
        } else if (boogie.get()) {
            getPassesForBoogie(passes);
        } else if (dafny.get()) {
            getPassesForDafny(passes);
        } else if (silver.used() || chalice.get()) {
            getPassesForSilver(passes, features);
        } else {
            Abort("no back-end or passes specified");
        }

        return passes;
    }

    private void doPasses(Deque<String> passes) throws FileNotFoundException {
        int fatal_errs = 0;
        int passCount = passes.size();

        while (!passes.isEmpty() && fatal_errs == 0) {
            String pass = passes.removeFirst();
            String[] pass_args = pass.split("=");
            pass = pass_args[0];
            if (pass_args.length == 1) {
                pass_args = new String[0];
            } else {
                pass_args = pass_args[1].split("\\+");
            }

            if (debugBefore.has(pass)) {
                report.getOutput().dump();
            }
            if (show_before.contains(pass)) {
                String name = show_file.get();
                if (name != null) {
                    String file = String.format(name, pass);
                    PrintWriter out = new PrintWriter(new FileOutputStream(file));
                    vct.col.ast.util.Configuration.getDiagSyntax().print(out, report.getOutput());
                    out.close();
                } else {
                    PrintWriter out = hre.lang.System.getLogLevelOutputWriter(hre.lang.System.LogLevel.Info);
                    vct.col.ast.util.Configuration.getDiagSyntax().print(out, report.getOutput());
                    out.close();
                }
            }

            AbstractPass task = Passes.BY_KEY_JAVA().get(pass);

            if(task == null) {
                Fail("unknown pass %s", pass);
            }

            tk.show();
            report = task.apply_pass(report, pass_args);
            fatal_errs = report.getFatal();
            Progress("[%02d%%] %s took %d ms", 100 * (passCount - passes.size()) / passCount, pass, tk.show());

            if(pass.equals("rainbow")) {
                passes.addAll(Passes.rainbowResult());
            }

            if (debugAfter.has(pass)) {
                report.getOutput().dump();
            }
            if (show_after.contains(pass)) {
                String name = show_file.get();
                if (name != null) {
                    String file = String.format(name, pass);
                    PrintWriter out = new PrintWriter(new FileOutputStream(file));
                    vct.col.ast.util.Configuration.getDiagSyntax().print(out, report.getOutput());
                    out.close();
                } else {
                    PrintWriter out = hre.lang.System.getLogLevelOutputWriter(hre.lang.System.LogLevel.Info);
                    vct.col.ast.util.Configuration.getDiagSyntax().print(out, report.getOutput());
                    out.close();
                }
            }
            if (stop_after.contains(pass)) {
                Fail("exit after pass %s", pass);
            }
        }
        Verdict("The final verdict is %s", fatal_errs == 0 ? "Pass" : "Fail");
    }

    private void run(String[] args) throws FileNotFoundException {
        int exit = 0;
        long wallStart = System.currentTimeMillis();
        tk = new TimeKeeper();

        try {
            hre.lang.System.setOutputStream(System.out, hre.lang.System.LogLevel.Info);
            hre.lang.System.setErrorStream(System.err, hre.lang.System.LogLevel.Info);

            String[] inputPaths = parseOptions(args);
            setupLogging();
            checkOptions();

            if (CommandLineTesting.enabled()) {
                CommandLineTesting.runTests();
            } else {
                parseInputs(inputPaths);
                doPasses(getPasses());
            }
        } catch (HREExitException e) {
            exit = e.exit;
            Verdict("The final verdict is Error");
        } catch (Throwable e) {
            DebugException(e);
            Verdict("An unexpected error occured in VerCors! " +
                    "Please report an issue at https://github.com/utwente-fmt/vercors/issues/new. " +
                    "You can see the full exception by adding '--debug vct.main.Main' to the flags.");
            Verdict("The final verdict is Error");
            throw e;
        } finally {
            Progress("entire run took %d ms", System.currentTimeMillis() - wallStart);
            System.exit(exit);
        }
    }

    public static void main(String[] args) throws Throwable {
        new Main().run(args);
    }
}
