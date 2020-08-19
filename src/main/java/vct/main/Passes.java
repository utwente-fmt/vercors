package vct.main;

import hre.config.BooleanSetting;
import hre.config.StringSetting;
import hre.lang.HREError;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.ASTClass;
import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.syntax.JavaDialect;
import vct.col.ast.syntax.JavaSyntax;
import vct.col.ast.syntax.Syntax;
import vct.col.ast.util.AbstractRewriter;
import vct.col.rewrite.*;
import vct.col.util.JavaTypeCheck;
import vct.col.util.SimpleTypeCheck;
import vct.experiments.learn.NonLinCountVisitor;
import vct.experiments.learn.Oracle;
import vct.experiments.learn.SpecialCountVisitor;
import vct.logging.ErrorMapping;
import vct.logging.ExceptionMessage;
import vct.logging.PassReport;
import vct.parsers.JavaResolver;
import vct.parsers.rewrite.FlattenVariableDeclarations;
import vct.parsers.rewrite.InferADTTypes;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.PrintWriter;
import java.lang.reflect.Constructor;
import java.util.Hashtable;
import java.util.Map;

import static hre.lang.System.Abort;

public class Passes {
    public static Hashtable<String, Pass> defined_passes = new Hashtable<>();

    static {
        defined_passes.put("java", new CompilerPass("print AST in java syntax") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                PrintWriter out = hre.lang.System.getLogLevelOutputWriter(hre.lang.System.LogLevel.Info);
                JavaSyntax.getJava(JavaDialect.JavaVerCors).print(out, arg);
                out.close();
                return arg;
            }
        });
        defined_passes.put("c", new CompilerPass("print AST in C syntax") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                PrintWriter out = hre.lang.System.getLogLevelOutputWriter(hre.lang.System.LogLevel.Info);
                vct.col.ast.print.CPrinter.dump(out, arg);
                out.close();
                return arg;
            }
        });
        defined_passes.put("add-type-adt", new CompilerPass("Add an ADT that describes the types and use it to implement instanceof") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                return new AddTypeADT(arg).rewriteAll();
            }
        });
        compiler_pass(defined_passes, "access", "convert access expressions for histories/futures", AccessIntroduce.class);
        defined_passes.put("assign", new CompilerPass("change inline assignments to statements") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                return new AssignmentRewriter(arg).rewriteAll();
            }
        });
        defined_passes.put("silver", new ValidationPass("verify input with Silver") {
            @Override
            public PassReport apply_pass(PassReport arg, String... args) {
                return vct.silver.SilverBackend.TestSilicon(arg, args[0]);
            }
        });
        defined_passes.put("check", new CompilerPass("run a basic type check") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                new SimpleTypeCheck(arg).check();
                return arg;
            }
        });
        defined_passes.put("array_null_values", new CompilerPass("rewrite null values for arrays to None") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                return new ArrayNullValues(arg).rewriteAll();
            }
        });
        defined_passes.put("pointers_to_arrays", new CompilerPass("rewrite pointers to arrays") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                return new PointersToArrays(arg).rewriteAll();
            }
        });
        defined_passes.put("desugar_valid_pointer", new CompilerPass("rewrite \\array, \\matrix, \\pointer and \\pointer_index") {
            @Override
            protected ProgramUnit apply(ProgramUnit arg, String... args) {
                return new DesugarValidPointer(arg).rewriteAll();
            }
        });
        defined_passes.put("lift_declarations", new CompilerPass("lift declarations to cell of the declared types, to treat locals as heap locations.") {
            @Override
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                return new LiftDeclarations(arg).rewriteAll();
            }
        });
        defined_passes.put("java-check", new CompilerPass("run a Java aware type check") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                new JavaTypeCheck(arg).check();
                return arg;
            }
        });
        defined_passes.put("check-defined", new CompilerPass("rewrite process algebra class to check if defined process match their contracts") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                ProgramUnit tmp = new CheckProcessAlgebra(arg).rewriteAll();
                return new RandomizedIf(tmp).rewriteAll();
            }
        });
        defined_passes.put("check-axioms", new CompilerPass("rewrite process algebra class to check if history axioms are correct") {
            @Override
            public PassReport apply_pass(PassReport arg, String... args) {
                ProgramUnit input = arg.getOutput();
                PassReport res = new PassReport(input);
                ErrorMapping map = new ErrorMapping(arg);
                res.add(map);
                res.setOutput(new CheckHistoryAlgebra(input, CheckHistoryAlgebra.Mode.AxiomVerification, map).rewriteAll());
                return res;
            }
        });
        defined_passes.put("check-history", new CompilerPass("rewrite process algebra class to check if history accounting is correct") {
            @Override
            public PassReport apply_pass(PassReport arg, String... args) {
                ProgramUnit input = arg.getOutput();
                PassReport res = new PassReport(input);
                ErrorMapping map = new ErrorMapping(arg);
                res.add(map);
                res.setOutput(new CheckHistoryAlgebra(input, CheckHistoryAlgebra.Mode.ProgramVerification, map).rewriteAll());
                return res;
            }
        });
        defined_passes.put("csl-encode", new CompilerPass("Encode CSL atomic regions with methods") {
            @Override
            public PassReport apply_pass(PassReport arg, String... args) {
                ProgramUnit input = arg.getOutput();
                PassReport res = new PassReport(input);
                ErrorMapping map = new ErrorMapping(arg);
                res.add(map);
                res.setOutput(new CSLencoder(input, map).rewriteAll());
                return res;
            }
        });
        defined_passes.put("class-conversion", new CompilerPass("Convert classes into records and procedures") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                return new ClassConversion(arg).rewriteAll();
            }
        });
        defined_passes.put("codegen", new CompilerPass("Generate code") {
            public ProgramUnit apply(PassReport report, ProgramUnit arg, String... args) {
                File dir = new File(args[0]);
                if (dir.exists()) {
                    if (!dir.isDirectory()) {
                        report.fatal("%s is not a directory", dir);
                        return arg;
                    }
                } else {
                    if (!dir.mkdirs()) {
                        report.fatal("could not create %s", dir);
                        return arg;
                    }
                }
                Syntax syntax = JavaSyntax.getJava(JavaDialect.JavaVerCors);
                for (ASTNode node : arg) {
                    if (node instanceof ASTClass) {
                        PrintWriter out;
                        try {
                            out = new PrintWriter(new FileOutputStream(new File(dir, ((ASTClass) node).name() + ".java")));
                        } catch (FileNotFoundException e) {
                            report.add(new ExceptionMessage(e));
                            return arg;
                        }
                        out.println("import col.lang.*;");
                        syntax.print(out, node);
                        out.close();
                    } else if (node instanceof ASTSpecial) {
                        ASTSpecial S = (ASTSpecial) node;
                        switch (S.kind) {
                            case Comment:
                                // TODO keep comments.
                                break;
                            default:
                                report.fatal("cannot deal with special %s yet", S.kind);
                                return arg;
                        }
                    } else {
                        report.fatal("cannot deal with %s yet", node.getClass());
                        return arg;
                    }
                }
                return arg;
            }
        });
        defined_passes.put("current_thread", new CompilerPass("Encode references to current thread.") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                return new CurrentThreadRewriter(arg).rewriteAll();
            }
        });
        defined_passes.put("java-encode", new CompilerPass("Encode Java overloading and inheritance") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                arg = new JavaEncoder(arg).rewriteAll();

                return arg;
            }
        });
        defined_passes.put("explicit_encoding", new CompilerPass("encode required and ensured permission as ghost arguments") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                return new ExplicitPermissionEncoding(arg).rewriteAll();
            }
        });
        defined_passes.put("finalize_args", new CompilerPass("???") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                return new FinalizeArguments(arg).rewriteAll();
            }
        });
        defined_passes.put("flatten", new CompilerPass("remove nesting of expression") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                return new Flatten(arg).rewriteAll();
            }
        });
        defined_passes.put("ghost-lift", new CompilerPass("Lift ghost code to real code") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                return new GhostLifter(arg).rewriteAll();
            }
        });
        defined_passes.put("globalize", new CompilerPass("split classes into static and dynamic parts") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                return new GlobalizeStaticsParameter(arg).rewriteAll();
            }
        });
        defined_passes.put("ds_inherit", new CompilerPass("rewrite contracts to reflect inheritance, predicate chaining") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                return new DynamicStaticInheritance(arg).rewriteOrdered();
            }
        });
        defined_passes.put("flatten_before_after", new CompilerPass("move before/after instructions") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                return new FlattenBeforeAfter(arg).rewriteAll();
            }
        });
        defined_passes.put("flatten_variable_declarations", new CompilerPass("put the base type in declarations") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                return new FlattenVariableDeclarations(arg).rewriteAll();
            }
        });
        defined_passes.put("inline", new CompilerPass("Inline all methods marked as inline") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                return new InlinePredicatesRewriter(arg).rewriteAll();
            }
        });
        defined_passes.put("kernel-split", new CompilerPass("Split kernels into main, thread and barrier.") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                return new KernelRewriter(arg).rewriteAll();
            }
        });
        defined_passes.put("pvl-encode", new CompilerPass("Encode PVL builtins for verification.") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                return new PVLEncoder(arg).rewriteAll();
            }
        });
        branching_pass(
                defined_passes,
                "magicwand",
                "Encode magic wand proofs with abstract predicates",
                WandEncoder.class
        );
        defined_passes.put("modifies", new CompilerPass("Derive modifies clauses for all contracts") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                new DeriveModifies().annotate(arg);
                return arg;
            }
        });
        defined_passes.put("openmp2pvl", new CompilerPass("Compile OpenMP pragmas to PVL") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                return new OpenMPToPVL(arg).rewriteAll();
            }
        });
        defined_passes.put("parallel_blocks", new CompilerPass("Encoded the proof obligations for parallel blocks") {
            @Override
            public PassReport apply_pass(PassReport arg, String... args) {
                ProgramUnit input = arg.getOutput();
                PassReport res = new PassReport(input);
                ErrorMapping map = new ErrorMapping(arg);
                res.add(map);
                res.setOutput(new ParallelBlockEncoder(input, map).rewriteAll());
                return res;
            }

        });
        defined_passes.put("pvl-compile", new CompilerPass("Compile PVL classes to Java classes") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                return new PVLCompiler(arg).rewriteAll();
            }
        });
        defined_passes.put("reorder", new CompilerPass("reorder statements (e.g. all declarations at the start of a bock") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                return new ReorderAssignments(arg).rewriteAll();
            }
        });
        defined_passes.put("standardize-functions", new CompilerPass("translate pure methods to function syntax.") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                return new PureMethodsAsFunctions(arg).rewriteAll();
            }
        });
        defined_passes.put("java_resolve", new CompilerPass("Resolve the library dependencies of a java program") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                return new JavaResolver(arg).rewriteAll();
            }
        });
        defined_passes.put("propagate-invariants", new CompilerPass("propagate invariants") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                return new PropagateInvariants(arg).rewriteAll();
            }
        });
        defined_passes.put("quant-optimize", new CompilerPass("insert satisfyability checks for all methods") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                return new OptimizeQuantifiers(arg).rewriteAll();
            }
        });
        defined_passes.put("rewrite", new CompilerPass("Apply a term rewrite system") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                RewriteSystem trs = RewriteSystems.getRewriteSystem(args[0]);
                return trs.normalize(arg);
            }
        });
        defined_passes.put("rewrite_arrays", new CompilerPass("rewrite arrays to sequences of cells") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                return new RewriteArrayRef(arg).rewriteAll();
            }
        });
        defined_passes.put("generate_adt_functions", new CompilerPass("rewrite  standard operators on sequences to function definitions/calls") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                return new GenerateADTFunctions(arg).rewriteAll();
            }
        });
        defined_passes.put("infer_adt_types", new CompilerPass("Transform typeless collection constructors by inferring their types.") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                return new InferADTTypes(arg).rewriteAll();
            }
        });
        defined_passes.put("adt_operator_rewrite", new CompilerPass("rewrite PVL-specific ADT operators") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                return new ADTOperatorRewriter(arg).rewriteAll();
            }
        });
        defined_passes.put("rm_cons", new CompilerPass("???") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                return new ConstructorRewriter(arg).rewriteAll();
            }
        });
        defined_passes.put("sat_check", new CompilerPass("insert satisfyability checks for all methods") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                return new SatCheckRewriter(arg).rewriteAll();
            }
        });
        defined_passes.put("silver-class-reduction", new CompilerPass("reduce classes to single Ref class") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                return new SilverClassReduction(arg).rewriteAll();
            }
        });
        defined_passes.put("silver-reorder", new CompilerPass("move declarations from inside if-then-else blocks to top") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                return new SilverReorder(arg).rewriteAll();
            }
        });
        defined_passes.put("scale-always", new CompilerPass("scale every predicate invokation") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                return new ScaleAlways(arg).rewriteAll();
            }
        });
        defined_passes.put("silver-optimize", new CompilerPass("Optimize expressions for Silver") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                RewriteSystem trs = RewriteSystems.getRewriteSystem("silver_optimize");
                return trs.normalize(arg);
            }
        });
        defined_passes.put("chalice-optimize", new CompilerPass("Optimize expressions for Chalice") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                RewriteSystem trs = RewriteSystems.getRewriteSystem("chalice_optimize");
                return trs.normalize(arg);
            }
        });
        defined_passes.put("simplify_expr", new CompilerPass("Simplify expressions") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                RewriteSystem trs = RewriteSystems.getRewriteSystem("simplify_expr");
                return trs.normalize(arg);
            }
        });
        defined_passes.put("simplify_quant", new CompilerPass("Simplify quantifications") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                RewriteSystem trs = RewriteSystems.getRewriteSystem("simplify_quant_pass1");
                ProgramUnit res = trs.normalize(arg);
                res = RewriteSystems.getRewriteSystem("simplify_quant_pass2").normalize(res);
                return res;
            }
        });
        defined_passes.put("simplify_sums", new CompilerPass("replace summations with provable functions") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                RewriteSystem trs = RewriteSystems.getRewriteSystem("summation");
                return trs.normalize(arg);
            }
        });
        defined_passes.put("simplify_quant_relations", new CompilerPass("simplify quantified relational expressions") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                return new SimplifyQuantifiedRelations(arg).rewriteAll();
            }
        });
        defined_passes.put("standardize", new CompilerPass("Standardize representation") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                return new Standardize(arg).rewriteAll();
            }
        });
        defined_passes.put("strip_constructors", new CompilerPass("Strip constructors from classes") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                return new StripConstructors(arg).rewriteAll();
            }
        });
        branching_pass(defined_passes, "voidcalls", "Replace return value by out parameter.", VoidCalls.class);
        defined_passes.put("voidcallsthrown", new CompilerPass("Replace return value and thrown exceptions by out parameters.") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                return new VoidCallsThrown(arg).rewriteAll();
            }
        });
        compiler_pass(defined_passes, "vector-encode", "Encode vector blocks using the vector library", VectorEncode.class);
        defined_passes.put("chalice-preprocess", new CompilerPass("Pre processing for chalice") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                return new ChalicePreProcess(arg).rewriteAll();
            }
        });
        defined_passes.put("simple_triggers", new CompilerPass("Add triggers to quantifiers if possible") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                ProgramUnit res = arg;
                int val = Integer.valueOf(args[0]);
                // First gather quantified variables for quantifiers without triggers.
                res = new OptimizeQuantifiers(res).rewriteAll();
                // For quantifiers without triggers, and complex subscripts not containing quantified variables, add quantifier variable equal to the complex subscript.
                if ((val & 2) > 0) {
                    res = new RewriteComplexUnitSubscripts(res).rewriteAll();
                }
                // Try to add triggers for the now possibly simplified quantifiers.
                if ((val & 1) > 0) {
                    res = new AddSimpleTriggers(res).rewriteAll();
                }
                return res;
            }
        });
        defined_passes.put("count", new CompilerPass("Count nodes.") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                NonLinCountVisitor cv = new NonLinCountVisitor(arg);
                cv.count();
                if (args.length == 1) {
                    Main.counters.put(args[0], cv);
                } else {
                    Abort("Learn is used without an oracle");
                }
                return arg;
            }
        });
        defined_passes.put("learn", new CompilerPass("Learn unit times from counted AST nodes.") {
            public ProgramUnit apply(ProgramUnit arg, String... args) {
                if (args.length == 1) {
                    long start_time = Long.valueOf(args[0]);
                    long time = System.currentTimeMillis() - start_time;
                    for (Map.Entry<String, SpecialCountVisitor> entry : Main.counters.entrySet()) {
                        Oracle.tell(entry.getKey(), entry.getValue(), time);
                    }
                } else {
                    Abort("Learn is used without a starting time.");
                }
                return arg;
            }
        });
    }

    private static void branching_pass(Hashtable<String, Pass> defined_passes,
                                       String key, String description, final Class<? extends AbstractRewriter> class1) {
        try {
            defined_passes.put(key, new CompilerPass(description) {
                Constructor<? extends AbstractRewriter> cons = class1.getConstructor(ProgramUnit.class, ErrorMapping.class);

                @Override
                public PassReport apply_pass(PassReport inrep, String... args) {
                    ProgramUnit arg = inrep.getOutput();
                    PassReport res = new PassReport(arg);
                    ErrorMapping map = new ErrorMapping(inrep);
                    res.add(map);
                    AbstractRewriter rw;
                    try {
                        rw = (AbstractRewriter) cons.newInstance(arg, map);
                    } catch (Exception e) {
                        throw new HREError("unexpected exception %s", e);
                    }
                    res.setOutput(rw.rewriteAll());
                    return res;
                }

            });
        } catch (NoSuchMethodException e) {
            Abort("bad rewriter pass %s", key);
        }
    }

    private static void compiler_pass(Hashtable<String, Pass> defined_passes,
                                      String key, String description, final Class<? extends AbstractRewriter> class1) {
        try {
            defined_passes.put(key, new CompilerPass(description) {

                Constructor<? extends AbstractRewriter> cons = class1.getConstructor(ProgramUnit.class);

                @Override
                public ProgramUnit apply(ProgramUnit arg, String... args) {
                    AbstractRewriter rw;
                    try {
                        rw = (AbstractRewriter) cons.newInstance(arg);
                    } catch (Exception e) {
                        throw new HREError("unexpected exception %s", e);
                    }
                    return rw.rewriteAll();
                }

            });
        } catch (NoSuchMethodException e) {
            Abort("bad rewriter pass %s", key);
        }
    }
}
