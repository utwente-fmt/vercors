package vct.main

import hre.config.BooleanSetting
import hre.config.StringSetting
import hre.lang.HREError
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.decl.ASTClass
import vct.col.ast.stmt.decl.ASTSpecial
import vct.col.ast.stmt.decl.ProgramUnit
import vct.col.ast.syntax.JavaDialect
import vct.col.ast.syntax.JavaSyntax
import vct.col.ast.syntax.Syntax
import vct.col.ast.util.AbstractRewriter
import vct.col.rewrite._
import vct.col.util.JavaTypeCheck
import vct.col.util.SimpleTypeCheck
import vct.experiments.learn.NonLinCountVisitor
import vct.experiments.learn.Oracle
import vct.experiments.learn.SpecialCountVisitor
import vct.logging.ErrorMapping
import vct.logging.ExceptionMessage
import vct.logging.PassReport
import vct.parsers.JavaResolver
import vct.parsers.rewrite.FlattenVariableDeclarations
import vct.parsers.rewrite.InferADTTypes
import java.io.File
import java.io.FileNotFoundException
import java.io.FileOutputStream
import java.io.PrintWriter
import java.lang.reflect.Constructor
import java.util

import hre.lang.System.Abort

import scala.collection.JavaConverters._

object Passes {
  var defined_passes = new util.Hashtable[String, Pass]

  defined_passes.put("java", SimplePass("print AST in java syntax", (arg, args) => {
    val out = hre.lang.System.getLogLevelOutputWriter(hre.lang.System.LogLevel.Info)
    JavaSyntax.getJava(JavaDialect.JavaVerCors).print(out, arg)
    out.close()
    arg
  }))
  defined_passes.put("c", SimplePass("print AST in C syntax", (arg, args) => {
    val out = hre.lang.System.getLogLevelOutputWriter(hre.lang.System.LogLevel.Info)
    vct.col.ast.print.CPrinter.dump(out, arg)
    out.close()
    arg
  }))
  defined_passes.put("add-type-adt", SimplePass("Add an ADT that describes the types and use it to implement instanceof", (arg, args) => new AddTypeADT(arg).rewriteAll))
  defined_passes.put("access", SimplePass("convert access expressions for histories/futures", (arg, args) => new AccessIntroduce(arg).rewriteAll))
  defined_passes.put("assign", SimplePass("change inline assignments to statements", (arg, args) => new AssignmentRewriter(arg).rewriteAll))
  defined_passes.put("silver", new Pass("verify input with Silver") {
    override def apply_pass(arg: PassReport, args: Array[String]) = vct.silver.SilverBackend.TestSilicon(arg, args(0))
  })
  defined_passes.put("check", SimplePass("run a basic type check", (arg, args) => {
    new SimpleTypeCheck(arg).check()
    arg
  }))
  defined_passes.put("array_null_values", SimplePass("rewrite null values for arrays to None", (arg, args) => new ArrayNullValues(arg).rewriteAll))
  defined_passes.put("pointers_to_arrays", SimplePass("rewrite pointers to arrays", (arg, args) => new PointersToArrays(arg).rewriteAll))
  defined_passes.put("desugar_valid_pointer", new Pass("rewrite \\array, \\matrix, \\pointer and \\pointer_index") {
    override protected def apply(arg: ProgramUnit, args: Array[String]) = new DesugarValidPointer(arg).rewriteAll
  })
  defined_passes.put("lift_declarations", SimplePass("lift declarations to cell of the declared types, to treat locals as heap locations.", (arg, args) => new LiftDeclarations(arg).rewriteAll))
  defined_passes.put("java-check", SimplePass("run a Java aware type check", (arg, args) => {
    new JavaTypeCheck(arg).check()
    arg
  }))
  defined_passes.put("check-defined", SimplePass("rewrite process algebra class to check if defined process match their contracts", (arg, args) => {
    val tmp = new CheckProcessAlgebra(arg).rewriteAll
    new RandomizedIf(tmp).rewriteAll
  }))
  defined_passes.put("check-axioms", new Pass("rewrite process algebra class to check if history axioms are correct") {
    override def apply_pass(arg: PassReport, args: Array[String]) = {
      val input = arg.getOutput
      val res = new PassReport(input)
      val map = new ErrorMapping(arg)
      res.add(map)
      res.setOutput(new CheckHistoryAlgebra(input, CheckHistoryAlgebra.Mode.AxiomVerification, map).rewriteAll)
      res
    }
  })
  defined_passes.put("check-history", new Pass("rewrite process algebra class to check if history accounting is correct") {
    override def apply_pass(arg: PassReport, args: Array[String]) = {
      val input = arg.getOutput
      val res = new PassReport(input)
      val map = new ErrorMapping(arg)
      res.add(map)
      res.setOutput(new CheckHistoryAlgebra(input, CheckHistoryAlgebra.Mode.ProgramVerification, map).rewriteAll)
      res
    }
  })
  defined_passes.put("csl-encode", new Pass("Encode CSL atomic regions with methods") {
    override def apply_pass(arg: PassReport, args: Array[String]) = {
      val input = arg.getOutput
      val res = new PassReport(input)
      val map = new ErrorMapping(arg)
      res.add(map)
      res.setOutput(new CSLencoder(input, map).rewriteAll)
      res
    }
  })
  defined_passes.put("class-conversion", SimplePass("Convert classes into records and procedures", (arg, args) => new ClassConversion(arg).rewriteAll))
  defined_passes.put("codegen", new Pass("Generate code") {
    override def apply(report: PassReport, arg: ProgramUnit, args: Array[String]): ProgramUnit = {
      val dir = new File(args(0))
      if (dir.exists) if (!dir.isDirectory) {
        report.fatal("%s is not a directory", dir)
        return arg
      }
      else if (!dir.mkdirs) {
        report.fatal("could not create %s", dir)
        return arg
      }
      val syntax = JavaSyntax.getJava(JavaDialect.JavaVerCors)
      for (node <- arg.asScala) {
        if (node.isInstanceOf[ASTClass]) {
          var out: PrintWriter = null
          try out = new PrintWriter(new FileOutputStream(new File(dir, node.asInstanceOf[ASTClass].name + ".java")))
          catch {
            case e: FileNotFoundException =>
              report.add(new ExceptionMessage(e))
              return arg
          }
          out.println("import col.lang.*;")
          syntax.print(out, node)
          out.close()
        }
        else if (node.isInstanceOf[ASTSpecial]) {
          val S = node.asInstanceOf[ASTSpecial]
          S.kind match {
            case ASTSpecial.Kind.Comment =>
            // TODO keep comments.
            case _ =>
              report.fatal("cannot deal with special %s yet", S.kind)
              return arg
          }
        }
        else {
          report.fatal("cannot deal with %s yet", node.getClass())
          return arg
        }
      }
      arg
    }
  })
  defined_passes.put("current_thread", SimplePass("Encode references to current thread.", (arg, args) => new CurrentThreadRewriter(arg).rewriteAll))
  defined_passes.put("java-encode", SimplePass("Encode Java overloading and inheritance", (arg, args) => new JavaEncoder(arg).rewriteAll))
  defined_passes.put("explicit_encoding", SimplePass("encode required and ensured permission as ghost arguments", (arg, args) => new ExplicitPermissionEncoding(arg).rewriteAll))
  defined_passes.put("finalize_args", SimplePass("???", (arg, args) => new FinalizeArguments(arg).rewriteAll))
  defined_passes.put("flatten", SimplePass("remove nesting of expression", (arg, args) => new Flatten(arg).rewriteAll))
  defined_passes.put("ghost-lift", SimplePass("Lift ghost code to real code", (arg, args) => new GhostLifter(arg).rewriteAll))
  defined_passes.put("globalize", SimplePass("split classes into static and dynamic parts", (arg, args) => new GlobalizeStaticsParameter(arg).rewriteAll))
  defined_passes.put("ds_inherit", SimplePass("rewrite contracts to reflect inheritance, predicate chaining", (arg, args) => new DynamicStaticInheritance(arg).rewriteOrdered))
  defined_passes.put("flatten_before_after", SimplePass("move before/after instructions", (arg, args) => new FlattenBeforeAfter(arg).rewriteAll))
  defined_passes.put("flatten_variable_declarations", SimplePass("put the base type in declarations", (arg, args) => new FlattenVariableDeclarations(arg).rewriteAll))
  defined_passes.put("inline", SimplePass("Inline all methods marked as inline", (arg, args) => new InlinePredicatesRewriter(arg).rewriteAll))
  defined_passes.put("kernel-split", SimplePass("Split kernels into main, thread and barrier.", (arg, args) => new KernelRewriter(arg).rewriteAll))
  defined_passes.put("pvl-encode", SimplePass("Encode PVL builtins for verification.", (arg, args) => new PVLEncoder(arg).rewriteAll))
  defined_passes.put("magicwand", new Pass("Encode magic wand proofs with abstract predicates") {
    override protected def apply(reportIn: PassReport, arg: ProgramUnit, args: Array[String]): ProgramUnit = {
      val arg = reportIn.getOutput
      val reportOut = new PassReport(arg)
      val map = new ErrorMapping(reportIn)
      try {
        val result = new WandEncoder(arg, map).rewriteAll
        reportOut.setOutput(result)
      } catch {
        case e: Exception =>
          throw new HREError("unexpected exception %s", e)
      }
    }
  })
  defined_passes.put("modifies", SimplePass("Derive modifies clauses for all contracts", (arg, args) => {
    new DeriveModifies().annotate(arg)
    arg
  }))
  defined_passes.put("openmp2pvl", SimplePass("Compile OpenMP pragmas to PVL", (arg, args) => new OpenMPToPVL(arg).rewriteAll))
  defined_passes.put("parallel_blocks", new Pass("Encoded the proof obligations for parallel blocks") {
    override def apply_pass(arg: PassReport, args: Array[String]) = {
      val input = arg.getOutput
      val res = new PassReport(input)
      val map = new ErrorMapping(arg)
      res.add(map)
      res.setOutput(new ParallelBlockEncoder(input, map).rewriteAll)
      res
    }
  })
  defined_passes.put("pvl-compile", SimplePass("Compile PVL classes to Java classes", (arg, args) => new PVLCompiler(arg).rewriteAll))
  defined_passes.put("reorder", SimplePass("reorder statements (e.g. all declarations at the start of a bock", (arg, args) => new ReorderAssignments(arg).rewriteAll))
  defined_passes.put("standardize-functions", SimplePass("translate pure methods to function syntax.", (arg, args) => new PureMethodsAsFunctions(arg).rewriteAll))
  defined_passes.put("java_resolve", SimplePass("Resolve the library dependencies of a java program", (arg, args) => new JavaResolver(arg).rewriteAll))
  defined_passes.put("propagate-invariants", SimplePass("propagate invariants", (arg, args) => new PropagateInvariants(arg).rewriteAll))
  defined_passes.put("quant-optimize", SimplePass("insert satisfyability checks for all methods", (arg, args) => new OptimizeQuantifiers(arg).rewriteAll))
  defined_passes.put("rewrite", SimplePass("Apply a term rewrite system", (arg, args) => {
    val trs = RewriteSystems.getRewriteSystem(args(0))
    trs.normalize(arg)
  }))
  defined_passes.put("rewrite_arrays", SimplePass("rewrite arrays to sequences of cells", (arg, args) => new RewriteArrayRef(arg).rewriteAll))
  defined_passes.put("generate_adt_functions", SimplePass("rewrite  standard operators on sequences to function definitions/calls", (arg, args) => new GenerateADTFunctions(arg).rewriteAll))
  defined_passes.put("infer_adt_types", SimplePass("Transform typeless collection constructors by inferring their types.", (arg, args) => new InferADTTypes(arg).rewriteAll))
  defined_passes.put("adt_operator_rewrite", SimplePass("rewrite PVL-specific ADT operators", (arg, args) => new ADTOperatorRewriter(arg).rewriteAll))
  defined_passes.put("rm_cons", SimplePass("???", (arg, args) => new ConstructorRewriter(arg).rewriteAll))
  defined_passes.put("sat_check", SimplePass("insert satisfyability checks for all methods", (arg, args) => new SatCheckRewriter(arg).rewriteAll))
  defined_passes.put("silver-class-reduction", SimplePass("reduce classes to single Ref class", (arg, args) => new SilverClassReduction(arg).rewriteAll))
  defined_passes.put("silver-reorder", SimplePass("move declarations from inside if-then-else blocks to top", (arg, args) => new SilverReorder(arg).rewriteAll))
  defined_passes.put("scale-always", SimplePass("scale every predicate invokation", (arg, args) => new ScaleAlways(arg).rewriteAll))
  defined_passes.put("silver-optimize", SimplePass("Optimize expressions for Silver", (arg, args) => {
    val trs = RewriteSystems.getRewriteSystem("silver_optimize")
    trs.normalize(arg)
  }))
  defined_passes.put("chalice-optimize", SimplePass("Optimize expressions for Chalice", (arg, args) => {
    val trs = RewriteSystems.getRewriteSystem("chalice_optimize")
    trs.normalize(arg)
  }))
  defined_passes.put("simplify_expr", SimplePass("Simplify expressions", (arg, args) => {
    val trs = RewriteSystems.getRewriteSystem("simplify_expr")
    trs.normalize(arg)
  }))
  defined_passes.put("simplify_quant", SimplePass("Simplify quantifications", (arg, args) => {
    val trs = RewriteSystems.getRewriteSystem("simplify_quant_pass1")
    var res = trs.normalize(arg)
    res = RewriteSystems.getRewriteSystem("simplify_quant_pass2").normalize(res)
    res
  }))
  defined_passes.put("simplify_sums", SimplePass("replace summations with provable functions", (arg, args) => {
    val trs = RewriteSystems.getRewriteSystem("summation")
    trs.normalize(arg)
  }))
  defined_passes.put("simplify_quant_relations", SimplePass("simplify quantified relational expressions", (arg, args) => new SimplifyQuantifiedRelations(arg).rewriteAll))
  defined_passes.put("standardize", SimplePass("Standardize representation", (arg, args) => new Standardize(arg).rewriteAll))
  defined_passes.put("strip_constructors", SimplePass("Strip constructors from classes", (arg, args) => new StripConstructors(arg).rewriteAll))
  defined_passes.put("voidcalls", new Pass("Replace return value by out parameter.") {
    override protected def apply(reportIn: PassReport, arg: ProgramUnit, args: Array[String]): ProgramUnit = {
      val arg = reportIn.getOutput
      val reportOut = new PassReport(arg)
      val map = new ErrorMapping(reportIn)
      try {
        val result = new VoidCalls(arg, map).rewriteAll
        reportOut.setOutput(result)
      } catch {
        case e: Exception =>
          throw new HREError("unexpected exception %s", e)
      }
    }
  })
  defined_passes.put("voidcallsthrown", SimplePass("Replace return value and thrown exceptions by out parameters.", (arg, args) => new VoidCallsThrown(arg).rewriteAll))
  defined_passes.put("vector-encode", SimplePass("Encode vector blocks using the vector library", (arg, args) => new VectorEncode(arg).rewriteAll))
  defined_passes.put("chalice-preprocess", SimplePass("Pre processing for chalice", (arg, args) => new ChalicePreProcess(arg).rewriteAll))
  defined_passes.put("simple_triggers", SimplePass("Add triggers to quantifiers if possible", (arg, args) => {
    var res = arg
    val `val` = Integer.valueOf(args(0))
    // First gather quantified variables for quantifiers without triggers.
    res = new OptimizeQuantifiers(res).rewriteAll
    // For quantifiers without triggers, and complex subscripts not containing quantified variables, add quantifier variable equal to the complex subscript.
    if ((`val` & 2) > 0) res = new RewriteComplexUnitSubscripts(res).rewriteAll
    // Try to add triggers for the now possibly simplified quantifiers.
    if ((`val` & 1) > 0) res = new AddSimpleTriggers(res).rewriteAll
    res
  }))
  defined_passes.put("count", SimplePass("Count nodes.", (arg, args) => {
    val cv = new NonLinCountVisitor(arg)
    cv.count()
    if (args.length == 1) Main.counters.put(args(0), cv)
    else Abort("Learn is used without an oracle")
    arg
  }))
  defined_passes.put("learn", SimplePass("Learn unit times from counted AST nodes.", (arg, args) => {
    if (args.length == 1) {
      val start_time = args(0).toLong
      val time = System.currentTimeMillis - start_time
      for (entry <- Main.counters.entrySet.asScala) {
        Oracle.tell(entry.getKey, entry.getValue, time)
      }
    }
    else Abort("Learn is used without a starting time.")
    arg
  }))
}