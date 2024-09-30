import $meta._
import $ivy.`com.lihaoyi::mill-contrib-buildinfo:`

import util._
import os._
import mill.{util => _, _}
import scalalib.{JavaModule => _, ScalaModule => _, _}
import contrib.buildinfo.BuildInfo
import me.pieterbos.mill.cpp.options.implicits._
import me.pieterbos.mill.cpp.options.{CppCompileOptions, CppExecutableOptions}
import me.pieterbos.mill.cpp.{CMakeModule, LinkableModule}
import me.pieterbos.mill.cpp.toolchain.GccCompatible
import mill.util.Jvm
import vct.col.ast.structure
import vct.col.ast.structure.{AllFamilies, FamilyDefinition, Name, NodeDefinition}

import scala.util.control.NonFatal

object external extends Module {
  object z3 extends Module {
    def url = T { "https://www.sosy-lab.org/ivy/org.sosy_lab/javasmt-solver-z3/com.microsoft.z3-4.8.7.jar" }

    def classPath = T {
      os.write(T.dest / "z3.jar", requests.get.stream(url()))
      PathRef(T.dest / "z3.jar")
    }
  }

  object antlr extends Module {
    def url = T {
      "https://github.com/pieter-bos/antlr4/releases/download/4.8-extractors-2/antlr4.jar"
    }

    def classPath = T {
      os.write(T.dest / "antlr.jar", requests.get.stream(url()))
      PathRef(T.dest / "antlr.jar")
    }
  }
}

object viper extends ScalaModule {
  object silverGit extends GitModule {
    def url = T { "https://github.com/viperproject/silver.git" }
    def commitish = T { "4a8065758868eae3414f86f3d96e843a283444fc" }
    def filteredRepo = T {
      val workspace = repo()
      os.remove.all(workspace / "src" / "test")
      workspace
    }
  }

  object siliconGit extends GitModule {
    def url = T { "https://github.com/viperproject/silicon.git" }
    def commitish = T { "4033dd21614b3bbba9c7615655e41c6cf0b9d80b" }
    def filteredRepo = T {
      val workspace = repo()
      os.remove.all(workspace / "src" / "test")
      os.remove(workspace / "src" / "main" / "resources" / "logback.xml")
      workspace
    }
  }

  object carbonGit extends GitModule {
    def url = T { "https://github.com/viperproject/carbon.git" }
    def commitish = T { "758481ef42f42720c36406bb278820ba802c7e68" }
    def filteredRepo = T {
      val workspace = repo()
      os.remove.all(workspace / "src" / "test")
      os.remove(workspace / "src" / "main" / "resources" / "logback.xml")
      workspace
    }
  }

  object silver extends ScalaModule {
    override def scalaVersion = "2.13.10"
    override def scalacOptions = T { Seq("-Xno-patmat-analysis", "-nowarn") }
    def repo = silverGit
    override def sources = T.sources { repo.filteredRepo() / "src" / "main" / "scala" }
    override def resources = T.sources { repo.filteredRepo() / "src" / "main" / "resources" }
    override def ivyDeps = settings.deps.log ++ Agg(
      ivy"org.scala-lang:scala-reflect:2.13.10",
      ivy"org.scalatest::scalatest:3.1.2",
      ivy"org.scala-lang.modules::scala-parser-combinators:1.1.2",
      ivy"com.lihaoyi::fastparse:2.2.2",
      ivy"org.rogach::scallop:4.0.4",
      ivy"commons-io:commons-io:2.8.0",
      ivy"com.google.guava:guava:29.0-jre",
      ivy"org.jgrapht:jgrapht-core:1.5.0",
      ivy"com.lihaoyi::requests:0.3.0",
      ivy"com.lihaoyi::upickle:1.0.0",
    )
  }

  object silicon extends ScalaModule {
    object buildInfo extends BuildInfo with ScalaModule {
      def buildInfoPackageName = "viper.silicon"
      override def buildInfoMembers = T {
        Seq(
          BuildInfo.Value("projectName", "silicon"),
          BuildInfo.Value("projectVersion", "1.1-SNAPSHOT"),
          BuildInfo.Value("scalaVersion", silicon.scalaVersion()),
          BuildInfo.Value("sbtVersion", "-"),
          BuildInfo.Value("gitRevision", silicon.repo.commitish()),
          BuildInfo.Value("gitBranch", "(detached)"),
        )
      }
    }

    object common extends ScalaModule {
      override def scalaVersion = "2.13.10"
      override def scalacOptions = T { Seq("-Xno-patmat-analysis", "-nowarn") }
      override def sources = T.sources { silicon.repo.filteredRepo() / "common" / "src" / "main" / "scala" }
      override def moduleDeps = Seq(silver)
    }

    override def scalaVersion = "2.13.10"
    override def scalacOptions = T { Seq("-Xno-patmat-analysis", "-nowarn") }
    def repo = siliconGit
    override def sources = T.sources { repo.filteredRepo() / "src" / "main" / "scala" }
    override def ivyDeps = settings.deps.log ++ Agg(
      ivy"org.apache.commons:commons-pool2:2.9.0",
      ivy"io.spray::spray-json:1.3.6",
    )
    override def resources = T.sources {
      repo.filteredRepo() / "src" / "main" / "resources"
    }
    override def unmanagedClasspath = Agg(external.z3.classPath())
    override def moduleDeps = Seq(silver, common, buildInfo)
  }

  object carbon extends ScalaModule {
    override def scalaVersion = "2.13.10"
    override def scalacOptions = T { Seq("-Xno-patmat-analysis", "-nowarn") }
    def repo = carbonGit
    override def sources = T.sources { repo.filteredRepo() / "src" / "main" / "scala" }
    override def ivyDeps = settings.deps.log
    override def moduleDeps = Seq(silver)
    override def resources = T.sources { repo.filteredRepo() / "src" / "main" / "resources" }
  }

  override def moduleDeps = Seq(silver, silicon, carbon)
}

object vercors extends Module {
  def read[T: upickle.default.ReadWriter](p: PathRef): T =
    upickle.default.read[T](p.path.toNIO)

  def readAndWatch[T: upickle.default.ReadWriter](p: Path): T =
    upickle.default.read[T](interp.watch(p).toNIO)

  object hre extends VercorsModule {
    def key = "hre"
    def deps = Agg(
      ivy"org.fusesource.jansi:jansi:2.4.0",
      ivy"net.harawata:appdirs:1.2.1",
      ivy"net.java.dev.jna:jna:5.13.0",
    )
    override def moduleDeps = Seq(pprofProto)

    object pprofProto extends ScalaPBModule {
      override def scalaPBSources = hre.sources
      override def scalaPBFlatPackage = true
    }
  }

  object col extends VercorsModule {
    object helpers extends Module {
      import upickle.default.write

      def analysis: Path = settings.meta / "analyseNodeDeclarations.dest"
      def structureClasspathDir: Path = settings.meta / "structureClassPath.dest"

      val familyCross = readAndWatch[Seq[Name]](analysis / "cross-family.json")
      val nodeCross = readAndWatch[Seq[Name]](analysis / "cross-node.json")

      def allFamiliesSource = T.source(analysis / "all-families.json")
      def allFamilies = T { read[AllFamilies](allFamiliesSource()) }
      def allDefinitionsSource = T.source(analysis / "all-definitions.json")
      def allDefinitions = T { read[Seq[NodeDefinition]](allDefinitionsSource()) }

      // Step 1: compile
      object generators extends VercorsModule {
        def key = "helpers"
        def deps: T[Agg[Dep]] = T {
          Agg(
            ivy"org.scalameta::scalameta:4.8.15",
          )
        }
        def structureClasspath = T.source(structureClasspathDir / "classpath.json")
        def unmanagedClasspath = T { read[Seq[Path]](structureClasspath()).map(PathRef(_)) }
      }

      def instantiate[T](name: String): Task[T] = T.task {
        mill.api.ClassLoader
          .create(
            generators.runClasspath().map(_.path.toIO.toURI.toURL),
            parent = null,
            sharedLoader = generators.getClass.getClassLoader,
            sharedPrefixes = Seq("vct.col.ast.structure.api"),
          )
          .loadClass(s"vct.col.ast.helpers.generator.$name")
          .asInstanceOf[Class[T]]
          .getDeclaredConstructor()
          .newInstance()
      }

      object implTraits extends Module {
        val root = settings.src / "col" / "vct" / "col" / "ast"

        def inputs = T.sources(os.walk(root).filter(_.last.endsWith("Impl.scala")).map(PathRef(_)))

        private def nodeName(p: PathRef): String =
          p.path.last.dropRight("Impl.scala".length)

        def generator = T.worker(instantiate[structure.api.ImplTraitGeneratorApi]("ImplTrait"))

        def allDefinitionsSet = T {
          allDefinitions().map(_.name.base).toSet
        }

        def allFamiliesSet = T {
          val allFamiliesVal = allFamilies()
          (allFamiliesVal.declaredFamilies ++ allFamiliesVal.structuralFamilies)
            .map(_.base).toSet
        }

        def fix = T {
          val gen = generator()
          val defnSet = allDefinitionsSet()
          val familySet = allFamiliesSet()
          inputs().foreach { input =>
            val name = nodeName(input)
            val opsNames =
              (if(defnSet.contains(name)) Seq(name + "Ops") else Nil) ++
                (if(familySet.contains(name)) Seq(name + "FamilyOps") else Nil)

            try {
              gen.fix(input.path.toNIO, write(opsNames))
            } catch {
              case NonFatal(_) => // ok, it's best effort.
            }
          }
        }

        def generate: T[Unit] = T {
          val gen = generator()
          val defnSet = allDefinitionsSet()
          val familySet = allFamiliesSet()
          val ungenerated = (defnSet ++ familySet) -- inputs().map(nodeName)
          ungenerated.foreach { node =>
            os.makeDir.all(root / "unsorted")
            gen.generate(
              p = (root / "unsorted" / (node + "Impl.scala")).toNIO,
              node = node,
              concrete = defnSet.contains(node),
              family = familySet.contains(node),
            )
          }

          if(ungenerated.nonEmpty)
            T.log.error({
              val items = ungenerated.map(name => s" - ${name}Impl").mkString("\n")
              s"""
                 |New {Node}Impl stubs have been generated for these nodes:
                 |$items
                 |Please make sure that the appropriate {Node} extends its corresponding {Node}Impl - you can do that before it exists.
                 |If not, compilation will fail after this message.""".stripMargin
            })

          ()
        }

        def run = T {
          fix()
          generate()
        }
      }

      def protoAuxTypes = T.worker {
        Seq(
          instantiate[structure.api.AllNodesGeneratorApi]("ProtoAuxTypes")(),
          instantiate[structure.api.AllNodesGeneratorApi]("RewriteHelpers")(),
          instantiate[structure.api.AllNodesGeneratorApi]("MegaCol")(),
        )
      }

      def allFamiliesGenerators = T.worker {
        Seq(
          instantiate[structure.api.AllFamiliesGeneratorApi]("AbstractRewriter")(),
          instantiate[structure.api.AllFamiliesGeneratorApi]("AllScopes")(),
          instantiate[structure.api.AllFamiliesGeneratorApi]("AllFrozenScopes")(),
          instantiate[structure.api.AllFamiliesGeneratorApi]("BaseCoercingRewriter")(),
          instantiate[structure.api.AllFamiliesGeneratorApi]("BaseNonLatchingRewriter")(),
          instantiate[structure.api.AllFamiliesGeneratorApi]("SuccessorsProvider")(),
        )
      }

      def familyGenerators = T.worker {
        Seq(
          instantiate[structure.api.FamilyGeneratorApi]("DeserializeFamily")(),
          instantiate[structure.api.FamilyGeneratorApi]("DeclareFamily")(),
          instantiate[structure.api.FamilyGeneratorApi]("OpsFamily")(),

          instantiate[structure.api.FamilyGeneratorApi]("ProtoFamily")(),
        )
      }

      def nodeGenerators = T.worker {
        Seq(
          instantiate[structure.api.NodeGeneratorApi]("Compare")(),
          instantiate[structure.api.NodeGeneratorApi]("Rewrite")(),
          instantiate[structure.api.NodeGeneratorApi]("Serialize")(),
          instantiate[structure.api.NodeGeneratorApi]("Subnodes")(),
          instantiate[structure.api.NodeGeneratorApi]("Ops")(),

          instantiate[structure.api.NodeGeneratorApi]("Deserialize")(),
          instantiate[structure.api.NodeGeneratorApi]("ProtoNode")(),
        )
      }

      object global extends Module {
        def generate = T {
          val allFamiliesVal = allFamilies()
          val declarationFamilies = allFamiliesVal.declaredFamilies
          val structuralFamilies = allFamiliesVal.structuralFamilies
          val definitions = allDefinitions()
          allFamiliesGenerators().foreach { gen =>
            T.log.debug(s"Generating ${gen.getClass.getSimpleName}")
            gen.generate(T.dest.toNIO, write(declarationFamilies), write(structuralFamilies))
          }
          protoAuxTypes().foreach(_.generate(T.dest.toNIO, write(definitions)))
          PathRef(T.dest, quick = true)
        }
      }

      implicit object NameSegments extends Cross.ToSegments[structure.Name](v => v.parts.toList)
      implicit object FamilySegments extends Cross.ToSegments[(structure.Name, structure.NodeKind, Seq[structure.Name])](v => implicitly[Cross.ToSegments[structure.Name]].convert(v._1))

      object family extends Cross[FamilyCross](familyCross)
      trait FamilyCross extends Cross.Module[Name] {
        def source = T.source(PathRef(analysis / "cross-family" / crossValue.path / "family.json", quick = true))
        def sourceDefn = T { read[FamilyDefinition](source()) }
        def generate = T {
          val defn = sourceDefn()
          familyGenerators().foreach { gen =>
            T.log.debug(s"Generating ${gen.getClass.getSimpleName} for ${defn.name.base}")
            gen.generate(T.dest.toNIO, write(defn.name), write(defn.kind), write(defn.nodes))
          }
          PathRef(T.dest, quick = true)
        }
      }

      object node extends Cross[NodeCross](nodeCross)
      trait NodeCross extends Cross.Module[Name] {
        def source = T.source(PathRef(analysis / "cross-node" / crossValue.path / "node.json", quick = true))
        def sourceDefn = T { read[NodeDefinition](source()) }
        def generate = T {
          val defn = sourceDefn()
          nodeGenerators().foreach { gen =>
            T.log.debug(s"Generating ${gen.getClass.getSimpleName} for ${defn.name.base}")
            gen.generate(T.dest.toNIO, write(defn))
          }
          PathRef(T.dest, quick = true)
        }
      }

      def generatedSources: T[Seq[PathRef]] = T {
        T.traverse(nodeCross)(node(_).generate)() ++
          T.traverse(familyCross)(family(_).generate)() :+
          global.generate()
      }

      def megacol: T[PathRef] = T.source(global.generate().path / "vct" / "col" / "ast" / "col.proto")

      def sources: T[PathRef] = T.persistent {
        util.quickCopy(T.dest, generatedSources())
        os.remove(T.dest / "vct" / "col" / "ast" / "col.proto")
        PathRef(T.dest, quick = true)
      }
    }

    def key = "col"
    def deps = T { Agg.empty }
    override def sources = T {
      helpers.implTraits.run()
      super.sources()
    }
    override def generatedSources = T { Seq(helpers.sources()) }
    override def moduleDeps = Seq(hre, serialize)

    object test extends Tests
  }

  object serialize extends VercorsModule with ScalaPBModule {
    override def scalaPBSources = T {
      col.helpers.sources() +: this.sources()
    }
    override def key: String = "serialize"
    override def deps: T[Agg[Dep]] = T { Agg.empty }
  }

  object parsers extends VercorsModule {
    def key = "parsers"
    override def generatedSources = T.sources {
      Seq(
        c.generate(),
        cpp.generate(),
        java.generate(),
        pvl.generate(),
        llvm.generate(),
      )
    }
    def deps = Agg(
      ivy"org.antlr:antlr4-runtime:4.8",
      ivy"org.apache.logging.log4j:log4j-to-slf4j:2.23.1",
    )
    override def moduleDeps = Seq(hre, col, serialize)

    val includeVcllvmCross = interp.watchValue {
      if(os.exists(settings.root / ".include-vcllvm")) {
        Seq("vcllvm")
      } else {
        Seq.empty[String]
      }
    }

    object vcllvmDep extends Cross[VcllvmDep](includeVcllvmCross)
    trait VcllvmDep extends Cross.Module[String] {
      def path = T {
        vcllvm.compile().path / os.up
      }
    }

    override def bareResourcePaths = T {
      T.traverse(includeVcllvmCross.map(vcllvmDep(_)))(_.path)()
    }

    trait GenModule extends Module {
      def base = T { settings.src / "parsers" / "antlr4" }

      def lexer: String
      final def lexerRef = T.sources { base() / lexer }

      def parser: String
      final def parserRef = T.sources { base() / parser }

      def deps: Seq[String]
      final def depsRef = T.sources { deps.map(dep => base() / dep).map(PathRef(_)) }

      def generate = T {
        def runAntlr(target: os.Path, args: Seq[String] = Nil): Unit = {
          val mainArgs = Seq(
            "-encoding", "utf-8",
            "-package", "vct.antlr4.generated",
            "-lib", base().toString,
            "-o", T.dest.toString,
            target.toString
          ) ++ args

          Jvm.runSubprocess(
            mainClass = "org.antlr.v4.Tool",
            classPath = Agg(external.antlr.classPath().path),
            mainArgs = mainArgs
          )
        }

        depsRef()
        lexerRef()
        parserRef()

        runAntlr(base() / lexer)
        runAntlr(base() / parser, args = Seq("-listener", "-visitor", "-scala-extractor-objects"))
        PathRef(T.dest)
      }
    }

    object c extends GenModule {
      def lexer = "LangCLexer.g4"
      def parser = "CParser.g4"
      def deps = Seq(
        "SpecParser.g4", "SpecLexer.g4",
        "LangCParser.g4", "LangCLexer.g4",
        "LangOMPParser.g4", "LangOMPLexer.g4",
        "LangGPGPUParser.g4", "LangGPGPULexer.g4",
      )
    }

    object cpp extends GenModule {
      def lexer = "LangCPPLexer.g4"
      def parser = "CPPParser.g4"
      def deps = Seq(
        "SpecParser.g4", "SpecLexer.g4",
        "LangCPPParser.g4", "LangCPPLexer.g4"
      )
    }

    object java extends GenModule {
      def lexer = "LangJavaLexer.g4"
      def parser = "JavaParser.g4"
      def deps = Seq(
        "SpecParser.g4", "SpecLexer.g4",
        "LangJavaParser.g4", "LangJavaLexer.g4",
      )
    }

    object pvl extends GenModule {
      def lexer = "LangPVLLexer.g4"
      def parser = "PVLParser.g4"
      def deps = Seq(
        "SpecParser.g4", "SpecLexer.g4",
        "LangPVLParser.g4", "LangPVLLexer.g4",
      )
    }

    object llvm extends GenModule {
      def lexer = "LangLLVMSpecLexer.g4"
      def parser = "LLVMSpecParser.g4"
      def deps = Seq(
        "SpecParser.g4", "SpecLexer.g4",
        "LangLLVMSpecParser.g4", "LangLLVMSpecLexer.g4"
      )
    }
  }

  object rewrite extends VercorsModule {
    def key = "rewrite"
    def deps = Agg(
      ivy"org.sosy-lab:java-smt:3.14.3",
      ivy"com.lihaoyi::upickle:2.0.0",
      ivy"org.antlr:antlr4-runtime:4.8",
    )
    override def moduleDeps = Seq(hre, col)
  }

  object viperApi extends VercorsModule {
    def key = "viper"
    def deps = Agg(
      ivy"org.scalatest::scalatest:3.2.7"
    )
    override def moduleDeps = Seq(hre, col, parsers, viper)

    object test extends Tests
  }

  object main extends VercorsModule {
    def key = "main"
    def name = "VerCors"
    def dockerName = DockerImageName.Public("utwentefmt", "vercors")
    def maintainer = "Pieter Bos <p.h.bos@utwente.nl>"
    def homepage = Some("https://utwente.nl/vercors")
    def executableName = "vercors"
    def version = T { buildInfo.gitVersion() }
    def dockerVersion = T { Some(buildInfo.gitDockerVersion()) }
    def summary = "A deductive verifier for concurrent and parallel software."
    def dockerAptDependencies = Seq("clang", "libllvm15")
    def description =
      """The VerCors verifier is a tool for deductive verification of concurrent
        |and parallel software. VerCors can reason about programs written in
        |different programming languages, such as Java, C and OpenCL, where
        |the specifications are written in terms of pre-post-condition
        |contracts using permission-based separation logic.""".stripMargin

    def githubReleaseOutputs() = T.command {
      System.out.println(s"TAG_NAME=${buildInfo.gitVersionTag().get}")
      val releaseName = if(buildInfo.gitIsPrerelease()) "VerCors Nightly" else s"${name()} ${version()}"
      System.out.println(s"RELEASE_NAME=$releaseName")
      System.out.println(s"BODY=${if(buildInfo.gitIsPrerelease()) "Nightly Build" else "..."}")
      System.out.println(s"PRERELEASE=${if(buildInfo.gitIsPrerelease()) "true" else "false"}")
    }

    def deps = Agg(
      ivy"com.github.scopt::scopt:4.0.1",
    )
    override def moduleDeps = Seq(hre, col, rewrite, parsers, viperApi, buildInfo)
    override def mainClass = Some("vct.main.Main")
    override def runScriptClasses = T { Map (
      "vercors" -> "vct.main.Main",
      "carbon" -> "viper.carbon.Carbon",
      "silicon" -> "viper.silicon.SiliconRunner",
      "bashOptions" -> "vct.options.BashCompletion",
    ) }
    override def packedResources = T.sources()
    override def bareResourcePaths = T {
      Seq(
        settings.res / "universal" / "res",
        settings.res / "universal" / "deps",
      )
    }

    object test extends Tests

    object buildInfo extends BuildInfo with ScalaModule {
      def callOrElse(command: Shellable*)(alt: => String): String =
        try {
          os.proc(command: _*).call().out.text().trim
        } catch {
          case _: SubprocessException => alt
        }

      def gitBranch = T.input { callOrElse("git", "rev-parse", "--abbrev-ref=strict", "HEAD")("unknown") }
      def gitCommit = T.input { callOrElse("git", "rev-parse", "HEAD")("unknown") }
      def gitShortCommit = T.input { callOrElse("git", "rev-parse", "--short=8", "HEAD")("unknown") }
      def gitHasChanges = T.input { callOrElse("git", "diff-index", "--name-only", "HEAD")("dummyChanges").nonEmpty }

      def gitVersionTag = T.input {
        val tags = callOrElse("git", "tag", "--points-at", "HEAD")("").split("\n")
        tags.collectFirst {
          case tag if tag.matches("^v[0-9].*") => tag
          case "dev-prerelease" => "dev-prerelease"
        }
      }

      def gitIsPrerelease = T.input { gitVersionTag().contains("dev-prerelease") }

      def gitVersion = T.input {
        gitVersionTag() match {
          case Some(tag) if tag.startsWith("v") => tag.substring(1)
          case _ => "9999.9.9-SNAPSHOT"
        }
      }

      def gitDockerVersion: T[String] = T.input {
        gitVersionTag() match {
          case Some(tag) if tag.startsWith("v") => tag.substring(1)
          case _ => "latest"
        }
      }

      def buildInfoPackageName = "vct.main"
      override def buildInfoMembers = T {
        Seq(
          BuildInfo.Value("name", "VerCors"),
          BuildInfo.Value("version", gitVersion()),
          BuildInfo.Value("scalaVersion", main.scalaVersion()),
          BuildInfo.Value("sbtVersion", "-"),
          BuildInfo.Value("currentBranch", gitBranch()),
          BuildInfo.Value("currentCommit", gitCommit()),
          BuildInfo.Value("currentShortCommit", gitShortCommit()),
          BuildInfo.Value("gitHasChanges", gitHasChanges().toString),
          BuildInfo.Value("silverCommit", viper.silver.repo.commitish()),
          BuildInfo.Value("siliconCommit", viper.silicon.repo.commitish()),
          BuildInfo.Value("carbonCommit", viper.carbon.repo.commitish()),
        )
      }
    }
  }

  object vcllvm extends CppExecutableModule {
    outer =>
    def root: T[os.Path] = T {
      settings.src / "llvm"
    }

    object llvm extends LinkableModule {
      def moduleDeps = Nil

      def systemLibraryDeps = T {
        Seq("LLVM-15")
      }

      def staticObjects = T {
        Seq.empty[PathRef]
      }

      def dynamicObjects = T {
        Seq.empty[PathRef]
      }

      def exportIncludePaths = T.sources(
        os.Path("/usr/include/llvm-15"),
        os.Path("/usr/include/llvm-c-15"),
        os.Path("/usr/local/opt/llvm/include")
      )
    }

    object json extends LinkableModule {
      def moduleDeps = Nil

      def systemLibraryDeps = T {
        Seq.empty[String]
      }

      def staticObjects = T {
        Seq.empty[PathRef]
      }

      def dynamicObjects = T {
        Seq.empty[PathRef]
      }

      def exportIncludePaths = T {
        os.write(T.dest / "json.tar.xz", requests.get.stream("https://github.com/nlohmann/json/releases/download/v3.11.2/json.tar.xz"))
        os.proc("tar", "-xf", T.dest / "json.tar.xz").call(cwd = T.dest)
        Seq(PathRef(T.dest / "json" / "include"))
      }
    }

    object origin extends CppModule {
      def moduleDeps = Seq(llvm, json, proto, protobuf.libprotobuf)

      def sources = T.sources(vcllvm.root() / "lib" / "origin")

      def includePaths = T.sources(vcllvm.root() / "include")

      override def unixToolchain = GccCompatible("g++", "ar")

    }

    object passes extends CppModule {
      def moduleDeps = Seq(llvm, proto, protobuf.libprotobuf)

      def sources = T.sources(vcllvm.root() / "lib" / "passes")

      def includePaths = T.sources(vcllvm.root() / "include")

      override def unixToolchain = GccCompatible("g++", "ar")

    }

    object transform extends CppModule {
      def moduleDeps = Seq(llvm, proto, protobuf.libprotobuf)

      def sources = T.sources(vcllvm.root() / "lib" / "transform")

      def includePaths = T.sources(vcllvm.root() / "include")

      override def unixToolchain = GccCompatible("g++", "ar")

    }

    object util extends CppModule {
      def moduleDeps = Seq(llvm, proto, protobuf.libprotobuf)

      def sources = T.sources(vcllvm.root() / "lib" / "util")

      def includePaths = T.sources(vcllvm.root() / "include")

      override def unixToolchain = GccCompatible("g++", "ar")

    }

    override def unixToolchain = GccCompatible("g++", "ar")

    def moduleDeps = Seq(origin, passes, transform, util, llvm, proto, protobuf.libprotobuf)

    def sources = T.sources(vcllvm.root() / "tools" / "vcllvm")

    def includePaths = T.sources(vcllvm.root() / "include")

    object protobuf extends CMakeModule {
      object protobufGit extends GitModule {
        override def url: T[String] = "https://github.com/protocolbuffers/protobuf"

        override def commitish: T[String] = "v25.2"

        override def fetchSubmodulesRecursively = true
      }

      override def root = T.source(protobufGit.repo())

      override def jobs = T {
        2
      }

      override def cMakeSetupBuild: T[os.Path] = T {
        val apiDir = T.dest / ".cmake" / "api" / "v1"
        os.makeDir.all(apiDir / "query")
        os.write(apiDir / "query" / "codemodel-v2", "")
        os.proc("cmake", "-B", T.dest, "-Dprotobuf_BUILD_TESTS=OFF", "-DABSL_PROPAGATE_CXX_STD=ON", "-S", root().path).call(cwd = T.dest)
        T.dest
      }

      object libprotobuf extends CMakeLibrary {
        def target = T {
          "libprotobuf"
        }
      }

      object protoc extends CMakeExecutable {
        def target = T {
          "protoc"
        }
      }
    }

    object proto extends CppModule {
      def protoPath = T.sources(
        vercors.col.helpers.megacol().path / os.up / os.up / os.up / os.up,
        settings.src / "serialize",
        serialize.scalaPBUnpackProto().path
      )

      def generate = T {
        os.proc(protobuf.protoc.executable().path,
          protoPath().map(p => "-I=" + p.path.toString),
          "--cpp_out=" + T.dest.toString,
          (Seq(vercors.col.helpers.megacol()) ++
            os.walk(serialize.scalaPBUnpackProto().path).filter(path => !path.startsWith(serialize.scalaPBUnpackProto().path / "google") && path.ext == "proto").map(PathRef(_)) ++
            os.walk(settings.src / "serialize").filter(_.ext == "proto").map(PathRef(_))).map(_.path)
        ).call()
        T.dest
      }

      override def moduleDeps = Seq(protobuf.libprotobuf)

      override def sources = T {
        Seq(PathRef(generate()))
      }

      override def includePaths = T {
        Seq(PathRef(generate()))
      }

      def precompileHeaders: T[PathRef] = T {
        def isHiddenFile(path: os.Path): Boolean = path.last.startsWith(".")

        val headers = for {
          root <- allSources()
          if os.exists(root.path)
          path <- if(os.isDir(root.path)) os.walk(root.path) else Seq(root.path)
          if os.isFile(path)
          if !isHiddenFile(path)
          if Seq("h", "hpp").contains(path.ext.toLowerCase)
        } yield (root.path, path.relativeTo(root.path))

        val options = CppCompileOptions(
          allIncludePaths().map(_.path),
          defines(),
          includes().map(_.path),
          standard(),
          optimization(),
          compileOptions(),
          compileEarlyOptions(),
        )

        for((base, header) <- headers) {
          val compileOut = toolchain.compile(base / header, T.dest, options)
          val outDir = T.dest / header / os.up
          val out = outDir / (header.last + ".gch")
          os.makeDir.all(outDir)
          os.move(compileOut, out)
          os.copy(base / header, T.dest / header)
        }

        PathRef(T.dest)
      }

      override def exportIncludePaths: T[Seq[PathRef]] = T {
        Seq(precompileHeaders())
      }

      override def unixToolchain = GccCompatible("g++", "ar")
    }
  }

  object allTests extends ScalaModule with ReleaseModule {
    def packedResources = T.sources()
    override def moduleDeps: Seq[JavaModule] = Seq(col.test, viperApi.test, main.test)

    override def mainClass = T { Some("org.scalatest.tools.Runner") }

    override def runScriptClasses = T {
      val paths = Seq(col.test.compile(), viperApi.test.compile(), main.test.compile())
      Map (
      "testSuite" -> ("org.scalatest.tools.Runner -R " + paths.map(_.classes.path.toString().replace(" ", "\\\\ ")).mkString("\""," ", "\""))
    ) }

    def test(args: String*) = T.command {
      col.test.test(args: _*)
      viperApi.test.test(args: _*)
      main.test.test(args: _*)
    }
  }
}
