package vct.main

import com.typesafe.scalalogging.LazyLogging
import hre.progress.Progress
import vct.col.ast._
import vct.col.check.CheckError
import vct.col.feature
import vct.col.newrewrite._
import vct.col.resolve.C
import vct.col.rewrite.{Generation, RewriterBuilder}
import vct.main.Vercors.TemporarilyUnsupported
import vct.options.Options
import vct.parsers.ParseResult
import vct.result.VerificationError
import vct.result.VerificationError.UserError
import viper.api.Silicon

import java.nio.file.Path

case object Vercors {
  case class TemporarilyUnsupported(feature: String, examples: Seq[Node[_]]) extends UserError {
    override def code: String = "unsupported"
    override def text: String =
      examples.head.o.messageInContext(
        s"The feature `$feature` is temporarily unsupported.")
  }
}

case class Vercors(options: Options) extends ImportADTImporter with LazyLogging {
  def parse[G](paths: Path*): ParseResult[G] =
    ???

  def resolve[G <: Generation](parse: Seq[GlobalDeclaration[G]], withJava: Boolean): Either[Seq[CheckError], Program[_ <: Generation]] =
    ???

  def passes: Seq[RewriterBuilder] = Nil

  def go(): VerificationError = try {
    Progress.stages(Seq(
      ("Parsing", 4),
      ("Translation", 3),
      ("Verification", 10),
    )) {
      val decls: Seq[GlobalDeclaration[_]] = ???
      val expectedErrors = ???
      Progress.nextPhase()

      decls.foreach(_.transSubnodes.foreach {
        case decl: CGlobalDeclaration[_] => decl.decl.inits.foreach(init => {
          if(C.getDeclaratorInfo(init.decl).params.isEmpty) {
            throw TemporarilyUnsupported("GlobalCVariable", Seq(decl))
          }
        })
        case addrOf: AddrOf[_] => throw TemporarilyUnsupported("&", Seq(addrOf))
        case ref: Refute[_] => throw TemporarilyUnsupported("Refute", Seq(ref))
        case _ =>
      })

      var program = ???

      val tempUnsupported = Set[feature.Feature](
        feature.JavaThreads,
        feature.MatrixVector,
        feature.NumericReductionOperator,
        feature.MagicWand,
        feature.Models,
      )

//      feature.Feature.examples(program).foreach {
//        case (feature, examples) if tempUnsupported.contains(feature) =>
//          throw TemporarilyUnsupported(feature.getClass.getSimpleName.stripSuffix("$"), examples.toSeq)
//        case (_, _) =>
//      }

      ???
    }
  } catch {
    case res: VerificationError => res
  }

  def helpPasses(): Unit = {
    println("Available passes:")
    for(pass <- passes) {
      println(s"- ${pass.key}")
      println(s"    ${pass.desc}")
    }
  }

  override def loadAdt[G](adtName: String): Program[G] = ???
}
