package vct.col.ast.analysis

import vct.col.ast.structure._
import Util._
import scala.meta.{Name => ScName, _}

object FileAnalysis {
  def namedDecls(input: Input): Result[Seq[RawStatAnalysis.RawStat]] = Try("analysis") {
    val source = input.parse[Source].fold((e: Parsed.Error) => fail(e.pos, e.message), identity)
    val (pkg, stats) = getStats(source).get
    stats.map(RawStatAnalysis.getRawStats).get.flatten.map(
      stat => stat.copy(name = pkg + stat.name)
    )
  }

  def getStats(source: Source): Result[(Name, Seq[Stat])] = Try("shape") {
    source match {
      case Source(List(Pkg(pkg, stats))) =>
        ("_root_" +: simpleName(pkg).get, stats)
      case Source(Nil) =>
        fail(source, "This source seems to be empty")
      case Source(List(other)) =>
        fail(other, "Expected a package declaration here")
      case Source(_ :: extra :: _) =>
        fail(extra, "Did not expect additional package declaration here")
    }
  }

  def simpleName(ref: Term): Result[Name] = Try("simple name extraction") {
    ref match {
      case Term.Name(name) => Name(Seq(name))
      case Term.Select(base, name) => simpleName(base).get + name.value
      case other => fail(other, "Not a simple name")
    }
  }
}
