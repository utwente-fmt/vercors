package vct.col.ast.helpers.generator

import vct.col.ast.helpers.defn.Constants._
import vct.col.ast.helpers.defn.Naming._
import vct.col.ast.structure.{FamilyGenerator, Name, NodeKind}

import java.nio.file.Path
import scala.meta._

class DeserializeFamily extends FamilyGenerator {
  override def generate(
      out: Path,
      family: Name,
      kind: NodeKind,
      nodes: Seq[Name],
  ): Unit =
    ResultStream.write(
      out.resolve("DeserializeFamily" + family.base + ".scala"),
      deserializeFamily(family, nodes),
    )

  def deserializeFamily(name: Name, nodes: Seq[Name]): Source =
    source"""
      package $DeserializePackage

      object ${Term.Name(deserializeFamilyName(name))} {
        def deserialize[G](node: ${scalapbType(name)}, decls: $MutMap[$Long, $Declaration[G]], blameProvider: $Origin => $Blame[$VerificationFailure]): ${typ(name)}[G] =
          ${if (nodes == Seq(name))
        q"${Term.Name(deserializeName(name))}.deserialize[G](node, decls, blameProvider)"
      else
        deserializeOneof(q"node.v", name, nodes)}
      }
    """

  def deserializeOneof(term: Term, name: Name, nodes: Seq[Name]): Term =
    Term.Match(
      q"$term.number: @_root_.scala.annotation.switch",
      nodes.zipWithIndex.map { case (node, index) =>
        deserializeCase(term, node, number = index + 1)
      }.toList,
    )

  def deserializeCase(term: Term, node: Name, number: Int): Case =
    Case(
      Lit.Int(number),
      None,
      q"${deserializeObject(node)}.deserialize[G]($term.value.asInstanceOf[${scalapbType(node)}], decls, blameProvider)",
    )
}
