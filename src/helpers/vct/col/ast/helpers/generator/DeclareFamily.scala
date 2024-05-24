package vct.col.ast.helpers.generator

import vct.col.ast.helpers.defn.Constants._
import vct.col.ast.helpers.defn.Naming._
import vct.col.ast.structure
import vct.col.ast.structure.{DeclaredNode, FamilyGenerator, NodeKind}

import java.nio.file.Path
import scala.meta._

class DeclareFamily extends FamilyGenerator {
  override def generate(out: Path, family: structure.Name, kind: NodeKind, nodes: Seq[structure.Name]): Unit =
    if(kind == DeclaredNode) {
      ResultStream.write(out.resolve(s"${family.base}FamilyDeclare.scala"), getDeclare(family))
    }

  def getDeclare(name: structure.Name): Source =
    source"""
      package $DeclarePackage

      trait ${declareTrait(name)}[Post] { this: ${typ(name)}[Post] with $Declaration[Post] =>
        def declare[Pre]()(implicit `~rw`: $AbstractRewriter[Pre, Post]): this.type = {
          `~rw`.${scopes(name.base)}.declare(this)
          this
        }

        def succeedOnly[Pre](pre: ${typ(name)}[Pre])(implicit `~rw`: $AbstractRewriter[Pre, Post]): this.type = {
          `~rw`.${scopes(name.base)}.succeedOnly(pre.asInstanceOf[${typ(name)}[Pre]], this)
          this
        }

        def succeed[Pre](pre: ${typ(name)}[Pre])(implicit `~rw`: $AbstractRewriter[Pre, Post]): this.type = {
          this.declare()
          this.succeedOnly(pre)
        }
      }
    """
}
