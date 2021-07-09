package vct.col.rewrite

import vct.col.ast.stmt.decl.{ASTClass, Method, ProgramUnit}
import vct.col.ast.util.AbstractRewriter
import hre.lang.System.{Abort, Debug}
import vct.col.ast.`type`.ASTReserved
import scala.jdk.CollectionConverters._

private case class Target(className: String, methodName: String);

object MinimiseMarker {
  private def parseMethodReference(ref: String): Option[Target] = {
    if (!ref.contains("#")) {
      return None
    }

    val elems = ref.split("#")

    if (elems.length != 2) {
      Abort("""%s is not a valid method reference. Syntax: "com.example.MyClass#methodName"""", ref)
    }

    Some(Target(elems(0), elems(1)))
  }
}

class MinimiseMarker(arg: ProgramUnit, minimiseTargets: java.util.List[String]) extends AbstractRewriter(arg) {
  private val targets: Seq[Target] = minimiseTargets
    .asScala
    .flatMap(MinimiseMarker.parseMethodReference)
    .toSeq

  override def visit(m: Method): Unit = {
    super.visit(m)

    val currentTarget = Target(current_class.toString, m.getName)
    if (targets contains currentTarget) {
      Debug("marking: %s", currentTarget)
      result.attach(create.reserved_name(ASTReserved.MinimiseTarget))
    }
  }
}
