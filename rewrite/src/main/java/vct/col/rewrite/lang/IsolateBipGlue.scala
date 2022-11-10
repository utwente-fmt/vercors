package vct.col.rewrite.lang

import com.typesafe.scalalogging.LazyLogging
import vct.col.ast._

case object IsolateBipGlue extends LazyLogging {
  def isolate[G](p: Program[G]): Program[G] = {
    val declsNew: Seq[GlobalDeclaration[G]] = p.declarations.map {
      case ns: JavaNamespace[G] =>
        val cleanDecls: Seq[GlobalDeclaration[G]] = ns.declarations.flatMap { decl =>
          getBipGlues(decl) match {
            case Seq() => Seq(decl)
            case glues =>
              logger.info(decl.o.messageInContext("[info] Of following class, _only_ the containing BIP glue is analyzed"))
              glues.map(g => new JavaBipGlueJob[G](g)(g.o))
          }
        }
        new JavaNamespace[G](ns.pkg, ns.imports, cleanDecls)(ns.o)
      case decl => decl
    }

    Program(declsNew)(p.blame)(p.o)
  }

  def getBipGlues[G](d: Declaration[G]): Seq[Expr[G]] = d.transSubnodes.collect {
    case g @ JavaBipGlue(_) => g
  }
}
