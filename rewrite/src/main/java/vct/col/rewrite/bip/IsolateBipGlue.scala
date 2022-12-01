package vct.col.rewrite.bip

import com.typesafe.scalalogging.LazyLogging
import vct.col.ast._
import vct.col.origin.Origin

case object IsolateBipGlue extends LazyLogging {
  def isolate[G](p: Program[G]): Program[G] = {
    val declsNew: Seq[GlobalDeclaration[G]] = p.declarations.map {
      case ns: JavaNamespace[G] =>
        val cleanDecls: Seq[GlobalDeclaration[G]] = ns.declarations.flatMap { decl =>
          getBipGlues(decl) match {
            case Seq() => Seq(decl)
            case glues =>
              logger.info(Origin.messagesInContext(
                (decl.o, s"In this class, only the following ${glues.size} glues are kept:") +:
                  glues.zipWithIndex.map { case (g, i) => (g.o, s"glue ${i + 1}") }
              ))
              glues.map(g => new JavaBipGlueContainer[G](g)(g.o))
          }
        }
        new JavaNamespace[G](ns.pkg, ns.imports, cleanDecls)(ns.o)
      case decl => decl
    }

    Program(declsNew)(p.blame)(p.o)
  }

  def getBipGlues[G](d: Declaration[G]): Seq[JavaBipGlue[G]] = d.transSubnodes.collect {
    case g @ JavaBipGlue(_) => g
  }
}
