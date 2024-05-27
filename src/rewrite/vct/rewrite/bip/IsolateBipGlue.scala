package vct.col.rewrite.bip

import com.typesafe.scalalogging.LazyLogging
import vct.col.ast._
import vct.col.origin.Origin
import vct.result.Message

/** This pass scans the program for bip glues. For each java namespace where
  * there is a glue, only the glue is included, not the namespace itself. This
  * is because for the JavaBIP paper this namespace containing the glue only
  * contains configuration code that vercors cannot analyze yet.
  */
case object IsolateBipGlue extends LazyLogging {
  def isolate[G](p: Program[G]): Program[G] = {
    val declsNew: Seq[GlobalDeclaration[G]] = p.declarations.map {
      case ns: JavaNamespace[G] =>
        val cleanDecls: Seq[GlobalDeclaration[G]] = ns.declarations
          .flatMap { decl =>
            getBipGlues(decl) match {
              case Seq() => Seq(decl)
              case glues =>
                logger.info(Message.messagesInContext(
                  (
                    decl.o,
                    s"In this class, only the following ${glues.size} glues are kept:",
                  ) +: glues.zipWithIndex.map { case (g, i) =>
                    (g.o, s"glue ${i + 1}")
                  }: _*
                ))
                glues.map(g => new JavaBipGlueContainer[G](g)(g.o))
            }
          }
        new JavaNamespace[G](ns.pkg, ns.imports, cleanDecls)(ns.o)
      case decl => decl
    }

    Program(declsNew)(p.blame)(p.o)
  }

  def getBipGlues[G](d: Declaration[G]): Seq[JavaBipGlue[G]] =
    d.transSubnodes.collect { case g @ JavaBipGlue(_) => g }
}
