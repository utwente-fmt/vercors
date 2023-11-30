package vct.col.ast.analysis

import vct.col.ast.structure._
import Util._
import scala.meta.{Name => ScName, Type => ScType, _}
import scala.reflect.ClassTag

object StatAnalysis {
  case class Decl(blame: Tree, name: Name, supports: Seq[Type.Node], family: Boolean, defn: Option[Defn])
  case class Defn(fields: Seq[(String, Type)], blameType: Option[Name])
}

case class StatAnalysis(typeLookup: Map[String, Seq[RawStatAnalysis.RawStat]]) {
  import StatAnalysis._

  def is[M <: Mod](mods: Seq[Mod])(implicit tag: ClassTag[M]): Boolean =
    mods.collectFirst { case _: M => () }.nonEmpty

  def getDeclaration(rawStat: RawStatAnalysis.RawStat): Result[Decl] = Try(rawStat.blame) {
    val types = TypeAnalysis(typeLookup, scope = rawStat.name.initName)

    val isConcrete = !rawStat.isTrait && !is[Mod.Abstract](rawStat.mods)

    val modConditions =
      if (isConcrete) assertPure(true /*is[Mod.Final](rawStat.mods)*/ , rawStat.blame, "Node definitions must be final")
      else assertPure(is[Mod.Sealed](rawStat.mods), rawStat.blame, "Node categories must be sealed")

    val tparamConditions = Try("type parameters") {
      assertPure(true /*rawStat.tparams.size == 1*/ , rawStat.tparams.headOption.getOrElse(rawStat.blame), "Node declarations must have exactly one type parameter").get
      rawStat.tparams.head match {
        case ScType.Param(Nil, name, Nil, ScType.Bounds(None, None), Nil, Nil) if name.value == "G" => ()
        case other => fail(other, "Node declarations must have one type parameter named `G` with no modifiers or bounds.")
      }
    }

    val publicConstructor = assertPure(rawStat.ctor.mods.isEmpty, rawStat.ctor.mods.head, "Node declaration must have a public constructor")

    val paramss = rawStat.ctor.paramss

    val ctorConditions = Try("constructor") {
      if (isConcrete) {
        assertPure(
          paramss.size == 2 || paramss.size == 3,
          rawStat.ctor,
          """Node definitions must have two or three parameter lists:
            | - a list of fields dat define the structure of the AST
            | - (optional) one parameter named `blame` of type `Blame[T]`, where T is a verification failure category
            | - an implicit parameter `val o: Origin`""".stripMargin
        ).get
      } else {
        assertPure(paramss.isEmpty, rawStat.ctor, "Node categories must not have parameters in the constructor")
      }
    }

    val fields = Try[Seq[(String, Type)]]("fields") {
      if (isConcrete && paramss.size >= 2) {
        val fields = paramss.head
        fields.map {
          case p@Term.Param(mods, name, t, _) =>
            Try(p) {
              val typ = Try("type") {
                assertPure(t.isDefined, p, "The fields of node definitions must be typed").get
                types.get(t.get).get
              }

              check(
                assertPure(true /*!is[Mod.Case](rawStat.mods) || mods.isEmpty*/ , p, "The fields of case classes should have no modifiers"),
                assertPure(is[Mod.Case](rawStat.mods) || (mods.size == 1 && is[Mod.ValParam](mods)), p, "The fields of classes should have only `val` as a modifier"),
                typ,
              )

              name.value -> typ.get
            }
        }.get
      } else Nil
    }

    val origin = Try("origin") {
      if (isConcrete && paramss.size >= 2) {
        paramss.last match {
          case (p@Term.Param(mods, name, t, _)) :: Nil =>
            check(
              assertPure(is[Mod.Implicit](mods), p, "The origin parameter should be implicit"),
              assertPure(is[Mod.ValParam](mods), p, "The origin parameter should be a `val`"),
              assertPure(mods.size <= 2, p, "The origin parameter should have no extra modifiers"),
              assertPure(name.value == "o", name, "The origin parameter must be named `o`"),
              assertPure(t match { case Some(ScType.Name("Origin")) => true; case _ => false }, p, "The origin parameter must have type `Origin`"),
            )
          case other =>
            fail(other.headOrElse(rawStat.blame), "The origin parameter list should have exactly one parameter")
        }
      }
    }

    val blameType = Try("blame") {
      if (isConcrete && paramss.size == 3) {
        paramss(1) match {
          case (p@Term.Param(mods, name, t, _)) :: Nil =>
            val blameType = t.map { t =>
              Try("blameType") {
                t match {
                  case ScType.Apply(t"Blame", List(blameType)) =>
                    TypeAnalysis.getName(blameType).get
                  case other =>
                    fail(other, "The blame parameter should be of type `Blame[T]`, where T is a verification failure category")
                }
              }
            }

            check(
              assertPure(is[Mod.ValParam](mods), p, "The blame parameter should be a `val`"),
              assertPure(mods.size <= 1, p, "The blame parameter should have no modifiers"),
              assertPure(name.value == "blame", name, "The blame parameter must be named `blame`"),
              assertPure(blameType.isDefined, p, "The blame parameter must have a type"),
              blameType.getOrElse(Ok(()))
            )

            val simpleName = blameType.get.get
            val fqName =
              if(simpleName.parts.size == 1) Constants.BlamePackage + simpleName
              else simpleName

            Some(fqName)
          case other =>
            fail(other.headOrElse(rawStat.blame), "The blame parameter list should have exactly one parameter")
        }
      } else None
    }

    val template = Try("template") {
      val Template(early, inits, _, stats) = rawStat.templ
      val supports = inits.flatMap(_.tpe match {
        case blame @ ScType.Apply(t, List(t"G")) =>
          Try(blame) {
            types.getNode(TypeAnalysis.getName(t).get, blame).get._1
          } match {
            case Failures(_) => Nil
            case Ok(res) => Seq(res)
          }
        case _ => Nil
      })
      check(
        assertPure(early.isEmpty, early.head, "Node declarations must not have early initializers"),
        assertPure(true /*stats.isEmpty*/ , stats.head, s"Any declarations must go in ${rawStat.name.base}Impl"),
      )
      supports
    }

    check(modConditions, tparamConditions, publicConstructor, ctorConditions, fields, origin, blameType, template)

    val defn =
      if(isConcrete) Some(Defn(fields.get, blameType.get))
      else None

    Decl(rawStat.blame, rawStat.name, template.get, rawStat.isFamily, defn)
  }
}