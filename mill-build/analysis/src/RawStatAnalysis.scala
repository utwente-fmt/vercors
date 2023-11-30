package vct.col.ast.analysis

import vct.col.ast.structure._
import Util._
import scala.meta.{Name => ScName, Type => ScType, _}
import scala.reflect.ClassTag

object RawStatAnalysis {
  case class RawStat(name: Name, isTrait: Boolean, mods: Seq[Mod], tparams: Seq[ScType.Param], ctor: Ctor.Primary, templ: Template, blame: Tree) {
    def isFamily: Boolean =
      mods.exists {
        case Mod.Annot(Init(t, ScName.Anonymous(), Nil)) =>
          TypeAnalysis.getName(t).get == Constants.FamilyName.baseName
        case _ => false
      }
  }

  def getRawStats(stat: Stat): Result[Seq[RawStat]] = Try(stat) {
    stat match {
      case Defn.Object(_, objectName, Template(_, _, _, stats)) =>
        stats.map(getRawStats).get.flatten.map {
          case stat => stat.copy(name = objectName.value +: stat.name)
        }

      case Defn.Class(mods, name, tparams, ctor, templ) =>
        Seq(RawStat(
          Name(Seq(name.value)),
          isTrait = false,
          mods,
          tparams,
          ctor,
          templ,
          blame = name
        ))

      case Defn.Trait(mods, name, tparams, ctor, templ) =>
        Seq(RawStat(
          Name(Seq(name.value)),
          isTrait = true,
          mods,
          tparams,
          ctor,
          templ,
          blame = name,
        ))

      case _ => Nil
    }
  }


}