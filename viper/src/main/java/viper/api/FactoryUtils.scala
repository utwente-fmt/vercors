package viper.api

import hre.util.Triple
import viper.silver.ast._

import java.util.List
import scala.collection.JavaConverters._

trait FactoryUtils[O] {
  def to_decls(o:O, map:List[Triple[O,String,Type]]) : Seq[LocalVarDecl] = {
    // This code looks silly, but the order must be preserved.
    map.asScala map {
      t => add(LocalVarDecl (t.v2, t.v3) _, t.v1)
    }
  }

  def to_labels(o: O, labels: List[String]): Seq[Label] =
    labels.asScala.map(Label(_, Seq())(NoPosition, new OriginInfo(o)))
  
  def add[T](f : (Position, Info, ErrorTrafo) => T, o : O) =
    f(NoPosition, new OriginInfo(o), NoTrafos)
}
