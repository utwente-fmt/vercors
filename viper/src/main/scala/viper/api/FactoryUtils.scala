package viper.api

import viper.silver.ast._
import scala.jdk.CollectionConverters._
import java.util.List
import hre.util.Triple

trait FactoryUtils[O] {
  def to_decls(o:O, map:List[Triple[O,String,Type]]) : Seq[LocalVarDecl] = {
    // This code looks silly, but the order must be preserved.
    map.asScala map {
      t => add(LocalVarDecl (t.v2, t.v3) _, t.v1)
    }
  }.toSeq

  def to_labels(o: O, labels: List[String]): Seq[Label] =
    labels.asScala.map(Label(_, Seq())(NoPosition, new OriginInfo(o))).toSeq
  
  def add[T](f : (Position, Info, ErrorTrafo) => T, o : O) =
    f(NoPosition, new OriginInfo(o), NoTrafos)
}
