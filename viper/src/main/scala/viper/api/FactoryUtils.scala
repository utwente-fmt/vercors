package viper.api

import viper.silver.ast._

import scala.collection.JavaConverters._
import scala.collection.JavaConverters._
import viper.silver.verifier.{AbortedExceptionally, Failure, Success, VerificationError}
import java.util.List
import java.util.Properties
import java.util.SortedMap

import scala.math.BigInt.int2bigInt
import viper.silver.ast.SeqAppend
import java.nio.file.Path

import hre.util.Triple
import viper.silver.parser.PLocalVarDecl

import scala.collection.mutable.WrappedArray

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
