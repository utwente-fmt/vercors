package viper.api

import viper.silver.ast._
import scala.jdk.CollectionConverters._
import scala.jdk.CollectionConverters._
import viper.silver.verifier.{Failure, Success, AbortedExceptionally, VerificationError}
import java.util.List
import java.util.Properties
import java.util.SortedMap
import scala.math.BigInt.int2bigInt
import viper.silver.ast.SeqAppend
import java.nio.file.Path
import viper.silver.parser.PLocalVarDecl
import scala.collection.mutable.WrappedArray

class SilverTypeFactory extends TypeFactory[Type] {
  
  override def Int() : Type = viper.silver.ast.Int
  override def Bool(): Type = viper.silver.ast.Bool
  override def Perm(): Type = viper.silver.ast.Perm
  override def Ref() : Type = viper.silver.ast.Ref
  override def List(t: Type): Type = SeqType(t)
  override def Bag(t: Type): Type = MultisetType(t)
  override def Set(t: Type): Type = SetType(t)
  override def domain_type(name:String,args:java.util.Map[String,Type]):Type = {
    val vars = args.keySet().asScala.toSeq map { x => TypeVar(x) }
    val tmp = args.asScala
    val pars:Map[viper.silver.ast.TypeVar,viper.silver.ast.Type] = 
     args.asScala.toMap map { case (k,v) => (TypeVar(k),v) }
    DomainType(name,pars)(vars)
  }
  override def type_var(name: String):Type = TypeVar(name)

}

