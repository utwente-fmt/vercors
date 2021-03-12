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

class Prog {
  val domains = new java.util.ArrayList[Domain]()
  val fields = new java.util.ArrayList[Field]()
  val functions = new java.util.ArrayList[Function]()
  val predicates = new java.util.ArrayList[Predicate]()
  val methods = new java.util.ArrayList[Method]()
}

