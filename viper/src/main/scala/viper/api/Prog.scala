package viper.api

import viper.silver.ast._

class Prog {
  val domains = new java.util.ArrayList[Domain]()
  val fields = new java.util.ArrayList[Field]()
  val functions = new java.util.ArrayList[Function]()
  val predicates = new java.util.ArrayList[Predicate]()
  val methods = new java.util.ArrayList[Method]()
}

