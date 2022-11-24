package viper.api.backend.silicon

import viper.silicon
import viper.silicon.Config
import viper.silicon.common.config
import viper.silicon.interfaces.decider.{Prover, Result, Unsat}
import viper.silicon.state.{DefaultIdentifierFactory, Identifier, SuffixedIdentifier, terms}
import viper.silicon.state.terms.{Decl, Fun, Sort, Term}
import viper.silver.verifier.{Model, ModelEntry}

case class AgreeableProver(inner: Prover) extends Prover {
  override def assert(goal: Term, timeout: Option[Int]): Boolean = true // unsat
  override def check(timeout: Option[Int]): Result = Unsat

  override def statistics(): silicon.Map[String, String] = silicon.Map.empty

  override def hasModel(): Boolean = false
  override def isModelValid(): Boolean = false
  override def getModel(): Model = Model(Map.empty[String, ModelEntry])
  override def clearLastModel(): Unit = {}

  override def name: String = inner.name
  override def minVersion: config.Version = inner.minVersion
  override def maxVersion: Option[config.Version] = inner.maxVersion
  override def version(): config.Version = inner.version()

  override def staticPreamble: String = inner.staticPreamble
  override def randomizeSeedsOptions: Seq[String] = inner.randomizeSeedsOptions

  override def pushPopScopeDepth: Int = -42
  override def push(n: Int, timeout: Option[Int]): Unit = {}
  override def pop(n: Int): Unit = {}

  override def reset(): Unit = {}
  override def emit(content: String): Unit = {}
  override def emitSettings(contents: Iterable[String]): Unit = {}
  override def assume(term: Term): Unit = {}
  override def declare(decl: Decl): Unit = {}
  override def comment(content: String): Unit = {}
  override def saturate(timeout: Int, comment: String): Unit = {}
  override def saturate(data: Option[Config.ProverStateSaturationTimeout]): Unit = {}
  override def start(): Unit = inner.start()
  override def stop(): Unit = inner.stop()

  override def fresh(id: String, argSorts: Seq[Sort], resultSort: Sort): terms.Function =
    inner.fresh(id, argSorts, resultSort)
}
