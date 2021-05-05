package vct.col.veymont

import org.scalactic.Fail

object WellBehavedness {

  def check(transitions : Map[LTSState,Set[LTSTransition]], LTSRole : String) : Unit = {
    val tauClosure = getTauClosure(transitions)
    if(!checkForwardNonTau(transitions,tauClosure))
      Fail("VeyMont Fail: Local LTS of %s not well-behaved (ForwardNonTau)",LTSRole)
    else if(!checkForwardTau(transitions,tauClosure))
      Fail("VeyMont Fail: Local LTS of %s not well-behaved (ForwardTau)",LTSRole)
    else  if(!checkBackward(transitions,tauClosure))
      Fail("VeyMont Fail: Local LTS of %s not well-behaved (Backward)",LTSRole)
  }

  def getTauClosure(transitions : Map[LTSState,Set[LTSTransition]]) : Map[LTSState, Set[LTSState]] = {
    var tauClosure : Map[LTSState, Set[LTSState]] = Map();
    transitions.keySet.foreach(i => {
      var todo = Set(i)
      while (todo.nonEmpty) {
        val j = todo.head
        todo = todo - j
        if (!tauClosure.contains(j)) {
          val ks : Set[LTSState] = transitions.values.flatMap { tr => tr.filter(_.label.action == Tau).map(_.destState) }.toSet
          val ls : Set[LTSState] = ks.flatMap(k => tauClosure(k)) + j
          tauClosure = tauClosure + (j -> ls)
          todo = todo + j ++ ks
        }
      }
    })
    tauClosure
  }

  def checkForwardNonTau(transitions : Map[LTSState,Set[LTSTransition]], tauClosure : Map[LTSState, Set[LTSState]]) : Boolean = {
   transitions.forall{case (i,trs) =>
      // For all (i, label1, j1) and (i, tau^*, j2) ...
      trs.forall(tr1 => //(label1, j1)
        tauClosure(i).forall(j2 => {
          // There exist (j1, tau^*, k1) and (j2, label2, k2) ...
          tr1.label.action == Tau || tauClosure(tr1.destState).exists(k1 =>
            transitions(j2).exists(tr2 => //(label2,k2)
              tr2.label == tr1.label && k1 == tr2.destState))
        }))};
  }

  def checkForwardTau(transitions : Map[LTSState,Set[LTSTransition]], tauClosure : Map[LTSState, Set[LTSState]]) : Boolean = {
    transitions.forall{case (i,trs) =>
      // For all (i, label1, j1) and (i, tau^*, j2) ...
      trs.forall(tr1 => //(label1, j1)
        tauClosure(i).forall(j2 =>
          // There exist (j1, tau^*, k1) and (j2, tau^*, k2) ...
          tr1.label.action != Tau || tauClosure(tr1.destState).exists( k1 =>
            tauClosure(j2).contains(k1)
        )))}
  }

  def checkBackward(transitions : Map[LTSState,Set[LTSTransition]], tauClosure : Map[LTSState, Set[LTSState]]) : Boolean = {
    transitions.keys.forall(i =>
      // For all (i, tau*, j1) and (j1, label1, k1):
      tauClosure(i).forall(j1 =>
        transitions(j1).forall(tr1 =>// (label1, k1)
          // There exist (i, label2, j2) and (j2, tau*, k2):
          tr1.label.action == BarrierWait || transitions(i).exists(tr2 => //(label2, j2)
            tauClosure(tr2.destState).exists(k2 =>
              tr1.label == tr2.label && tr1.destState == k2
        )))))
  }

}
