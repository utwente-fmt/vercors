package vct.col.veymont

import org.scalactic.Fail

object WellBehavednessIterative {

  def check(transitions : Map[LTSState,Set[LTSTransition]], LTSRole : String) : Unit = {
    val destStates : Set[LTSState] = transitions.values.flatMap(_.map(_.destState)).toSet
    val states : Set[LTSState] = transitions.keys.toSet ++ destStates
    val stateMap : Map[LTSState,Int] = states.zipWithIndex.toMap
    val transitionsInt : Map[Int,Set[(String,Int)]] = transitions.map(tr => (stateMap(tr._1) -> tr._2.map(t => (t.label.action.toString,stateMap(t.destState)))))
    val tauClosure = whileTauCl(transitionsInt,stateMap.values.toList)
    if(!checkForwardNonTau(transitionsInt,tauClosure))
      Fail("VeyMont Fail: Local LTS of %s not well-behaved (ForwardNonTau)",LTSRole)
    else if(!checkForwardTau(transitionsInt,tauClosure))
      Fail("VeyMont Fail: Local LTS of %s not well-behaved (ForwardTau)",LTSRole)
    else  if(!checkBackward(transitionsInt,tauClosure))
      Fail("VeyMont Fail: Local LTS of %s not well-behaved (Backward)",LTSRole)
    else { //LTS is well-behaved
    }
  }

  private def whileTauCl(transitions : Map[Int,Set[(String,Int)]], states : List[Int]) : Map[Int,Set[Int]] = {
    var todo = states
    var tauClosure : Map[Int, Set[Int]] = Map.empty
    while(todo.nonEmpty) {
      val i = todo.head
      if(!tauClosure.contains(i)) {
        val ks: Set[Int] = transitions.getOrElse(i, Set.empty).filter(tr => tr._1 == Tau.toString).map(_._2)
        if (ks.forall(tauClosure.contains(_))) {
          todo = todo.tail
          tauClosure = tauClosure + (i -> (ks.flatMap(k => tauClosure(k)) + i))
        } else todo = (ks.toList :+ i) ++ todo.tail
      } else todo = todo.tail
    }
    tauClosure
  }

  def checkForwardNonTau(transitions : Map[Int,Set[(String,Int)]], tauClosure : Map[Int, Set[Int]]) : Boolean = {
   transitions.forall{case (i,trs) =>
      // For all (i, label1, j1) and (i, tau^*, j2) ...
      trs.forall{case (label1, j1) =>
        tauClosure(i).forall(j2 => {
          // There exist (j1, tau^*, k1) and (j2, label2, k2) ...
          label1 == Tau.toString || tauClosure(j1).exists(k1 =>
            transitions(j2).exists{case (label2,k2) =>
              label2 == label1 && k1 == k2})
        })}};
  }

  def checkForwardTau(transitions : Map[Int,Set[(String,Int)]], tauClosure : Map[Int, Set[Int]]) : Boolean = {
    transitions.forall{case (i,trs) =>
      // For all (i, label1, j1) and (i, tau^*, j2) ...
      trs.forall{case (label1,j1) =>
        tauClosure(i).forall(j2 =>
          // There exist (j1, tau^*, k1) and (j2, tau^*, k2) ...
          label1 != Tau.toString || tauClosure(j1).exists( k1 =>
            tauClosure(j2).contains(k1)
        ))}}
  }

  def checkBackward(transitions : Map[Int,Set[(String,Int)]], tauClosure : Map[Int, Set[Int]]) : Boolean = {
    transitions.keys.forall(i =>
      // For all (i, tau*, j1) and (j1, label1, k1):
      tauClosure(i).forall(j1 =>
        transitions.getOrElse(j1,Set.empty).forall{ case (label1, k1) =>
          // There exist (i, label2, j2) and (j2, tau*, k2):
          label1 == BarrierWait.toString || transitions(i).exists{ case (label2, j2) =>
            tauClosure(j2).exists(k2 =>
              label1 == label2 && k1 == k2
        )}}))
  }

}
