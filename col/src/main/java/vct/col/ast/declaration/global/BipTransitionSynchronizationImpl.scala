package vct.col.ast.declaration.global

import vct.col.ast.{BipPortSynchronization, BipTransitionSynchronization}

trait BipTransitionSynchronizationImpl[G] { this: BipTransitionSynchronization[G] =>
  def summarize: String = {
    val portsTxt = if (transitions.isEmpty) "No transitions" else transitions.map("- " + _.decl.signature.shortSignature).mkString("\n")
    val wiresTxt = if (wires.isEmpty) "No wires" else wires.map("- " + _.o.preferredName).mkString("\n")

    s"""=== Transition synchronization ===
       |Transition:
       |${portsTxt}
       |Wires:
       |${wiresTxt}""".stripMargin
  }
}
