package vct.col.ast.declaration.global

import vct.col.ast.BipSynchronization

trait BipSynchronizationImpl[G] { this: BipSynchronization[G] =>
  def summarize: String = {
    val portsTxt = if (ports.isEmpty) "No ports" else ports.map("- " + _.decl.o.preferredName).mkString("\n")
    val wiresTxt = if (wires.isEmpty) "No wires" else wires.map("- " + _.o.preferredName).mkString("\n")

    s"""=== Synchronization ===
       |Ports:
       |${portsTxt}
       |Wires:
       |${wiresTxt}""".stripMargin
  }
}
