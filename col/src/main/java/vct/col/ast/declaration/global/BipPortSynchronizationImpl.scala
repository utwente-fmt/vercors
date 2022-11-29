package vct.col.ast.declaration.global

import vct.col.ast.BipPortSynchronization

trait BipPortSynchronizationImpl[G] { this: BipPortSynchronization[G] =>
  def summarize: String = {
    val portsTxt = if (ports.isEmpty) "No ports" else ports.map("- " + _.decl.o.preferredName).mkString("\n")
    val wiresTxt = if (wires.isEmpty) "No wires" else wires.map("- " + _.o.preferredName).mkString("\n")

    s"""=== Port synchronization ===
       |Ports:
       |${portsTxt}
       |Wires:
       |${wiresTxt}""".stripMargin
  }
}
