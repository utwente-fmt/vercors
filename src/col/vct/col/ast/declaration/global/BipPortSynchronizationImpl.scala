package vct.col.ast.declaration.global

import vct.col.ast.BipPortSynchronization
import vct.col.print._

trait BipPortSynchronizationImpl[G] { this: BipPortSynchronization[G] =>
  def summarize: String = {
    val portsTxt = if (ports.isEmpty) "No ports" else ports.map("- " + _.decl.o.getPreferredName.get.preferredName).mkString("\n")
    val wiresTxt = if (wires.isEmpty) "No wires" else wires.map("- " + _.o.getPreferredName.get.preferredName).mkString("\n")

    s"""=== Port synchronization ===
       |Ports:
       |${portsTxt}
       |Wires:
       |${wiresTxt}""".stripMargin
  }

  override def layout(implicit ctx: Ctx): Doc =
    Text("/*") <+/>
    Text("javaBipPortSynchronization {") <>> {
      Text("ports:") <>> Doc.stack(ports.map(ctx.name).map(Text("-") <+> _)) <+/>
        Text("wires:") <>> Doc.stack(wires.map(wire => Text(ctx.name(wire.dataOut)) <+> "->" <+> ctx.name(wire.dataIn)))
    } <+/> "*/"
}
