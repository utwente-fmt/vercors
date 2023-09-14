package vct.col.ast.declaration.global

import vct.col.ast.BipTransitionSynchronization
import vct.col.print.{Ctx, Doc, Text}

trait BipTransitionSynchronizationImpl[G] { this: BipTransitionSynchronization[G] =>
  def summarize: String = {
    val portsTxt = if (transitions.isEmpty) "No transitions" else transitions.map("- " + _.decl.signature.shortSignature).mkString("\n")
    val wiresTxt = if (wires.isEmpty) "No wires" else wires.map("- " + _.o.getPreferredName.get.preferredName).mkString("\n")

    s"""=== Transition synchronization ===
       |Transition:
       |${portsTxt}
       |Wires:
       |${wiresTxt}""".stripMargin
  }

  override def layout(implicit ctx: Ctx): Doc =
    Text("/*") <+/>
      Text("javaBipTransitionSynchronization {") <>> {
      Text("transitions:") <>> Doc.stack(transitions.map(ctx.name).map(Text("-") <+> _)) <+/>
        Text("wires:") <>> Doc.stack(wires.map(wire => Text(ctx.name(wire.dataOut)) <+> "->" <+> ctx.name(wire.dataIn)))
    } <+/> "*/"
}
