package viper.api.backend.silicon

import hre.progress.ProgressRender
import vct.col.origin.{InlineContext, Origin}
import viper.api.transform.NodeInfo
import viper.silver.ast.{Infoed, Node}

case object Util {
  def getOrigin(node: Node): Option[Origin] =
    node match {
      case node: Infoed => node.info.getUniqueInfo[NodeInfo[vct.col.ast.Node[_]]].map(_.node.o)
      case _ => None
    }

  def renderOrigin(node: Node, message: String, short: Boolean): ProgressRender = {
    val o = getOrigin(node)

    o match {
      case None => ProgressRender(message)
      case Some(o) if short =>
        ProgressRender(s"$message `${o.getInlineContext.getOrElse(InlineContext("unknown")).inlineContext}`")
      case Some(o) =>
        val lines = o.messageInContext(message).split("\n")
        ProgressRender(lines, lines.size - 2)
    }
  }
}
