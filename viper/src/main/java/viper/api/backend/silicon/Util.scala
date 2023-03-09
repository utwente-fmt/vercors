package viper.api.backend.silicon

import vct.col.origin.Origin
import viper.api.transform.NodeInfo
import viper.silver.ast.{Infoed, Node}

case object Util {
  def getOrigin(node: Node): Option[Origin] =
    node match {
      case node: Infoed => node.info.getUniqueInfo[NodeInfo[vct.col.ast.Node[_]]].map(_.node.o)
      case _ => None
    }
}
