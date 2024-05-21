package vct.rewrite.veymont

import vct.col.ast.{Type, CommunicateX}
import vct.col.rewrite.Generation

class ChannelInfo[Pre <: Generation](val comExpr: CommunicateX[Pre], val channelType: Type[Pre], val channelName: String) {

}
