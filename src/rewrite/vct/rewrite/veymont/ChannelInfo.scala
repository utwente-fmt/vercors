package vct.rewrite.veymont

import vct.col.ast.{Type, Communicate}
import vct.col.rewrite.Generation

class ChannelInfo[Pre <: Generation](val comExpr: Communicate[Pre], val channelType: Type[Pre], val channelName: String) {

}
