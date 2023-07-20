package vct.rewrite.veymont

import vct.col.ast.{Type, VeyMontCommExpression}
import vct.col.rewrite.Generation

class ChannelInfo[Pre <: Generation](val comExpr: VeyMontCommExpression[Pre], val channelType: Type[Pre], val channelName: String) {

}
