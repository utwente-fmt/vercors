package vct.rewrite.veymont

import vct.col.ast.Type
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilderArg}

object ChannelClassGenerator extends RewriterBuilderArg[Type[_]] {
  override def key: String = "ChannelClassGenerator"

  override def desc: String = "Generate classes for channels of a VeyMont seq_program"

}

case class ChannelClassGenerator [Pre <: Generation](channelType: Type[_]) extends Rewriter[Pre] {

  override def dispatch(t: Type[Pre]) : Type[Post] = {
    if(t.o.preferredName == "MessageType")
      dispatch(channelType.asInstanceOf[Type[Pre]])
    else rewriteDefault(t)
  }

}
