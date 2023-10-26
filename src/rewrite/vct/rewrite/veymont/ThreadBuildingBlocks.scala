package vct.rewrite.veymont

import vct.col.ast.{ClassDeclaration, InstanceField, JavaClass, Type, Communicate, Endpoint}
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewritten}

class ThreadBuildingBlocks[Pre <: Generation](
                                               val runMethod: ClassDeclaration[Pre],
                                               val methods: Seq[ClassDeclaration[Pre]],
                                               val channelFields: Map[(Communicate[Pre],Origin),InstanceField[Rewritten[Pre]]],
                                               val channelClasses: Map[Type[Pre],JavaClass[Rewritten[Pre]]],
                                               val thread: Endpoint[Pre],
                                               val threadField: InstanceField[Rewritten[Pre]]) {

}
