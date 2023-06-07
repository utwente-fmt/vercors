package vct.rewrite.veymont

import vct.col.ast.{ClassDeclaration, InstanceField, JavaClass, Statement, Type, VeyMontThread}
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewritten}

class ThreadBuildingBlocks[Pre <: Generation](
                                               val runMethod: ClassDeclaration[Pre],
                                               val methods: Seq[ClassDeclaration[Pre]],
                                               val channelFields: Map[Ref[Pre,VeyMontThread[Pre]],InstanceField[Rewritten[Pre]]],
                                               val channelClasses: Map[Type[Pre],JavaClass[Rewritten[Pre]]],
                                               val thread: VeyMontThread[Pre],
                                               val threadField: InstanceField[Rewritten[Pre]]) {

}
