package vct.rewrite.veymont

import vct.col.ast.{ClassDeclaration, InstanceField, Statement, VeyMontThread}
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewritten}

class ThreadBuildingBlocks[Pre <: Generation](
                                               val runMethod: ClassDeclaration[Pre],
                                               val methods: Seq[ClassDeclaration[Pre]],
                                               val threadFieldMap: Map[VeyMontThread[Pre],InstanceField[Rewritten[Pre]]],
                                               val channelFields: Map[Ref[Pre,VeyMontThread[Pre]],InstanceField[Rewritten[Pre]]]) {

}
