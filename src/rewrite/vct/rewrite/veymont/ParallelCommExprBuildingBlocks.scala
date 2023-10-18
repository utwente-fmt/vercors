package vct.rewrite.veymont

import vct.col.ast.{Assign, Deref, InstanceField, JavaClass}
import vct.col.rewrite.{Generation, Rewritten}

class ParallelCommExprBuildingBlocks[Pre <: Generation](
    val channelField: InstanceField[Rewritten[Pre]],
    val channelClass: JavaClass[Rewritten[Pre]],
    val thisChanField: Deref[Rewritten[Pre]],
    val assign: Assign[Pre],
) {}
