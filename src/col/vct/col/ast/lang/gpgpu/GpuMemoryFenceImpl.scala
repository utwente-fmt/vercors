package vct.col.ast.lang.gpgpu

import vct.col.ast.GpuMemoryFence
import vct.col.ast.ops.GpuMemoryFenceFamilyOps

trait GpuMemoryFenceImpl[G] extends GpuMemoryFenceFamilyOps[G] { this: GpuMemoryFence[G] =>

}