package vct.col.ast.lang.llvm

import vct.col.ast.LLVMExtractValue
import vct.col.ast.ops.LLVMExtractValueOps
import vct.col.print._

trait LLVMExtractValueImpl[G] extends LLVMExtractValueOps[G] {
  this: LLVMExtractValue[G] =>

  override def layout(implicit ctx: Ctx): Doc =
    Text("extractValue<") <+> aggregateType <> ">" <+> value.show <> "[" <+>
      Doc.args(indices.map(i => Text(i.toString))) <+> "]"
  override def t = resultType
}
