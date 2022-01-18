package vct.col.veymont

import vct.col.ast.`type`.{ClassType, PrimitiveSort, PrimitiveType}
import vct.col.ast.expr.StandardOperator
import vct.col.ast.stmt.decl.{ASTClass, Method, ProgramUnit}
import vct.col.ast.util.{AbstractRewriter, ContractBuilder}

class GlobalProgPerms(override val source: ProgramUnit) extends AbstractRewriter(null, true) {


//  override def visit(c : ASTClass) : Unit = {
//
//
////    c.fields().forEach(f => {
////      val p = create.expression(StandardOperator.Perm, create.field_name(f.name), create.fullPermission())
////      //Warning(p.toString)
////    })
////
////    c.add_dynamic(create.field_decl("foo", create.primitive_type(PrimitiveSort.Integer)))
////    val x = create.predicate(
////      "all_permissions",
////      create.expression(StandardOperator.Perm, create.field_name("foo"), create.fullPermission()))
////    c.add(x)
//
////    result = c
//    super.visit(c)
//  }
//
//  override def visit(m : Method) : Unit = {
////    Warning("GlobalProgPerms not implemented!")
//
//    val c = m.getContract()
//    val cb = new ContractBuilder()
//
//
//
//    //Warning(m.getType.toString)
//
////    val inv = create.invokation();
//
////    Warning("\n" + m.getContract.pre_condition.toString)
////    m.getContract.pre_condition.
//
////    m.setContract(...);
//
//    super.visit(m)
//  }

}
