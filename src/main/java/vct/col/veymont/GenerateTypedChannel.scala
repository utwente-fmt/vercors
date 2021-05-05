package vct.col.veymont

import hre.lang.System.Output
import vct.col.ast.`type`.{ClassType, PrimitiveSort, PrimitiveType}
import vct.col.ast.stmt.decl.{ASTClass, Method, ProgramUnit, VariableDeclaration}
import vct.col.ast.util.AbstractRewriter
import vct.col.veymont.Util.channelClassName

import scala.collection.convert.ImplicitConversions.`iterable AsScalaIterable`

class GenerateTypedChannel(override val source: ProgramUnit, val sort : Either[PrimitiveType,ClassType]) extends AbstractRewriter(null, true) {

  private def getTypeName : String = (sort match {
    case Left(s) => s.sort.toString
    case Right(cl) => cl.getName
  }) + channelClassName

  override def visit(t : PrimitiveType) : Unit = {
    if(t.sort == PrimitiveSort.Integer) {
      sort match {
        case Left(s) => result = create.primitive_type(s.sort)
        case Right(c) => result = create.class_type(c.getName)
      }
    } else super.visit(t)
  }

  override def visit(v : VariableDeclaration) : Unit = {
    val varDecl = create.variable_decl(sort match {
      case Left(s) => s
      case Right(c) => c
    })
    varDecl.add(v.get().head)
    result = varDecl
  }

  override def visit(m : Method) : Unit = {
    if(m.kind == Method.Kind.Constructor) {
      result = create.method_kind(m.kind, m.getReturnType, rewrite(m.getContract), getTypeName, m.getArgs, rewrite(m.getBody))
    } else
      super.visit(m)
  }

  override def visit(c : ASTClass) : Unit = {
    val res = create.new_class(getTypeName, null, null)
    for (item <- c) {
      res.add(rewrite(item))
    }
    result = res
  }
}
