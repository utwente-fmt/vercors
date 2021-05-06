package vct.col.veymont

import hre.lang.System.Output
import vct.col.ast.`type`.{ASTReserved, ClassType, PrimitiveSort, PrimitiveType}
import vct.col.ast.expr.{OperatorExpression, StandardOperator}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.{BlockStatement, LoopStatement}
import vct.col.ast.stmt.decl.{ASTClass, Method, ProgramUnit, VariableDeclaration}
import vct.col.ast.util.{AbstractRewriter, ContractBuilder}
import vct.col.veymont.Util.{chanReadMethodName, chanValueFieldName, chanWriteMethodName, channelClassName}

import scala.collection.convert.ImplicitConversions.`iterable AsScalaIterable`

class GenerateTypedChannel(override val source: ProgramUnit, val sort : Either[PrimitiveType,ASTClass]) extends AbstractRewriter(null, true) {

  override def visit(t : PrimitiveType) : Unit = {
    if(t.sort == PrimitiveSort.Integer) {
      sort match {
        case Left(s) => result = create.primitive_type(s.sort)
        case Right(c) => result = create.class_type(c.getName)
      }
    } else super.visit(t)
  }

  override def visit(v : VariableDeclaration) : Unit = {
    val varType = sort match {
      case Left(s) => s
      case Right(c) => create.class_type(c.name)
    }
    result = create.field_decl(v.get().head.name,varType,create.field_name(chanValueFieldName))
  }

  override def visit(m : Method) : Unit = {
    sort match {
      case Left(_) => {
        if(m.kind == Method.Kind.Constructor)
          result = create.method_kind(m.kind, m.getReturnType, rewrite(m.getContract), getTypeName, m.getArgs, rewrite(m.getBody))
        else
          super.visit(m)
      }
      case Right(cl) => {
        if(m.kind == Method.Kind.Constructor) {
          cl.methods().find(_.kind == Method.Kind.Constructor) match {
            case None => Fail("VeyMont Fail: Cannot find constructor of class %s!",cl.name)
            case Some(constr) => {
              val dummyArgs : Array[ASTNode] = constr.getArgs.map(_.`type` match {
                case p : PrimitiveType => p.sort match {
                  case PrimitiveSort.Boolean => create.constant(true)
                  case PrimitiveSort.Integer => create.constant(0)
                  case PrimitiveSort.Double => val d : Double = 0.1; create.constant(d)
                  case _ => Fail("VeyMont Fail: Could not generate channel of type %s",cl.name); create.constant(true)
                }
              })
              val initValueField = create.invokation(null,create.class_type(cl.name),Method.JavaConstructor,dummyArgs:_*)
              val initAssign = create.assignment(create.field_name(chanValueFieldName),initValueField)
              m.getBody match {
                case b : BlockStatement => result = create.method_kind(m.kind, m.getReturnType, rewrite(m.getContract),
                  getTypeName, m.getArgs, create.block((rewrite(b.getStatements) :+ initAssign):_*))
                case _ => Fail("VeyMont Fail: BlockStatement expected!")
              }
            }
          }
        } else if(m.kind == Method.Kind.Predicate) {
          m.getBody match {
            case o : OperatorExpression =>
              result = create.method_kind(m.kind, m.getReturnType, rewrite(m.getContract), m.name, m.getArgs,
                create.expression(StandardOperator.Star,rewrite(o),getFieldPerms(cl,create.field_name(chanValueFieldName))))
            case _ => Fail("VeyMont Fail: OperatorExpression expected! %s", m.getBody)
          }
        } else if(m.name == chanWriteMethodName) {
          val cb = new ContractBuilder()
          rewrite(m.getContract,cb)
          val newArgs = m.getArgs.map(rewrite(_))
          cb.context(getFieldPerms(cl,create.argument_name(newArgs.head.name)))
          result = create.method_kind(m.kind, rewrite(m.getReturnType), cb.getContract, m.name, newArgs, rewrite(m.getBody))
        } else if(m.name == chanReadMethodName) {
          val cb = new ContractBuilder()
          rewrite(m.getContract,cb)
          cb.ensures(getFieldPerms(cl,create.reserved_name(ASTReserved.Result)))
          val newArgs = m.getArgs.map(rewrite(_))
          val res = create.method_kind(m.kind, rewrite(m.getReturnType), cb.getContract, m.name, newArgs, rewrite(m.getBody))
          result = res
        }
      }
    }
  }

  override def visit(l : LoopStatement) : Unit = sort match {
    case Left(p) => super.visit(l)
    case Right(cl) => {
      val cb = new ContractBuilder()
      rewrite(l.getContract,cb)
      cb.appendInvariant(getFieldPerms(cl,create.field_name(chanValueFieldName)))
      val res = create.while_loop(rewrite(l.getEntryGuard),rewrite(l.getBody),cb.getContract)
      result = res
    }
  }

  override def visit(c : ASTClass) : Unit = {
    val res = create.new_class(getTypeName, null, null)
    for (item <- c) {
      res.add(rewrite(item))
    }
    result = res
  }

  private def getTypeName : String = (sort match {
    case Left(s) => s.sort.toString
    case Right(cl) => cl.getName
  }) + channelClassName

  private def getFieldPerms(astClass : ASTClass, varNode : ASTNode) : OperatorExpression =
    astClass.fields().map(f =>
      create.expression(StandardOperator.Perm,create.dereference(varNode,f.name),create.reserved_name(ASTReserved.ReadPerm)))
      .reduce((p1,p2) => create.expression(StandardOperator.Star,p1,p2))
}
