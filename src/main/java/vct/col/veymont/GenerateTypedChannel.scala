package vct.col.veymont

import geny.Generator.from
import hre.lang.System.Failure
import vct.col.ast.`type`.{ASTReserved, ClassType, PrimitiveSort, PrimitiveType, Type}
import vct.col.ast.expr.{NameExpression, NameExpressionKind, OperatorExpression, StandardOperator}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.LoopStatement
import vct.col.ast.stmt.decl.{ASTClass, Method, ProgramUnit, VariableDeclaration}
import vct.col.ast.util.{ASTUtils, AbstractRewriter, ContractBuilder}
import vct.col.veymont.GenerateTypedChannel.getPrimitiveTypeName
import vct.col.veymont.Util.{chanReadMethodName, chanValueFieldName, chanWriteMethodName, channelClassName, getBlockOrThrow}

import scala.jdk.CollectionConverters._

object GenerateTypedChannel {

  def getTypeName(t : Type) : String = t match {
    case p: PrimitiveType => getPrimitiveTypeName(p)
    case cl : ClassType  => cl.getName
  }

  private def getPrimitiveTypeName(t : ASTNode) : String = t match {
    case p : PrimitiveType => p.sort match {
      case PrimitiveSort.Option => getPrimitiveTypeName(p.args.head)
      case PrimitiveSort.Array => getPrimitiveTypeName(p.args.head) + "Array"
      case PrimitiveSort.Cell => getPrimitiveTypeName(p.args.head)
      case PrimitiveSort.Sequence => getPrimitiveTypeName(p.args.head) + "Sequence"
      case PrimitiveSort.Map => getPrimitiveTypeName(p.args.head) + "Map"
      case _ => p.sort.toString
    }
    case cl : ClassType => cl.getName
    case _ => throw Failure("VeyMont Fail: ASTNode %s is not a type", t.toString)
  }

}
class GenerateTypedChannel(override val source: ProgramUnit, val sort : Either[PrimitiveType,ASTClass]) extends AbstractRewriter(null, true) {

  override def visit(t : PrimitiveType) : Unit = {
    if(t.sort == PrimitiveSort.Integer) {
      sort match {
        case Left(s) => result = createNewType(s)
        case Right(c) => result = create.class_type(c.getName)
      }
    } else super.visit(t)
  }

  private def createNewType(t : ASTNode) : Type = t match {
    case p : PrimitiveType =>
      if(Set(PrimitiveSort.Option, PrimitiveSort.Array, PrimitiveSort.Cell, PrimitiveSort.Sequence).contains(p.sort))
        create.primitive_type(p.sort,createNewType(p.args.head))
      else if(p.sort == PrimitiveSort.Map)
        create.primitive_type(p.sort,createNewType(p.args.head),createNewType(p.args.tail.head))
      else create.primitive_type(p.sort)
    case cl : ClassType => create.class_type(cl.getName)
    case _ => throw Failure("VeyMont Fail: ASTNode %s is not a type", t.toString)
  }

  override def visit(v : VariableDeclaration) : Unit = {
    val varType = sort match {
      case Left(s) => s
      case Right(c) => create.class_type(c.name)
    }
    result = create.field_decl(v.get().asScala.head.name,varType,create.field_name(chanValueFieldName))
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
          visitConstructor(m,cl)
        } else if(m.kind == Method.Kind.Predicate || m.kind == Method.Kind.Pure) {
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
        } else Fail("VeyMont Fail: unexpected method %s in channel class %s!",m.name,cl.name)
      }
    }
  }

  private def visitConstructor( m : Method, cl : ASTClass) =
    cl.methods().asScala.find(_.kind == Method.Kind.Constructor) match {
      case None => Fail("VeyMont Fail: Cannot find constructor of class %s!",cl.name)
      case Some(constr) => {
        if(!ASTUtils.conjuncts(constr.getContract.post_condition, StandardOperator.Star).asScala.filter {
          case op: OperatorExpression => op.operator == StandardOperator.Perm && (op.arg(0) match {
            case n: NameExpression => cl.fields().asScala.map(_.name).contains(n.name)
            case _ => false
          })
          case _ => false
        }.forall(_.asInstanceOf[OperatorExpression].arg(1) match {
          case r : NameExpression => r.kind == NameExpressionKind.Reserved && r.reserved == ASTReserved.ReadPerm
          case _ => false
        })) {
          Fail("VeyMont Fail: the constructor of class %s must ensure read permission to its fields!",cl.name)
        }
        val initValueField = create.invokation(null,create.class_type(cl.name),Method.JavaConstructor,getDummyArgs(constr,cl):_*)
        val initAssign = create.assignment(create.field_name(chanValueFieldName),initValueField)
        result = create.method_kind(m.kind, m.getReturnType, rewrite(m.getContract),
          getTypeName, m.getArgs, create.block((rewrite(
            getBlockOrThrow(m.getBody,"VeyMont Fail: BlockStatement expected!")
              .getStatements) :+ initAssign):_*))

      }
    }

  private def getDummyArgs(constr : Method, cl : ASTClass) : Array[ASTNode] = constr.getArgs.map(_.`type` match {
    case p : PrimitiveType => p.sort match {
      case PrimitiveSort.Boolean => create.constant(true)
      case PrimitiveSort.Integer => create.constant(0)
      case PrimitiveSort.Double => val d : Double = 0.1; create.constant(d)
      case _ => throw Failure("VeyMont Fail: Could not generate channel of type %s",cl.name)
    }
  })

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
    val res = create.ast_class(getTypeName,c.kind,c.parameters,c.super_classes,c.implemented_classes)
    for (item <- c.asScala) {
      res.add(rewrite(item))
    }
    result = res
  }

  private def getTypeName : String = (sort match {
    case Left(s) => getPrimitiveTypeName(s)
    case Right(cl) => cl.getName
  }) + channelClassName

  private def getFieldPerms(astClass : ASTClass, varNode : ASTNode) : OperatorExpression =
    astClass.fields().asScala.map(f =>
      create.expression(StandardOperator.Perm,create.dereference(varNode,f.name),create.reserved_name(ASTReserved.ReadPerm)))
      .reduce((p1,p2) => create.expression(StandardOperator.Star,p1,p2))
}
