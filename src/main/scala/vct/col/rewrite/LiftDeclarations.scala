package vct.col.rewrite

import vct.col.ast.stmt.decl.{ASTSpecial, DeclarationStatement, Method, ProgramUnit}
import hre.lang.System.{Abort, Debug, DebugException, Fail, Failure, Output, Warning}
import vct.col.ast.`type`.{ASTReserved, PrimitiveSort}
import vct.col.ast.expr.{MethodInvokation, NameExpression, OperatorExpression, StandardOperator}
import vct.col.ast.generic.ASTNode
import vct.col.util.SequenceUtils

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

class LiftDeclarations(arg: ProgramUnit) extends AbstractRewriter(arg) {
  var renameArguments: Boolean = false

  override def visit(decl: DeclarationStatement): Unit = {
    Debug("%s %s", decl.`type`, decl.name)
    val newType = SequenceUtils.optArrayCell(create, decl.`type`)
    val newStructType = SequenceUtils.arrayCell(create, decl.`type`)
    val initVal = decl.init match {
      case Some(x) => rewrite(x)
      case None => decl.`type`.zero
    }

    result = DeclarationStatement(
      decl.name,
      newType,
      Some(create.expression(StandardOperator.OptionSome, create.struct_value(newStructType, null, initVal)))
    )
    result.setOrigin(decl.getOrigin)
  }

  override def visit(invokation: MethodInvokation): Unit = {
    result = create.invokation(
      copy_rw.rewrite(invokation.`object`),
      rewrite(invokation.dispatch),
      invokation.method,
      rewrite(invokation.getArgs):_*
    )
  }

  override def visit(method: Method): Unit = {
    var body: ListBuffer[ASTNode] = ListBuffer()
    var args: ListBuffer[DeclarationStatement] = ListBuffer()

    for(arg <- method.getArgs) {
      args += create.field_decl(arg.getOrigin, "__arg_" + arg.name, arg.`type`)
      body += create.field_decl(
        arg.getOrigin,
        arg.name,
        SequenceUtils.optArrayCell(create, arg.`type`),
        create.expression(StandardOperator.OptionSome,
          create.struct_value(SequenceUtils.arrayCell(create, arg.`type`), null,
            create.unresolved_name("__arg_"+arg.name)))
      )
    }

    body += rewrite(method.getBody)

    for(arg <- method.getArgs) {
      body += create.special(arg.getOrigin, ASTSpecial.Kind.Exhale,
        create.expression(StandardOperator.Perm,
          SequenceUtils.access(create, create.unresolved_name(arg.name), constant(0)),
          create.reserved_name(ASTReserved.FullPerm)
        )
      )
    }

    renameArguments = true
    val newContract = rewrite(method.getContract)
    renameArguments = false

    result = create.method_decl(
      method.getReturnType,
      newContract,
      method.getName,
      args.asJava,
      create.block(body:_*)
    )
  }

  override def visit(name: NameExpression): Unit = {
    if(name.getKind == NameExpression.Kind.Argument) {
      if(renameArguments) {
        // Within contracts
        result = create.argument_name("__arg_" + name.getName)
      } else {
        // Otherwise, re-resolve the name to the masking argument
        result = SequenceUtils.access(create, create.unresolved_name(name.getName), create.constant(0))
      }
    } else if(name.getKind != NameExpression.Kind.Reserved) {
      result = SequenceUtils.access(create, name, create.constant(0))
    } else {
      super.visit(name)
    }
  }
}
