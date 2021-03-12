package vct.col.rewrite

import vct.col.ast.`type`.ASTReserved
import vct.col.ast.expr.{MethodInvokation, NameExpression, NameExpressionKind, StandardOperator}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.BlockStatement
import vct.col.ast.stmt.decl.{ASTClass, ASTSpecial, DeclarationStatement, Method, ProgramUnit}
import vct.col.ast.stmt.terminal.ReturnStatement
import vct.col.ast.util.{AbstractRewriter, SequenceUtils}

import scala.jdk.CollectionConverters._
import scala.collection.mutable.ListBuffer

class LiftDeclarations(arg: ProgramUnit) extends AbstractRewriter(arg) {
  var inContract: Boolean = false

  override def visit(decl: DeclarationStatement): Unit = {
    if(inContract) {
      // More accurately we should skip declarations in binding expressions
      super.visit(decl)
    } else {
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

    /* TODO: ugly hack to stop the feature system from complaining about a return statement in a weird place. Instead
      the arguments should be exhaled before every return point. */

    val lastReturn = method.getBody.asInstanceOf[BlockStatement].getStatements.lastOption match {
      case Some(ret: ReturnStatement) =>
        body ++= method.getBody.asInstanceOf[BlockStatement].getStatements.init.map(rewrite(_))
        Some(ret)
      case _ =>
        body ++= method.getBody.asInstanceOf[BlockStatement].getStatements.map(rewrite(_))
        None
    }

    for(arg <- method.getArgs) {
      body += create.special(arg.getOrigin, ASTSpecial.Kind.Exhale,
        create.expression(StandardOperator.Perm,
          SequenceUtils.access(create, create.unresolved_name(arg.name), constant(0)),
          create.reserved_name(ASTReserved.FullPerm)
        )
      )
    }

    if(lastReturn.nonEmpty) {
      body += rewrite(lastReturn.get)
    }

    inContract = true
    val newContract = rewrite(method.getContract)
    inContract = false

    result = create.method_decl(
      method.getReturnType,
      newContract,
      method.getName,
      args.asJava,
      create.block((body.toSeq):_*)
    )
  }

  override def visit(name: NameExpression): Unit = {
    if(name.getKind == NameExpressionKind.Argument) {
      if(inContract) {
        // Within contracts
        result = create.argument_name("__arg_" + name.getName)
      } else {
        // Otherwise, re-resolve the name to the masking argument
        result = SequenceUtils.access(create, create.unresolved_name(name.getName), create.constant(0))
      }
    } else if(!inContract && name.getKind != NameExpressionKind.Reserved && name.getKind != NameExpressionKind.Label) {
      result = SequenceUtils.access(create, name, create.constant(0))
    } else {
      super.visit(name)
    }
  }

  override def visit(cls: ASTClass): Unit = {
    val res = create.ast_class(
      cls.name, cls.kind, rewrite(cls.parameters),
      rewrite(cls.super_classes), rewrite(cls.implemented_classes))

    res.setContract(rewrite(cls.getContract))

    cls.asScala.foreach {
      case field: DeclarationStatement =>
        res.add(copy_rw.rewrite(field))
      case other =>
        res.add(rewrite(other))
    }

    result = res
  }
}
