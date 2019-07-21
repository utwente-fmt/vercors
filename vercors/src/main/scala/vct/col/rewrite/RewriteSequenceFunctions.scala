package vct.col.rewrite

import hre.ast.MessageOrigin
import vct.col.ast.`type`.{ASTReserved, PrimitiveSort, PrimitiveType, Type}
import vct.col.ast.expr.{Dereference, OperatorExpression, StandardOperator}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.decl.{DeclarationStatement, ProgramUnit}
import vct.col.ast.util.ContractBuilder

import scala.collection.mutable

object RewriteSequenceFunctions {
  val getRemoveName: mutable.Map[Type, String] = mutable.Map()
  val getRangeName: mutable.Map[Type, String] = mutable.Map()

  val namesUsed: mutable.Set[String] = mutable.Set()

  def getUniqueName(str: String): String = {
    var result = str.replaceAll("[^a-zA-Z0-9$_']", "_")
    while (namesUsed contains str) {
      result += "$"
    }
    namesUsed += str
    str
  }

  def getRemoveFunction(t: Type): String = {
    getRemoveName getOrElseUpdate(t, getUniqueName("remove_by_index_" + t.toString))
  }

  def getRangeFunction(t: Type): String = {
    getRangeName getOrElseUpdate(t, getUniqueName("take_range_" + t.toString))
  }



}

class RewriteSequenceFunctions(source: ProgramUnit) extends AbstractRewriter(source) {
  override def rewriteAll(): ProgramUnit = {
    val res = super.rewriteAll()

    create.enter()
    create.setOrigin(new MessageOrigin("Sequence Functions"))
    for ((t, name) <- RewriteSequenceFunctions.getRemoveName) {
      res.add(removeFromSequenceByIndex(t, name))
    }
    for ((t, name) <- RewriteSequenceFunctions.getRangeName) {
      res.add(takeRangeFromSequence(t, name))
    }
    create.leave()

    res
  }

  override def visit(operator: OperatorExpression): Unit = {
    operator.operator match {
      case StandardOperator.Remove =>
        val sequenceType = operator.arg(0).getType
        result = create.invokation(null, null, RewriteSequenceFunctions.getRemoveFunction(sequenceType), rewrite(operator.args.toArray):_*)
      case StandardOperator.RangeFromSeq =>
        val sequenceType = operator.arg(0).getType
        result = create.invokation(null, null, RewriteSequenceFunctions.getRangeFunction(sequenceType), rewrite(operator.args.toArray):_*)
      case _ =>
        super.visit(operator)
    }
  }




  def takeRangeFromSequence(sequenceType: Type, methodName: String): ASTNode = {
    val contract = new ContractBuilder
    val result = create.reserved_name(ASTReserved.Result)


    val sequenceArgName = "seq0"
    val startArgName = "start"
    val stopArgName = "stop"

    val sequenceArg = new DeclarationStatement(sequenceArgName, sequenceType)
    val startArg = new DeclarationStatement(startArgName, create.primitive_type(PrimitiveSort.Integer))
    val stopArg = new DeclarationStatement(stopArgName, create.primitive_type(PrimitiveSort.Integer))

    contract.requires(validIndex(sequenceArgName, startArgName))
    contract.requires(and(lte(create.constant(0), name(stopArgName)), lte(name(stopArgName), size(name(sequenceArgName)))))
    contract.requires(lte(name(startArgName), name(stopArgName)))

    contract.ensures(eq(size(result), minus(name(stopArgName), name(startArgName))))


    val forAllIndex = new DeclarationStatement("j0", create.primitive_type(PrimitiveSort.Integer))
    val indexNode = name(forAllIndex.name)


    contract.ensures(
      create.starall(
        and(lte(name(startArgName), indexNode), less(indexNode, name(stopArgName))),
        eq(
          get(result, minus(indexNode, name(startArgName))),
          get(name(sequenceArgName), indexNode)),
        forAllIndex
      )
    )

    contract.ensures(
      create.starall(
        and(lte(create.constant(0), indexNode), less(indexNode, size(result))),
        eq(
          get(result, indexNode),
          get(name(sequenceArgName), plus(indexNode, name(startArgName)))),
        forAllIndex
      )
    )

    val methodArguments = List(sequenceArg, startArg, stopArg)
    val declaration = create.method_decl(sequenceType, contract.getContract, methodName, methodArguments.toArray, null)
    declaration.setStatic(true)
    declaration

  }

  def removeFromSequenceByIndex(sequenceType: Type, methodName: String): ASTNode = {
    val contract = new ContractBuilder
    val result = create.reserved_name(ASTReserved.Result)

    val sequenceArgumentName = "seq0"
    val indexArgumentName = "i0"

    val sequenceArgument = new DeclarationStatement(sequenceArgumentName, sequenceType)
    val indexArgument = new DeclarationStatement(indexArgumentName, create.primitive_type(PrimitiveSort.Integer))

    contract.requires(validIndex(sequenceArgumentName, indexArgumentName))
    contract.ensures(
      eq(
        size(result),
        minus(
          size(name(sequenceArgumentName)),
          create.constant(1)
        )
      )
    )

    val forAllIndex = new DeclarationStatement("j0", create.primitive_type(PrimitiveSort.Integer))
    val indexNode = name("j0")

    contract.ensures(
      create.starall(
        and(lte(constant(0), indexNode), less(indexNode, name(indexArgumentName))),
        eq(
          get(result, indexNode),
          get(name(sequenceArgumentName), indexNode)),
        forAllIndex
      )
    )

    contract.ensures(
      create.starall(
        and(lte(name(indexArgumentName), indexNode), less(indexNode, size(result))),
        eq(
          get(result, indexNode),
          get(name(sequenceArgumentName), plus(indexNode, create.constant(1)))),
        forAllIndex
      )
    )

    val methodArguments = List(sequenceArgument, indexArgument)


    val declaration = create.method_decl(sequenceType, contract.getContract, methodName, methodArguments.toArray, null)
    declaration.setStatic(true)
    declaration
  }

  def validIndex(sequenceName: String, indexName: String): ASTNode = {
    val lowerbound = lte(create.constant(0), name(indexName))
    val upperbound = less(name(indexName), size(name(sequenceName)))
    val validIndexCondition = and(lowerbound, upperbound)
    validIndexCondition
  }
}
