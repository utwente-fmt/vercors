package vct.col.rewrite

import hre.ast.{MessageOrigin, Origin}
import vct.col.ast.`type`.{ASTReserved, PrimitiveSort, Type}
import vct.col.ast.expr.{Binder, BindingExpression, NameExpression, OperatorExpression, SetComprehension, StandardOperator}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.decl.{DeclarationStatement, ProgramUnit}
import vct.col.ast.util.ContractBuilder

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object GenerateADTFunctions {
  val getRemoveFromSeqName: mutable.Map[Type, String] = mutable.Map()
  val getSetCompName: mutable.Map[SetComprehension, String] = mutable.Map()

  val namesUsed: mutable.Set[String] = mutable.Set()

  def getUniqueName(str: String): String = {
    var result = str.replaceAll("[^a-zA-Z0-9$_']", "_")
    while (namesUsed contains result) {
      result += "$"
    }
    namesUsed += result
    result
  }

  def getRemoveFromSeqFunction(t: Type): String = {
    getRemoveFromSeqName getOrElseUpdate(t, getUniqueName("remove_by_index_" + t.toString))
  }

  def getSetCompFunction(sc: SetComprehension): String = {
    getSetCompName getOrElseUpdate(sc, getUniqueName("vct_set_comprehension_" + sc.result_type.toString))
  }
}

class GenerateADTFunctions(source: ProgramUnit) extends AbstractRewriter(source) {
  override def rewriteAll(): ProgramUnit = {
    val res = super.rewriteAll()

    create.enter()
    create.setOrigin(new MessageOrigin("Sequence Function: Remove"))
    for ((t, name) <- GenerateADTFunctions.getRemoveFromSeqName) {
      res.add(removeFromSequenceByIndex(t, name))
    }
    create.leave()

    create.enter()
    create.setOrigin(new MessageOrigin("Set Function: Set Comprehension"))
    for ((sc, name) <- GenerateADTFunctions.getSetCompName) {
      res.add(generateSetComprehensionFunction(sc, name))
    }
    create.leave()

    res
  }

  override def visit(binding: BindingExpression): Unit = {
    binding.binder match {
      case Binder.SetComp =>
        // Get Arguments
        var boundedVariables = binding.asInstanceOf[SetComprehension].boundedVariables
        val args = binding.asInstanceOf[SetComprehension].boundedVariables.map(_.asInstanceOf[OperatorExpression].arg(1))
        result = create.invokation(null, null, GenerateADTFunctions.getSetCompFunction(binding.asInstanceOf[SetComprehension]), rewrite(args.toArray): _*);
      case _ =>
        super.visit(binding)
    }
  }

  override def visit(operator: OperatorExpression): Unit = {
    operator.operator match {
      case StandardOperator.RemoveAt =>
        val sequenceType = operator.arg(0).getType
        result = create.invokation(null, null, GenerateADTFunctions.getRemoveFromSeqFunction(sequenceType), rewrite(operator.args.toArray): _*)
      case _ =>
        super.visit(operator)
    }
  }


  def generateSetComprehensionFunction(setComprehension: SetComprehension, functionName: String): ASTNode = {
    val resultType = setComprehension.result_type
    val contract = new ContractBuilder
    val result = create.reserved_name(ASTReserved.Result, resultType)


    var selector: ASTNode = create.constant(true)
    if (setComprehension.boundedVariables != null && !setComprehension.boundedVariables.isEmpty) {
      selector = setComprehension.boundedVariables.map(boundedVar =>
        create.expression(StandardOperator.Member,
          boundedVar.asInstanceOf[OperatorExpression].arg(0),
          create.argument_name("setCompArg" + boundedVar.asInstanceOf[OperatorExpression].arg(0).asInstanceOf[NameExpression].getName)

        )
      ).reduce(create.expression(StandardOperator.And, _, _))
    }

    val argsOfFunction = setComprehension.boundedVariables.map( boundedVar =>
      new DeclarationStatement(
        "setCompArg" + boundedVar.asInstanceOf[OperatorExpression].arg(0).asInstanceOf[NameExpression].getName,
        boundedVar.asInstanceOf[OperatorExpression].arg(1).getType
      )
    )

    //// Do all the work
    contract.ensures(
      create.forall(
        // The actual selector (like x in y && a in b)
        selector,
        eq(
          setComprehension.select,
          create.expression(StandardOperator.Member, setComprehension.main, result)
        ),
        setComprehension.getDeclarations:_*
      )
    )

    val function = create.function_decl(resultType, contract.getContract(), functionName, argsOfFunction.toArray, null)
    function.setStatic(true)
    function
  }


  def removeFromSequenceByIndex(sequenceType: Type, functionName: String): ASTNode = {
    val contract = new ContractBuilder
    val result = create.reserved_name(ASTReserved.Result, sequenceType)

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
      create.forall(
        valueInRange(indexNode, constant(0), name(indexArgumentName)),
        eq(
          get(result, indexNode),
          get(name(sequenceArgumentName), indexNode)
        ),
        forAllIndex
      )
    )

    contract.ensures(
      create.forall(
        valueInRange(indexNode, name(indexArgumentName), size(result)),
        eq(
          get(result, indexNode),
          get(name(sequenceArgumentName), plus(indexNode, constant(1)))),
        forAllIndex
      )
    )

    val functionArguments = List(sequenceArgument, indexArgument)
    val declaration = create.function_decl(sequenceType, contract.getContract, functionName, functionArguments.toArray, null)
    declaration.setStatic(true)
    declaration
  }

  def validIndex(sequenceName: String, indexName: String, inclusive: Boolean = false): ASTNode = {
    valueInRange(name(indexName), constant(0), size(name(sequenceName)), inclusive)
  }


  def valueInRange(value: ASTNode, lowerbound: ASTNode, upperbound: ASTNode, inclusive: Boolean = false): ASTNode = {
    val left = lte(lowerbound, value)
    val right = if (inclusive) lte(value, upperbound) else less(value, upperbound)

    and(left, right)
  }
}
