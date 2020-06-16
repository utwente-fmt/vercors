package vct.col.rewrite

import java.util
import java.util.stream.{Collectors, StreamSupport}

import hre.ast.{MessageOrigin, Origin}
import vct.col.ast.`type`
import vct.col.ast.`type`.{ASTReserved, ClassType, PrimitiveSort, Type}
import vct.col.ast.expr.{Binder, BindingExpression, NameExpression, OperatorExpression, SetComprehension, StandardOperator}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.decl.{DeclarationStatement, ProgramUnit}
import vct.col.ast.util.ContractBuilder
import vct.col.util.FieldAccessCollector

import collection.JavaConverters
import scala.collection.mutable
import scala.collection.JavaConverters._

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
        var boundedVariables = binding.asInstanceOf[SetComprehension].variables
        val args = JavaConverters.collectionAsScalaIterable(binding.asInstanceOf[SetComprehension].variables.values())

        result = create.invokation(null,
          null,
          GenerateADTFunctions.getSetCompFunction(binding.asInstanceOf[SetComprehension]),
          rewrite(args.toArray): _*);
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

    val fieldCollector = new FieldAccessCollector
    setComprehension.main.apply(fieldCollector)
    setComprehension.select.apply(fieldCollector)

    val fieldAccessses = fieldCollector.getFieldAccesses

    val usedClasses = setComprehension.getDeclarations.filter(_.`type`.isInstanceOf[ClassType])

    for (clazz <- usedClasses) {
      val conditions: mutable.ListBuffer[ASTNode] = mutable.ListBuffer()
      for (field <- fieldAccessses.asScala) {
        conditions +=
          create.starall(
            create.expression(StandardOperator.Member, name(clazz.getDeclName.toString()), name("setCompArg" + clazz.getDeclName.toString)),
            create.expression(
              StandardOperator.Value,
              create.dereference(create.local_name(clazz.name),
                field.field)
            )
            ,
            clazz
          )
      }
      contract.requires(conditions.reduce(star _))
    }

    var selector: ASTNode = create.constant(true)
    if (setComprehension.variables != null && !setComprehension.variables.isEmpty) {
      selector = JavaConverters.collectionAsScalaIterable(setComprehension.variables.entrySet()).map(entry =>
        create.expression(StandardOperator.Member,
          entry.getKey,
          create.argument_name("setCompArg" + entry.getKey.getName)
        )
      ).reduce(create.expression(StandardOperator.And, _, _))
    }


    val argsOfFunction = JavaConverters.collectionAsScalaIterable(setComprehension.variables.entrySet())
      .map(boundedVar =>
        new DeclarationStatement(
          "setCompArg" + boundedVar.getKey.getName,
          boundedVar.getValue.getType
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
        setComprehension.getDeclarations: _*
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
