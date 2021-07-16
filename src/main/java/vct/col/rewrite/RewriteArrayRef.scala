package vct.col.rewrite

import hre.ast.MessageOrigin
import vct.col.ast.`type`.{ASTReserved, PrimitiveSort, PrimitiveType, Type}
import vct.col.ast.expr._
import vct.col.ast.expr.constant.StructValue
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.decl.{DeclarationStatement, Method, ProgramUnit}
import vct.col.ast.util.{AbstractRewriter, ContractBuilder, SequenceUtils}

import scala.collection.mutable

object RewriteArrayRef {
  val constructorName: mutable.Map[(Type, Int), String] = mutable.Map()
  val valuesName: mutable.Map[Type, String] = mutable.Map()
  val subName: mutable.Map[Type, String] = mutable.Map()

  val namesUsed: mutable.Set[String] = mutable.Set()

  def getArrayConstructor(t: Type, definedDimensions: Int): String = {
    constructorName getOrElseUpdate((t, definedDimensions), getUniqueName("array_new_" + t.toString))
  }

  def getArrayValues(t: Type): String = {
    valuesName getOrElseUpdate(t, getUniqueName("array_values_" + t.toString))
  }

  def getUniqueName(str: String): String = {
    var result = str.replaceAll("[^a-zA-Z0-9$_']", "_")
    while (namesUsed contains result) {
      result += "$"
    }
    namesUsed += result
    result
  }

  def getSubArray(t: Type): String = {
    subName getOrElseUpdate(t, getUniqueName("sub_array_" + t.toString))
  }
}

class RewriteArrayRef(source: ProgramUnit) extends AbstractRewriter(source) {
  override def rewriteAll(): ProgramUnit = {
    val res = super.rewriteAll()

    create.enter()
    create.setOrigin(new MessageOrigin("Array Constructors"))
    for (((t, definedDimensions), name) <- RewriteArrayRef.constructorName) {
      res.add(arrayConstructorFor(t, definedDimensions, name))
    }
    create.leave()

    create.enter()
    create.setOrigin(new MessageOrigin("Array Values Functions"))
    for ((t, name) <- RewriteArrayRef.valuesName) {
      res.add(arrayValuesFor(t, name))
    }
    create.leave()

    create.enter()
    create.setOrigin(new MessageOrigin("Generated Subarray Functions"))
    for ((t, name) <- RewriteArrayRef.subName) {
      res.add(subArrayFor(t, name))
    }
    create.leave()

    res
  }

  override def visit(operator: OperatorExpression): Unit = {
    operator.operator match {
      case StandardOperator.ValidArray =>
        val t = operator.arg(0).getType
        val array = rewrite(operator.arg(0))
        val size = rewrite(operator.arg(1))
        result = validArrayFor(array, t, size)
      case StandardOperator.ValidMatrix =>
        val t = operator.arg(0).getType
        val matrix = rewrite(operator.arg(0))
        val size0 = rewrite(operator.arg(1))
        val size1 = rewrite(operator.arg(2))
        result = validMatrixFor(matrix, t, size0, size1)
      case StandardOperator.NewArray =>
        result = create.invokation(
          null, null,
          RewriteArrayRef.getArrayConstructor(operator.arg(0).asInstanceOf[Type], operator.args.length - 1),
          rewrite(operator.args.tail.toArray): _*)
      case StandardOperator.Subscript =>
        var baseType: Type = operator.arg(0).getType
        var result = rewrite(operator.arg(0))
        val subscript = rewrite(operator.arg(1))

        if (baseType.isPrimitive(PrimitiveSort.Option)) {
          result = create.expression(StandardOperator.OptionGet, result)
          baseType = baseType.asInstanceOf[PrimitiveType].firstarg.asInstanceOf[Type]
        }

        if (!(baseType.isPrimitive(PrimitiveSort.Array) ||
          baseType.isPrimitive(PrimitiveSort.Sequence))) {
          Fail("Unsupported array format")
        } else {
          result = create.expression(StandardOperator.Subscript, result, subscript)
          baseType = baseType.asInstanceOf[PrimitiveType].firstarg.asInstanceOf[Type]
        }

        if (baseType.isPrimitive(PrimitiveSort.Cell)) {
          result = create.dereference(result, "item")
          baseType = baseType.asInstanceOf[PrimitiveType].firstarg.asInstanceOf[Type]
        }

        this.result = result
      case StandardOperator.Values =>
        val array = operator.arg(0)
        result = create.invokation(null, null, RewriteArrayRef.getArrayValues(array.getType), rewrite(operator.args.toArray): _*)
      case StandardOperator.Drop =>
        val seqInfo = SequenceUtils.getInfoOrFail(operator.arg(0), "Expected a sequence type but got %s")
        if (seqInfo.getSequenceSort == PrimitiveSort.Array) {
          val array = rewrite(operator.arg(0))
          array.setType(seqInfo.getCompleteType)

          val dropCount = rewrite(operator.arg(1))
          var properArray = array
          if (seqInfo.isOpt) {
            properArray = create.expression(StandardOperator.OptionGet, properArray)
          }
          result = create.invokation(null, null, RewriteArrayRef.getSubArray(seqInfo.getCompleteType), array, dropCount, create.expression(StandardOperator.Length, properArray))
        } else {
          super.visit(operator)
        }
      case _ =>
        super.visit(operator)
    }
  }

  def validArrayFor(input: ASTNode, t: Type, size: ASTNode): ASTNode = {
    val conditions: mutable.ListBuffer[ASTNode] = mutable.ListBuffer()
    val seqInfo = SequenceUtils.expectArrayType(t, "Expected an array type here, but got %s")

    var value = input

    if (seqInfo.isOpt) {
      conditions += neq(value, create.reserved_name(ASTReserved.OptionNone))
      value = create.expression(StandardOperator.OptionGet, value)
    }

    conditions += eq(create.expression(StandardOperator.Length, value), size)

    conditions.reduce(and _)
  }

  def validMatrixFor(input: ASTNode, t: Type, size0: ASTNode, size1: ASTNode): ASTNode = {
    val conditions: mutable.ListBuffer[ASTNode] = mutable.ListBuffer()
    val seqInfo0 = SequenceUtils.expectArrayType(t, "Expected a matrix type here, but got %s")
    val seqInfo1 = SequenceUtils.expectArrayType(seqInfo0.getElementType, "The subscripts of a matrix should be of array type, but got %s")

    var matrix = input

    if (seqInfo0.isOpt) {
      conditions += neq(matrix, create.reserved_name(ASTReserved.OptionNone))
      matrix = create.expression(StandardOperator.OptionGet, matrix)
    }

    conditions += eq(create.expression(StandardOperator.Length, matrix), size0)

    val quantVar = name("i0")
    // We also carry these variables through for later
    val quantI0 = name("i0")
    val quantI1 = name("i1")
    val quantJ0 = name("j0")
    val quantJ1 = name("j1")
    val quantDecls = List(new DeclarationStatement(quantVar.getName, create.primitive_type(PrimitiveSort.Integer))).toArray
    var quantGuard = and(lte(constant(0), quantVar), less(quantVar, size0))

    var row: ASTNode = create.expression(StandardOperator.Subscript, matrix, quantVar)
    var rowI: ASTNode = create.expression(StandardOperator.Subscript, matrix, quantI0)
    var rowJ: ASTNode = create.expression(StandardOperator.Subscript, matrix, quantJ0)

    if (seqInfo0.isCell) {
      row = create.dereference(row, "item")
      rowI = create.dereference(rowI, "item")
      rowJ = create.dereference(rowJ, "item")
      conditions += create.starall(quantGuard, create.expression(StandardOperator.Perm, create.pattern(row), create.reserved_name(ASTReserved.ReadPerm)), quantDecls: _*)
    }

    if (seqInfo1.isOpt) {
      conditions += create.forall(quantGuard, neq(create.pattern(row), create.reserved_name(ASTReserved.OptionNone)), quantDecls: _*)
      row = create.expression(StandardOperator.OptionGet, row)
      rowI = create.expression(StandardOperator.OptionGet, rowI)
      rowJ = create.expression(StandardOperator.OptionGet, rowJ)
    }

    conditions += create.forall(quantGuard, eq(create.expression(StandardOperator.Length, create.pattern(row)), size1), quantDecls: _*)

    // Show injectivitiy: if two cells are equal, their indices are equal.
    if (seqInfo1.isCell) {
      // We compare matrix[i0][i1] and matrix[j0][j1]. We're just comparing the cells themselves, so no permission needed.
      var cellI: ASTNode = rowI
      var cellJ: ASTNode = rowJ

      if (seqInfo1.isOpt) {
        cellI = create.expression(StandardOperator.OptionGet, cellI)
        cellJ = create.expression(StandardOperator.OptionGet, cellJ)
      }

      cellI = create.expression(StandardOperator.Subscript, cellI, quantI1)
      cellJ = create.expression(StandardOperator.Subscript, cellJ, quantJ1)

      // the second dimension is of cell types, so we've reached the cells here.

      val quantDecls = Array(
        new DeclarationStatement(quantI0.getName, create.primitive_type(PrimitiveSort.Integer)),
        new DeclarationStatement(quantI1.getName, create.primitive_type(PrimitiveSort.Integer)),
        new DeclarationStatement(quantJ0.getName, create.primitive_type(PrimitiveSort.Integer)),
        new DeclarationStatement(quantJ1.getName, create.primitive_type(PrimitiveSort.Integer)),
      )

      val quantGuard = List(
        and(lte(constant(0), quantI0), less(quantI0, size0)),
        and(lte(constant(0), quantI1), less(quantI1, size1)),
        and(lte(constant(0), quantJ0), less(quantJ0, size0)),
        and(lte(constant(0), quantJ1), less(quantJ1, size1)),
        eq(cellI, cellJ)
      ).reduce(and)

      val claim = and(eq(quantI0, quantJ0), eq(quantI1, quantJ1))

      val triggers = Array(Array(cellI, cellJ))

      val condition = new BindingExpression(Binder.Star, new PrimitiveType(PrimitiveSort.Resource), quantDecls, triggers, quantGuard, claim)
      condition.setOrigin(create.getOrigin)

      conditions += condition
    }

    conditions.reduce(star)
  }

  override def visit(dereference: Dereference): Unit = {
    if (dereference.field == Dereference.ArrayLength) {
      var objType = dereference.obj.getType

      if (objType.isPrimitive(PrimitiveSort.Cell)) {
        objType = objType.firstarg.asInstanceOf[Type]
      }

      if (objType.isPrimitive(PrimitiveSort.Array)) {
        result = create.expression(StandardOperator.Length, rewrite(dereference.obj))
      } else if (objType.isPrimitive(PrimitiveSort.Option) && objType.firstarg.asInstanceOf[Type].isPrimitive(PrimitiveSort.Array)) {
        result = create.expression(StandardOperator.Length, create.expression(StandardOperator.OptionGet, rewrite(dereference.obj)))
      } else if (objType.isPrimitive(PrimitiveSort.Sequence) ||
        objType.isPrimitive(PrimitiveSort.Bag) ||
        objType.isPrimitive(PrimitiveSort.Set)) {
        result = create.expression(StandardOperator.Size, rewrite(dereference.obj))
      } else {
        super.visit(dereference)
      }
    } else {
      super.visit(dereference)
    }
  }

  override def visit(struct_value: StructValue): Unit = {
    struct_value.`type` match {
      case t: PrimitiveType if Set(PrimitiveSort.Option, PrimitiveSort.Array).contains(t.sort) =>
        RewriteArrayRef.getArrayConstructor(struct_value.getType, 1)
      case _ =>
    }

    super.visit(struct_value)
  }

  def quantify(dimension: Int, claim: ASTNode): ASTNode = {
    if (dimension == 0) {
      claim
    } else {
      val declarations = for (i <- 0 until dimension) yield new DeclarationStatement("i" + i, create.primitive_type(PrimitiveSort.Integer))
      val guards = for (i <- 0 until dimension) yield and(lte(constant(0), name("i" + i)), less(name("i" + i), name("size" + i)))
      create.starall(guards.reduce(and _), claim, declarations: _*)
    }
  }

  def arrayConstructorContract(contract: ContractBuilder, t: Type, value: ASTNode, dimension: Int, definedDimensions: Int): (Type, ASTNode) = {
    if (!t.isInstanceOf[PrimitiveType]) {
      return (t, value)
    }

    val pType = t.asInstanceOf[PrimitiveType]

    val claim = pType.sort match {
      case PrimitiveSort.Option => neq(create.pattern(value), create.reserved_name(ASTReserved.OptionNone))
      case PrimitiveSort.Array => eq(create.expression(StandardOperator.Length, create.pattern(value)), name("size" + dimension))
      case PrimitiveSort.Cell => create.expression(StandardOperator.Perm, create.pattern(create.dereference(value, "item")), create.reserved_name(ASTReserved.FullPerm))
      case _ =>
        return (t, value)
    }

    contract.ensures(quantify(dimension, claim))

    val newType = pType.args.head.asInstanceOf[Type]

    val newValue = pType.sort match {
      case PrimitiveSort.Option => create.expression(StandardOperator.OptionGet, value)
      case PrimitiveSort.Array => create.expression(StandardOperator.Subscript, value, name("i" + dimension))
      case PrimitiveSort.Cell => create.dereference(value, "item")
      case _ => return (t, value)
    }

    val newDimension = if (pType.sort == PrimitiveSort.Array) dimension + 1 else dimension

    // If the last array dimension is accessed, only permit Cell types.
    if (newDimension == definedDimensions && !newType.isPrimitive(PrimitiveSort.Cell)) {
      return (newType, newValue)
    }

    arrayConstructorContract(contract, newType, newValue, newDimension, definedDimensions)
  }

  def arrayConstructorFor(t: Type, definedDimensions: Int, methodName: String): ASTNode = {
    val contract = new ContractBuilder
    val result = create.reserved_name(ASTReserved.Result)
    val (elementType, elementValue) = arrayConstructorContract(contract, t, result, 0, definedDimensions)
    contract.ensures(quantify(definedDimensions, eq(create.pattern(elementValue), elementType.zero)))

    val methodArguments = for (i <- 0 until definedDimensions) yield new DeclarationStatement("size" + i, create.primitive_type(PrimitiveSort.Integer))

    val declaration = create.method_decl(t, contract.getContract, methodName, methodArguments.toArray, null)
    declaration.setStatic(true)
    declaration
  }

  def arrayValuesFor(t: Type, methodName: String): ASTNode = {
    val contract = new ContractBuilder
    var array: ASTNode = name("array")
    val from = name("from")
    val to = name("to")
    var arrayType = t
    val result = create.reserved_name(ASTReserved.Result)

    if (arrayType.isPrimitive(PrimitiveSort.Option)) {
      contract.requires(neq(array, create.reserved_name(ASTReserved.OptionNone)))
      array = create.expression(StandardOperator.OptionGet, array)
      arrayType = arrayType.firstarg.asInstanceOf[Type]
    }

    if (!arrayType.isPrimitive(PrimitiveSort.Array)) {
      Fail("Unsupported array format")
    }

    arrayType = arrayType.firstarg.asInstanceOf[Type]

    val arrayLength = create.expression(StandardOperator.Length, array)
    val seqLength = create.expression(StandardOperator.Size, result)

    contract.requires(lte(constant(0), from))
    contract.requires(lte(from, to))
    contract.requires(lte(to, arrayLength))

    val quantVar = name("i")
    val quantDecls = List(new DeclarationStatement("i", new PrimitiveType(PrimitiveSort.Integer))).toArray
    var quantGuardAdd = and(lte(from, quantVar), less(quantVar, to))
    var quantGuard = and(lte(constant(0), quantVar), less(quantVar, create.expression(StandardOperator.Minus, to, from)))

    var quantArrayItem: ASTNode = create.expression(StandardOperator.Subscript, array, quantVar)
    var quantArrayItemAdd: ASTNode = create.expression(StandardOperator.Subscript, array, create.expression(StandardOperator.Plus, quantVar, from))
    var quantSeqItemSub = create.expression(StandardOperator.Subscript, result, create.expression(StandardOperator.Minus, quantVar, from))
    var quantSeqItem = create.expression(StandardOperator.Subscript, result, quantVar)

    if (arrayType.isPrimitive(PrimitiveSort.Cell)) {
      quantArrayItem = create.dereference(quantArrayItem, "item")
      quantArrayItemAdd = create.dereference(quantArrayItemAdd, "item")
      arrayType = arrayType.firstarg.asInstanceOf[Type]
    }

    contract.requires(create.starall(quantGuardAdd, create.expression(StandardOperator.Perm, create.pattern(quantArrayItem), create.reserved_name(ASTReserved.ReadPerm)), quantDecls: _*))

    contract.ensures(eq(seqLength, create.expression(StandardOperator.Minus, to, from)))
    contract.ensures(create.forall(quantGuardAdd, eq(quantArrayItem, create.pattern(quantSeqItemSub)), quantDecls: _*))
    contract.ensures(create.forall(quantGuard, eq(quantArrayItemAdd, create.pattern(quantSeqItem)), quantDecls: _*))

    val arguments = List(
      new DeclarationStatement("array", t),
      new DeclarationStatement("from", new PrimitiveType(PrimitiveSort.Integer)),
      new DeclarationStatement("to", new PrimitiveType(PrimitiveSort.Integer)),
    )

    val resType = create.primitive_type(PrimitiveSort.Sequence, arrayType)

    val declaration = create.function_decl(resType, contract.getContract, methodName, arguments.toArray, null)
    declaration.setStatic(true)
    declaration
  }

  def subArrayFor(t: Type, methodName: String): ASTNode = {
    var result: ASTNode = create.reserved_name(ASTReserved.Result)
    var array: ASTNode = name("array")

    val contract = new ContractBuilder
    val from: ASTNode = name("from")
    val to: ASTNode = name("to")
    val seqInfo = SequenceUtils.expectArrayType(t, "Expected array but got %s")
    val i: ASTNode = name("i")
    val iDecl: Array[DeclarationStatement] = Array(create.field_decl("i", create.primitive_type(PrimitiveSort.Integer)))

    if (seqInfo.isOpt) {
      contract.requires(neq(array, create.reserved_name(ASTReserved.OptionNone)))
      contract.ensures(neq(result, create.reserved_name(ASTReserved.OptionNone)))
      array = create.expression(StandardOperator.OptionGet, array)
      result = create.expression(StandardOperator.OptionGet, result)
    }

    contract.ensures(eq(create.expression(StandardOperator.Length, result), create.expression(StandardOperator.Minus, to, from)))

    contract.requires(List(
      lte(constant(0), from),
      lte(from, to),
      lte(to, create.expression(StandardOperator.Length, array))
    ).reduce(and))

    var eqLeft: ASTNode = create.expression(StandardOperator.Subscript, array, create.expression(StandardOperator.Plus, i, from))
    var eqRight: ASTNode = create.expression(StandardOperator.Subscript, result, i)

    contract.ensures(create.forall(
      and(lte(constant(0), i), less(i, create.expression(StandardOperator.Minus, to, from))),
      eq(eqLeft, create.pattern(eqRight)),
      iDecl: _*
    ))

    contract.ensures(
      create.expression(StandardOperator.Implies,
        and(eq(from, constant(0)), eq(to, create.expression(StandardOperator.Length, array))),
        eq(result, array)
      )
    )

    val arguments = Array(
      new DeclarationStatement("array", t),
      new DeclarationStatement("from", create.primitive_type(PrimitiveSort.Integer)),
      new DeclarationStatement("to", create.primitive_type(PrimitiveSort.Integer))
    )

    val declaration = create.method_kind(Method.Kind.Pure, t, contract.getContract, methodName, arguments, false, null)
    declaration.setStatic(true)
    declaration
  }
}