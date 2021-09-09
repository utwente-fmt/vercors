package vct.helper

import vct.col.ast.{Constant, Declaration, Node, Program, Ref}

import scala.collection.mutable


/**
 * This object is to compare two programs of the ast.
 */
object AstComparer {


  /**
   * Throws an AstUnequalException when the Programs are not equal
   */
  def astProgramEquals(left: Program, right: Program): Unit ={
    val succession: mutable.Map[Declaration, Declaration] = mutable.Map[Declaration, Declaration]()
    val leftGlobalDeclarations = left.declarations
    val rightGlobalDeclarations = right.declarations
    if (leftGlobalDeclarations.size != rightGlobalDeclarations.size) {
      throw AstUnequalException("Mismatched globalDeclaration length",left,right)
    }
    succession ++= leftGlobalDeclarations.zip(rightGlobalDeclarations).toMap

    for((leftDeclaration, rightDeclaration) <- leftGlobalDeclarations.zip(rightGlobalDeclarations)) {
      astNodeEquals(leftDeclaration, rightDeclaration, succession)
    }
  }

  /**
   * Throws an AstUnequalException when the Nodes are not equal
   */
  def astNodeEquals(left: Node, right: Node, succession: mutable.Map[Declaration, Declaration]): Unit = {
    val leftDecls = left.subnodes.collect { case decl: Declaration => decl }
    val rightDecls = right.subnodes.collect { case decl: Declaration => decl }
    if (leftDecls.size != rightDecls.size) {
      throw AstUnequalException("Mismatched scope length",left,right)
    }
    succession ++= leftDecls.zip(rightDecls).toMap

    if (left.getClass != right.getClass) {
      val leftClass = left.getClass
      val rightClass = right.getClass
      throw AstUnequalException(s"The kind $leftClass is not the same as $rightClass",left,right)
    }

    if (left.subnodes.size != right.subnodes.size) {
      throw AstUnequalException("Unequal number of subnodes",left,right)
    }

    (left, right) match {
      case (left: Declaration, right: Declaration) =>
        astDeclarationEquals(left, right, succession)
      case (left: scala.Product, right: scala.Product) =>
        astProductEquals(left, right, succession)
      case(left: Constant[_], right: Constant[_]) =>
        astConstantEquals(left, right)
      case (_, _) =>
        throw AstUnequalException("Incomparable nodes",left,right)
    }
    for ((left, right) <- left.subnodes.zip(right.subnodes)) {
      astNodeEquals(left, right, succession)
    }
  }

  /**
   * Throws an AstUnequalException when the values of the products are not equal. It does not check the subnodes
   */
  def astProductEquals(leftProduct: scala.Product, rightProduct: scala.Product, succession: mutable.Map[Declaration, Declaration]): Unit = {
    for ((leftValue, rightValue) <- leftProduct.productIterator.zip(rightProduct.productIterator)) {
      astValueEquals(leftValue,rightValue,succession)
    }
  }

  /**
   * Throws an AstUnequalException when the values are not equal. It does compare nodes.
   */
  def astValueEquals(left: Any, right: Any, succession: mutable.Map[Declaration, Declaration]): Unit ={
    (left,right) match{
      case (left: Ref[Declaration], right: Ref[Declaration]) =>
        astDeclarationEquals(left.decl, right.decl, succession)
      case (_: Node, _: Node) =>
        //subnodes are compared in astNodeEquals
      case (leftList: List[_], rightList: List[_]) =>
        for ((leftListItem, rightListItem) <- leftList.zip(rightList)) {
          astValueEquals(leftListItem,rightListItem,succession)
        }
      case (left, right) =>
        if (left != right)
          throw AstUnequalException("Unequal values",left,right)
    }
  }

  /**
   * Throws an AstUnequalException when the declarations do not references the same declaration. It does not check the subnodes or values.
   */
  def astDeclarationEquals(left: Declaration, right: Declaration, succession: mutable.Map[Declaration, Declaration]): Unit = {
    //TODO: Does not check all values of the declaration. The subnodes are checked in astNodeEquals.
    succession.get(left) match {
      case Some(decl) if decl == right =>
      case Some(other) =>
        throw AstUnequalException("Expected declaration of the right node but got another node",left,right,other)
      case None =>
        throw AstUnequalException("Could not find declaration for the left node",left,right)
    }
  }

  /*
   * Throws an AstUnequalException when values of the constants are not equal.
   */
  def astConstantEquals(left: Constant[_], right: Constant[_]): Unit = {
    if(left.value != right.value){
      throw AstUnequalException("Value of constants are not the same",left,right)
    }
  }


}
