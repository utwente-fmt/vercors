package vct.col.ast.`type`

import hre.lang.System.Debug
import vct.col.ast._
import vct.col.ast.expr.NameExpression
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.decl.{ASTClass, ASTDeclaration, NameSpace, ProgramUnit}
import vct.col.ast.util.{ASTMapping, ASTMapping1, ASTVisitor, ExternalClassLoader, TypeMapping, VisitorHelper}

import scala.jdk.CollectionConverters._

object ClassType {
  val nullType = new ClassType("<<null>>")
  val labelType = new ClassType("<<label>>")

  val javaLangObjectName = "java_DOT_lang_DOT_Object"
  val javaLangThrowableName = "java_DOT_lang_DOT_Throwable"
  val javaLangExceptionName = "java_DOT_lang_DOT_Exception"
  val javaLangRuntimeExceptionName = "java_DOT_lang_DOT_RuntimeException"
  
  /** Tests whether `name` holds a default type name, for example "null" or "label". */
  def isDefaultName(name: List[String]): Boolean =
    name == nullType.names || name == labelType.names
}

/**
 * AST node that represents the type of classes (including their class parameters).
 * 
 * @param names A list of name parts that together constitute the full class name (including package name)
 * @param params A list of AST nodes representing the types of the class parameters
 * @author sccblom, whmoortwijn
 */
case class ClassType(names: List[String], params: List[ASTNode]) extends Type(params) with VisitorHelper {
  private var _definition: Option[ASTDeclaration] = None

  require(names.nonEmpty, "class types must have a name (at least one name part).")

  /** Constructs a new class type from Java constructs. */
  def this(names:List[String], params:java.util.List[ASTNode]) = this(names, params.asScala.toList)

  /** Constructs a new class type from Java constructs. */
  def this(names:Array[String], params:java.util.List[ASTNode]) = this(names.toList, params)
  def this(names:Array[String], args:Array[ASTNode]) = this(names.toList, args.toList)
  def this(names:Array[String]) = this(names, Array[ASTNode]())
  def this(name:String) = this(Array(name))

  def getName: String = names.last
  def getNameFull: Array[String] = names.toArray
  def getFullName(separator:String): String = names mkString separator
  def getFullName : String = getFullName(".")

  def definition(source: Option[ProgramUnit],
                 loader: Option[ExternalClassLoader],
                 ns: Option[NameSpace]): Option[ASTDeclaration] = {
    _definition
      .orElse(source flatMap(source => Option(source.find(this))))
      .orElse(loader flatMap(_.load(names, ns)))
  }

  def definitionJava(source: ProgramUnit,
                     loader: ExternalClassLoader,
                     ns: NameSpace): ASTDeclaration =
    definition(Option(source), Option(loader), Option(ns)).orNull

  def setDefinition(decl:ASTDeclaration): Unit =
    _definition = Some(decl)

  /**
   * Checks if the type of this object (i.e. `this`) is a supertype of `ct` in the
   * given program context (`unit`).
   */
  private def searchForSupertype(other: ClassType,
                                 source: Option[ProgramUnit],
                                 loader: Option[ExternalClassLoader],
                                 ns: Option[NameSpace]): Boolean = {
    val myDef = definition(source, loader, ns)
    val otherDef = other.definition(source, loader, ns)
    if(myDef.nonEmpty && otherDef.nonEmpty && myDef == otherDef) {
      true
    } else if(other.names == names) {
      // PB: Incorrect: types may be compared across namespaces, making namespace a { Object } == namespace b { Object }
      true
    } else {
      other.definition(source, loader, ns) == definition(source, loader, ns) || (
        other.definition(source, loader, ns) match {
          case Some(cls: ASTClass) =>
            (cls.super_classes ++ cls.implemented_classes)
              .exists(parent => searchForSupertype(parent, source, loader, ns))
          case _ /* adts and such */ => false
        }
      )
    }
  }

  override def supertypeof(other: Type, source: Option[ProgramUnit], loader: Option[ExternalClassLoader], ns: Option[NameSpace]): Boolean =
    getName == "_AnyTypeForSimplificationRules" || (other match {
      case ct: ClassType =>
        ClassType.isDefaultName(ct.names) ||
          searchForSupertype(ct, source, loader, ns)
      case _ => false
    })
  
  /**
   * Tests whether this class is a supertype of `otherType` (in the given `context`).
   * 
   * @note `java.lang.Object` is a supertype of everything. However, implicit casts for anything other than class types
   *       is not supported (e.g. boxing, array conversion, ...) so we restrict it to class types for now, in order to
   *       generate a useful error.
   */
  override def supertypeof(source: ProgramUnit, otherType: Type): Boolean =
    supertypeof(otherType, Option(source), None, None)
  
  override def equals(obj:Any): Boolean = obj match {
    case other: ClassType => this.getFullName == other.getFullName
    case _ => false
  }
  
  override def hashCode: Int = this.getFullName.hashCode
  override def toString: String = this.getFullName + super.toString
  override def isNull: Boolean = this.names == ClassType.nullType.names
  override def zero = new NameExpression(ASTReserved.Null)
  
  override def accept_simple[T,A](m:ASTMapping1[T,A], arg:A): T = m.map(this, arg)
  override def accept_simple[T](v:ASTVisitor[T]): Unit = handle_standard(() => v.visit(this))
  override def accept_simple[T](m:ASTMapping[T]): T = handle_standard(() => m.map(this))
  override def accept_simple[T](m:TypeMapping[T]): T = handle_standard(() => m.map(this))

  override def debugTreeChildrenFields: Iterable[String] = Seq("names", "args")
  override def debugTreePropertyFields: Iterable[String] = Seq()
}
