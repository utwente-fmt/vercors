package vct.col.rewrite

import vct.col.ast.`type`.ClassType
import vct.col.ast.expr.{NameExpression, NameExpressionKind}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.decl.{ASTClass, ASTDeclaration, AxiomaticDataType, NameSpace, ProgramUnit}
import vct.col.ast.util.{AbstractRewriter, RecursiveVisitor}
import vct.java.ASTClassLoader

import scala.collection.JavaConverters._

object JavaResolver {
  val DOT = "_DOT_"
}

case class JavaResolver(override val source: ProgramUnit) extends AbstractRewriter(source) {
  var currentNamespace: Option[NameSpace] = None

  override def visit(ns: NameSpace): Unit = {
    if(currentNamespace.isEmpty) {
      currentNamespace = Some(ns)

      ns.asScala.map(rewrite(_)).foreach(target.add(_))

      currentNamespace = None
    } else {
      ns.getOrigin.report("error", "Nested namespaces are not supported")
      Fail("")
    }
  }

  private def currentPackageName: Seq[String] = currentNamespace match {
    case None => Seq()
    case Some(ns) => ns.getDeclName.name.filter(_.nonEmpty)
  }

  override def visit(cls: ASTClass): Unit =
    if(cls.kind == ASTClass.ClassKind.Plain) {
      val res = create ast_class(
        (currentPackageName ++ cls.getName.split('.')).mkString(JavaResolver.DOT),
        ASTClass.ClassKind.Plain,
        rewrite(cls.parameters),
        rewrite(cls.super_classes),
        rewrite(cls.implemented_classes),
      )

      cls.asScala.map(rewrite(_)).foreach(res add _)
      result = res
    } else {
      super.visit(cls)
    }

  override def visit(t: ClassType): Unit = {
    val names = require(t.names)
    result = create class_type(names.mkString(JavaResolver.DOT), rewrite(t.argsJava))
  }

  override def visit(name: NameExpression): Unit = name.kind match {
    case NameExpressionKind.Unresolved =>
      variables.lookup(name.name) match {
        case null =>
          loadClass(Seq(name.name)) match {
            case None =>
              // This leaves an unresolved name in the AST
              super.visit(name)
            case Some(cls) =>
              result = create class_type cls.mkString(JavaResolver.DOT)
          }
        case res =>
          name.setKind(res.kind)
          super.visit(name)
      }
    case _ => super.visit(name)
  }

  private def require(name: Seq[String]): Seq[String] = loadClass(name) match {
    case None => Fail("Could not find class: %s", name); ???
    case Some(fqcn) => fqcn
  }

  private def loadClass(name: Seq[String]): Option[Seq[String]] = {
    if(predef.contains(name)) {
      Some(name)
    } else if(currentNamespace.nonEmpty && predef.contains(currentPackageName ++ name)) {
      Some(currentPackageName ++ name)
    } else {
      val cls = ASTClassLoader.load(name, currentNamespace) match {
        case Some(cls) => cls
        case None => return None
      }

      if(!scanned.contains(cls))
        toScan += cls

      Some(cls.getName.split('.'))
    }
  }

  var predef: Set[Seq[String]] = Set()
  var scanned: Set[ASTClass] = Set()
  var toScan: Set[ASTClass] = Set()

  private def collectPredef(node: ASTNode, ns: Option[NameSpace] = None): Unit = node match {
    case cls: ASTClass =>
      predef += (ns.map(_.getDeclName.name.toSeq.filter(_.nonEmpty)).getOrElse(Seq.empty[String]) ++ cls.getFullName.toSeq)
    case adt: AxiomaticDataType =>
      predef += Seq(adt.name)
    case nameSpace: NameSpace =>
      if(ns.isEmpty) {
        nameSpace.asScala.foreach(collectPredef(_, Some(nameSpace)))
      } else {
        Fail("Layered namespaces are not supported")
      }
    case _ =>
  }

  override def rewriteAll(): ProgramUnit = {
    source.asScala.foreach(collectPredef(_))
    source.asScala.map(rewrite(_)).foreach {
      case null =>
      case other => target.add(other)
    }

    while(toScan.nonEmpty) {
      val next = toScan.head
      target().add(rewrite(next))
      toScan -= next
      scanned += next
    }

    target
  }
}
