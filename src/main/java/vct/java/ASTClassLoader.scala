package vct.java

import java.io.File
import java.nio.file.{Path, Paths}

import hre.ast.{FileOrigin, MessageOrigin}
import vct.col.ast.`type`.{PrimitiveSort, Type}
import vct.col.ast.stmt.decl.{ASTClass, Method, NameSpace}
import vct.col.ast.util.{ASTFactory, ClassName, ExternalClassLoader, SequenceUtils}
import vct.parsers.ColJavaParser
import vct.parsers.rewrite.RemoveBodies

import scala.collection.JavaConverters._
import scala.collection.mutable

object ASTClassLoader extends ExternalClassLoader {
  // PB: dumb java hack (is there no better way?)
  val INSTANCE: ExternalClassLoader = this

  private val REFLECTION_CACHE = mutable.Map[Seq[String], Option[ASTClass]]()
  private val FILE_CACHE = mutable.Map[(Path, Seq[String]), Option[ASTClass]]()

  private def potentialImportsOfNamespace(name: String, ns: NameSpace): Seq[Seq[String]] = {
    // First try package-local
    (ns.getDeclName.name.filter(_.nonEmpty) :+ name).toSeq +:
    // Otherwise by import order
    ns.imports.asScala.filter(
      // either by a.b.* import, or the last name must match (a.b.Class)
      `import` => `import`.all || `import`.name.last == name
    ).map(
      `import` => (if (`import`.all) `import`.name.filter(_.nonEmpty) :+ name else `import`.name.filter(_.nonEmpty)).toSeq
    )
  }

  private def loadFile(basePath: Path, parts: Seq[String]): Option[ASTClass] = FILE_CACHE.getOrElseUpdate((basePath, parts), {
    try {
      val parser = new ColJavaParser(false)
      // (path/to/src, Seq(java, lang, Object)) -> path/to/src/java/lang/Object.java
      val f = new File(parts.init.foldLeft(basePath.toFile)(new File(_, _)), parts.last + ".java")
      val pu = parser.parse(f)
      val strippedPU = new RemoveBodies(pu).rewriteAll()
      // Make sure the class name matches by finding it
      Option(strippedPU.find(new ClassName(parts:_*)))
    } catch {
      case _: hre.lang.Failure => None
    }
  })

  private def loadByFile(name: Seq[String], ns: Option[NameSpace]): Option[ASTClass] = {
    ns match {
      // If we're going by file: require a namespace, as we can't guess how many directories to go up etc.
      case Some(ns) if ns.getOrigin != null && ns.getOrigin.isInstanceOf[FileOrigin] =>
        var basePath = Paths.get(ns.getOrigin.asInstanceOf[FileOrigin].getName).toAbsolutePath.getParent
        for(_ <- ns.getDeclName.name.filter(_.nonEmpty /* pls */)) {
          basePath = basePath.getParent
        }

        if(name.size == 1) {
          // If it's one name and it's not defined, it must be imported or package-local
          potentialImportsOfNamespace(name.head, ns)
            .toStream.flatMap(loadFile(basePath, _)).headOption
        } else {
          // If it's multiple names, it must be fully qualified
          loadFile(basePath, name)
        }
      case _ => None
    }
  }

  private def jvmTypeToCOL[T](create: ASTFactory[_], t: Class[T]): Type = t match {
    case java.lang.Boolean.TYPE => create primitive_type PrimitiveSort.Boolean
    case java.lang.Character.TYPE => create primitive_type PrimitiveSort.Char
    case java.lang.Byte.TYPE => create primitive_type PrimitiveSort.Byte
    case java.lang.Short.TYPE => create primitive_type PrimitiveSort.Short
    case java.lang.Integer.TYPE => create primitive_type PrimitiveSort.Integer
    case java.lang.Long.TYPE => create primitive_type PrimitiveSort.Long
    case java.lang.Float.TYPE => create primitive_type PrimitiveSort.Float
    case java.lang.Double.TYPE => create primitive_type PrimitiveSort.Double
    case java.lang.Void.TYPE => create primitive_type PrimitiveSort.Void
    case str if str.getCanonicalName == "java.lang.String" => create primitive_type PrimitiveSort.String
    case arr if arr.isArray => SequenceUtils.optArrayCell(create, jvmTypeToCOL(create, arr.getComponentType))
    case cls => create class_type(cls.getCanonicalName.split('.'))
  }

  private def loadReflectively(name: Seq[String]): Option[ASTClass] = REFLECTION_CACHE.getOrElseUpdate(name, {
    try {
      val create = new ASTFactory[Unit]()
      create.enter()
      create.setOrigin(new MessageOrigin("Java system class"))

      // throws ClassNotFoundException
      val jvmClass = this.getClass.getClassLoader.loadClass(name.mkString("."))
      val `class` = create.new_class(
        jvmClass.getName,
        Array(),
        Option(jvmClass.getSuperclass).map(`super` => create.class_type(`super`.getCanonicalName.split('.'))).orNull
      )

      // Very limited: only import parameter-less constructors and the fields
      jvmClass.getConstructors.filter(_.getParameterCount == 0).foreach(cons => {
        `class`.add_dynamic(
          create.method_kind(
            Method.Kind.Constructor,
            create primitive_type PrimitiveSort.Void,
            null,
            jvmClass.getSimpleName,
            Array(),
            null
          )
        )
      })

      jvmClass.getFields.foreach(field => {
        `class`.add_dynamic(
          create.field_decl(field.getName, jvmTypeToCOL(create, field.getType))
        )
      })

      Some(`class`)
    } catch {
      case _: ClassNotFoundException => None
    }
  })

  private def loadByReflection(name: Seq[String], ns: Option[NameSpace]): Option[ASTClass] = {
    if(name.size == 1) {
      ns match {
        case Some(ns) =>
          potentialImportsOfNamespace(name.head, ns)
            .toStream.flatMap(loadReflectively).headOption
            .orElse(loadReflectively(Seq("java", "lang") :+ name.head))
        case _ => None
      }
    } else {
      loadReflectively(name)
    }
  }

  def load(name: Seq[String], ns: Option[NameSpace] = None): Option[ASTClass] = {
    loadByFile(name, ns)
      .orElse(loadByReflection(name, ns))
  }
}
