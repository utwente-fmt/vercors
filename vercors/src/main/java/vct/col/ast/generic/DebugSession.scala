package vct.col.ast.generic

import java.lang.reflect.Field
import java.util

import hre.lang.System.Output

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import scala.collection.JavaConverters._

case class DebugSession() {
  val seen: mutable.HashSet[AnyRef] = mutable.HashSet()

  private def indent_str(indent: Int): String = {
    var line = ""

    for(_ <- 0 until indent) {
      line += "  "
    }

    line
  }

  private def getReflectionField(obj: AnyRef, fieldName: String): Field = {
    var cls: Class[_] = obj.getClass

    while(cls != null) {
      try {
        return cls.getDeclaredField(fieldName)
      } catch {
        case e: NoSuchFieldException =>
          cls = cls.getSuperclass
      }
    }

    null
  }

  private def getField(obj: AnyRef, fieldName: String): AnyRef = {
    val field = getReflectionField(obj, fieldName)
    field.setAccessible(true)
    field.get(obj)
  }

  private def shouldBeShortcut[T1, T2](obj: AnyRef): Boolean = obj match {
    case null => true
    case node: DebugNode => isOnlyShortcut(node)
    case s: String => true
    case map: util.HashMap[T1, T2] => map.isEmpty
    case arr: Array[T1] => arr.isEmpty
    case arr: util.ArrayList[T1] => arr.isEmpty
    case list: List[T1] => list.isEmpty
    case opt: Option[T1] => opt.isEmpty
    case map: util.Hashtable[T1, T2] => map.isEmpty
    case arr: ArrayBuffer[T1] => arr.isEmpty
    case _ => false
  }

  private def isOnlyShortcut(node: DebugNode): Boolean = {
    for(property <- node.debugTreeChildrenFields) {
      if(!shouldBeShortcut(getField(node, property))) {
        return false
      }
    }

    true
  }

  private def getShortcut[T1, T2](obj: AnyRef): String = obj match {
    case null => "null"
    case node: DebugNode if isOnlyShortcut(node) => "(" + firstLine(node) + ")"
    case s: String => s
    case map: util.HashMap[T1, T2] if map.isEmpty => "{}"
    case arr: Array[T1] if arr.isEmpty => "[]"
    case arr: util.ArrayList[T1] if arr.isEmpty => "[]"
    case list: List[T1] if list.isEmpty => "[]"
    case opt: Option[T1] if opt.isEmpty => "None"
    case map: util.Hashtable[T1, T2] if map.isEmpty => "{}"
    case arr: ArrayBuffer[T1] if arr.isEmpty => "[]"
    case _ => null
  }

  private def dump[T1, T2](indent: Int, obj: Object, prefix: String): Unit = {
    obj match {
      case node: DebugNode =>
        dumpNode(indent, node, prefix)
      case map: util.Map[T1, T2] =>
        Output("%s", indent_str(indent) + prefix + "Map")
        for (entry <- map.entrySet().asScala) {
          dump(indent + 1, entry.getValue.asInstanceOf[Object], "- " + entry.getKey.toString + ": ")
        }
      case arr: Array[T1] =>
        Output("%s", indent_str(indent) + prefix + "List")
        for (value <- arr) {
          dump(indent + 1, value.asInstanceOf[Object], "- ")
        }
      case arr: util.ArrayList[T1] =>
        Output("%s", indent_str(indent) + prefix + "List")
        for (value <- arr.asScala) {
          dump(indent + 1, value.asInstanceOf[Object], "- ")
        }
      case arr: ArrayBuffer[T1] =>
        Output("%s", indent_str(indent) + prefix + "List")
        for (value <- arr) {
          dump(indent + 1, value.asInstanceOf[Object], "- ")
        }
      case list: List[T1] =>
        Output("%s", indent_str(indent) + prefix + "List")
        for (value <- list) {
          dump(indent + 1, value.asInstanceOf[Object], "- ")
        }
      case opt: Option[T1] =>
        dump(indent, opt.get.asInstanceOf[Object], prefix)
      case s: String =>
        Output("%s'%s'", indent_str(indent) + prefix, s)
      case _ =>
        Output("%s", indent_str(indent) + prefix + obj.getClass.getSimpleName + "???")
    }
  }

  private def firstLine(obj: DebugNode): String = {
    var firstLine = obj.getClass.getSimpleName

    for(property <- obj.debugTreePropertyFields) {
      firstLine += " " + property + "="
      val value = getField(obj, property)
      if(value == null) {
        firstLine += "null"
      } else {
        firstLine += value.toString.replace("\n", "\\n")
      }
    }

    for(property <- obj.debugTreeChildrenFields) {
      val value = getField(obj, property)
      if(shouldBeShortcut(value)) {
        firstLine += " " + property + "="
        firstLine += getShortcut(value)
      }
    }

    firstLine
  }

  def dumpNode(indent: Int, obj: DebugNode, prefix: String = ""): Unit = {
    if(seen.contains(obj)) {
      Output(indent_str(indent) + prefix + "(cycle: " + obj.getClass.getSimpleName + ")")
      return
    }

    seen.add(obj)

    Output(indent_str(indent) + prefix + firstLine(obj))

    for(property <- obj.debugTreeChildrenFields) {
      val value = getField(obj, property)
      if(!shouldBeShortcut(value)) {
        dump(indent+1, value, property+": ")
      }
    }

    seen.remove(obj)
  }
}
