package vct.z3

import java.io.PrintWriter

import hre.io.NamedPipeListener
import hre.lang.System._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class LogParser extends NamedPipeListener {
  val log = new Z3Log
  val f = new PrintWriter("/home/pieter/z3.log")

  override def onNewLine(line: String): Unit = {
    f.println(line)
    f.flush()
    log.addLine(line)
  }
}

class Z3Log {
  sealed trait Expr {
    def precedence: Int
    def asUnicode: String = asUnicodeImpl(Seq())
    def asUnicodeImpl(scope: Binder.VarInfo): String
    def asUnicodeParen(scope: Binder.VarInfo, maxPrecedence: Int, assoc: Boolean): String =
      if(precedence < maxPrecedence || (precedence == maxPrecedence && assoc))
        asUnicodeImpl(scope) else "(" + asUnicodeImpl(scope) + ")"
  }

  object App {
    val OPS: Map[String, Op] = Map(
      "~" -> BinaryOp(leftAssoc = false, rightAssoc = false, 80, "~sat~"),
      "=>" -> BinaryOp(leftAssoc = false, rightAssoc = true, 60, "⇒"),
      "if" -> CustomOp(55, (args, scope) => {
        if(args.size == 3) {
          args(0).asUnicodeParen(scope, 55, assoc = false) + " ? " +
            args(1).asUnicodeParen(scope, 55, assoc = false) + " : " +
            args(2).asUnicodeParen(scope, 55, assoc = true)
        } else {
          ???
        }
      }),
      "and" -> BinaryOp(leftAssoc = true, rightAssoc = true, 50, "⋀"),
      "or" -> BinaryOp(leftAssoc = true, rightAssoc = true, 40, "⋁"),
      "=" -> BinaryOp(leftAssoc = false, rightAssoc = false, 30, "="),
      "Set_equal" -> BinaryOp(leftAssoc = false, rightAssoc = false, 30, "="),
      "Set_subset" -> BinaryOp(leftAssoc = false, rightAssoc = false, 28, "⊂"),
      "Set_in" -> BinaryOp(leftAssoc = false, rightAssoc = false, 28, "∈"),
      ">" -> BinaryOp(leftAssoc = false, rightAssoc = false, 25, ">"),
      ">=" -> BinaryOp(leftAssoc = false, rightAssoc = false, 25, "≥"),
      "<" -> BinaryOp(leftAssoc = false, rightAssoc = false, 25, "<"),
      "<=" -> BinaryOp(leftAssoc = false, rightAssoc = false, 25, "≤"),
      "Set_intersection" -> BinaryOp(leftAssoc = true, rightAssoc = false, 23, "∩"),
      "Set_union" -> BinaryOp(leftAssoc = true, rightAssoc = false, 23, "∪"),
      "Set_difference" -> BinaryOp(leftAssoc = true, rightAssoc = false, 23, "\\"),
      "+" -> BinaryOp(leftAssoc = true, rightAssoc = true, 20, "+"),
      "-" -> BinaryOp(leftAssoc = true, rightAssoc = false, 20, "-"),
      "*" -> BinaryOp(leftAssoc = true, rightAssoc = true, 15, "⋅"),
      "/" -> BinaryOp(leftAssoc = true, rightAssoc = false, 15, "/"),
      "not" -> UnaryOp("¬"),
      "true" -> ConstOp("⊤"),
      "false" -> ConstOp("⊥"),
      "pi" -> ConstOp("π"),
      "euler" -> ConstOp("e"),
      "Set_empty" -> ConstOp("∅"),
    )

    val SUBSCRIPTS: Map[Char, Char] = "0123456789".zip("₀₁₂₃₄₅₆₇₈₉").toMap

    def niceName(x: String): String = {
      val parts = x.split("@")
      if(parts.length == 3) {
        parts(0) + Integer.parseInt(parts(1)).toString.map(App.SUBSCRIPTS)
      } else {
        x
      }
    }
  }

  case class App(var func: String, args: Seq[Expr]) extends Expr {
    override def precedence: Int = App.OPS.get(func) match {
      case Some(op) => op.precedence
      case None => 0
    }

    override def asUnicodeImpl(scope: Binder.VarInfo): String = App.OPS.get(func) match {
      case Some(op) => op.asUnicode(args, scope)
      case None => if (args.isEmpty) {
        App.niceName(func)
      } else {
        App.niceName(func) + "(" + args.map(_.asUnicodeImpl(scope)).mkString(", ") + ")"
      }
    }
  }

  case class QuantVar(index: Int) extends Expr {
    override def precedence: Int = 0
    override def asUnicodeImpl(scope: Binder.VarInfo): String = {
      val (name, _) = scope(index)
      App.niceName(name.getOrElse(s"?$index?"))
    }
  }

  object Binder {
    type VarInfo = Seq[(Option[String], Option[String])]
  }

  abstract sealed class Binder extends Expr {
    def var_count: Int
    def body: Expr
    var var_desc: Binder.VarInfo = (0 until var_count).map(_ => (None, None))

    def bindingsAsUnicode: String = {
      var_desc.zipWithIndex.map {
        case ((name, t), index) =>
          val nameText = name.map(App.niceName).getOrElse(s"?$index?")
          val typeText = t.getOrElse("?")
          s"$nameText: $typeText"
      }.mkString(", ")
    }
  }

  case class Quantifier(var_count: Int, triggers: Seq[Expr], body: Expr)(var name: String) extends Binder {
    override def precedence: Int = 70
    override def asUnicodeImpl(scope: Binder.VarInfo): String = {
      "∀" + bindingsAsUnicode + " " + triggers.map(_.asUnicodeImpl(var_desc)).mkString(" ") +
        " :: " + body.asUnicodeParen(var_desc, precedence, assoc = false)
    }
  }

  case class Lambda(var_count: Int, body: Expr) extends Binder {
    override def precedence: Int = 70
    override def asUnicodeImpl(scope: Binder.VarInfo): String = {
      "λ" + bindingsAsUnicode + " . " + body.asUnicodeParen(var_desc, precedence, assoc = false)
    }
  }

  trait Op {
    def precedence: Int
    def asUnicode(args: Seq[Expr], scope: Binder.VarInfo): String
  }

  case class BinaryOp(leftAssoc: Boolean, rightAssoc: Boolean, precedence: Int, text: String) extends Op {
    override def asUnicode(args: Seq[Expr], scope: Binder.VarInfo): String = {
      if(leftAssoc && rightAssoc) {
        args.map(_.asUnicodeParen(scope, precedence, assoc=true)).mkString(" " + text + " ")
      } else if(args.size == 2) {
        args(0).asUnicodeParen(scope, precedence, leftAssoc) +
          " " + text + " " + args(1).asUnicodeParen(scope, precedence, rightAssoc)
      } else {
        ???
      }
    }
  }

  case class UnaryOp(text: String) extends Op {
    override def precedence: Int = 10
    override def asUnicode(args: Seq[Expr], scope: Binder.VarInfo): String = args match {
      case Seq(arg) => text + arg.asUnicodeParen(scope, precedence, assoc=true)
      case _ => ???
    }
  }

  case class ConstOp(text: String) extends Op {
    override def precedence: Int = 0
    override def asUnicode(args: Seq[Expr], scope: Binder.VarInfo): String = args match {
      case Seq() => text
      case _ => ???
    }
  }

  case class CustomOp(precedence: Int, f: (Seq[Expr], Binder.VarInfo) => String) extends Op {
    override def asUnicode(args: Seq[Expr], scope: Binder.VarInfo): String = f(args, scope)
  }

  case class Proof(prereqs: Seq[Proof], rule: String, result: Expr)

  var toolVersion: Option[String] = None

  var defs: mutable.Map[String, Expr] = mutable.Map()
  var proofs: mutable.Map[String, Proof] = mutable.Map()
  var instantiations: mutable.Map[String, Int] = mutable.Map()

  def readLine(text: String): Seq[String] = {
    var sb = new StringBuilder()
    val elements = new ArrayBuffer[String]()
    var stack = 0

    text.replace("<no position>", "").foreach {
      case ' ' if stack == 0 =>
        elements += sb.toString
        sb.clear
      case '(' =>
        stack += 1
        sb.append('(')
      case ')' =>
        stack -= 1
        sb.append(')')
      case other => sb.append(other)
    }

    elements += sb.toString
    elements
  }

  def makeApp(args: Seq[String]): Unit = {
    defs += args(0) -> App(args(1), args.drop(2).map(defs(_)))
  }

  def makeVar(args: Seq[String]): Unit = {
    defs += args(0) -> QuantVar(Integer.parseInt(args(1)))
  }

  def makeQuant(args: Seq[String]): Unit = {
    val quantifier = Quantifier(
      var_count=Integer.parseInt(args(2)),
      triggers=args.drop(3).init.map(defs(_)),
      body=defs(args.last))(name=args(1))
    defs += args(0) -> quantifier
  }

  def makeProof(args: Seq[String]): Unit = {
    // The proof ID in an expression refers to the result
    defs += args(0) -> defs(args.last)
    proofs += args(0) -> Proof(args.drop(2).init.map(proofs(_)), args(1), defs(args.last))
  }

  def makeLambda(args: Seq[String]): Unit = {
    // args(1) is always "null", not sure why. Maybe an unused name?
    defs += args(0) -> Lambda(Integer.parseInt(args(2)), defs(args(3)))
  }

  def parseVarDescPart(part: String): Option[String] = {
    val stripPart = part.strip
    if(stripPart.isEmpty) {
      None
    } else if(stripPart.startsWith("|") && stripPart.endsWith("|")) {
      Some(stripPart.init.tail)
    } else {
      Some(stripPart)
    }
  }

  def parseVarDesc(varDesc: String): (Option[String], Option[String]) = {
    assert(varDesc.startsWith("("))
    assert(varDesc.endsWith(")"))
    val parts = varDesc.tail.init.split(";")
    assert(parts.size == 2)
    val name = parts(0)
    val `type` = parts(1)

    (parseVarDescPart(name), parseVarDescPart(`type`))
  }

  def attachVarNames(args: Seq[String]): Unit = {
    val quantifier = defs(args.head).asInstanceOf[Binder]
    assert(args.size-1 == quantifier.var_count)
    quantifier.var_desc = args.tail.map(parseVarDesc)
  }

  def newMatch(args: Seq[String]): Unit = {
    val id = args(0)
    val quant = defs(args(1))
    val trigger = defs(args(2))
    val sepIndex = args.indexOf(";")
    val bindings = args.slice(3, sepIndex).map(defs(_))
    instantiations(args(1)) = instantiations.getOrElse(args(1), 0) + 1
    if(instantiations(args(1)) % 10000 == 0) {
      Output("%d instantiations: %s", Int.box(instantiations(args(1))), quant.asUnicode)
    }
  }

  def addLine(line: String): Unit = {
    this.synchronized {
      val vals = readLine(line)
      vals.head match {
        case "[tool-version]" => toolVersion match {
          case None => toolVersion = Some(vals(2))
          case Some(other) => assert(false)
        }
        case "[push]" | "[pop]" => // push/pop doesn't match defs, but probably the input push/pop
        case "[assign]" | "" => // assign is followed by lines starting with a space

        case "[mk-app]" => makeApp(vals.tail)
        case "[mk-var]" => makeVar(vals.tail)
        case "[mk-quant]" => makeQuant(vals.tail)
        case "[mk-proof]" => makeProof(vals.tail)
        case "[mk-lambda]" => makeLambda(vals.tail)

        case "[new-match]" =>
          newMatch(vals.tail)

        case "[attach-meaning]" =>
          defs(vals(1)).asInstanceOf[App].func = vals(3)

        case "[attach-var-names]" =>
          attachVarNames(vals.tail)

        case other =>
//           Output("Kind %s: %s", other, vals.tail)
      }
    }
  }
}
