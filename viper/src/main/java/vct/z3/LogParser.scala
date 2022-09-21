package vct.z3

import java.io.{BufferedWriter, File, PrintWriter}

import hre.io.{NamedPipe, NamedPipeListener}
import hre.lang.System._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object LogParser {
  def main(args: Array[String]): Unit = {
    val parser = new LogParser
    val pipe = NamedPipe.create(parser, "vercors-z3-log-", "-01.log")
    println(s"Waiting for z3 to open ${pipe.path}")
    hre.lang.System.setErrorStream(System.err, hre.lang.System.LogLevel.Info)
    hre.lang.System.setErrorStream(System.out, hre.lang.System.LogLevel.Info)
  }
}

class LogParser extends NamedPipeListener {
  val log = new Z3Log
  val f = new PrintWriter("/home/bobe/z3.log")

  override def onNewLine(line: String): Unit = {
    f.println(line)
    f.flush()
    log.addLine(line)
  }
}

class Z3Log {
  sealed trait InstantiationCause

  sealed abstract class Expr extends InstantiationCause {
    def precedence: Int
    def asUnicode: String = asUnicodeImpl(Seq())
    def asUnicodeImpl(scope: Binder.VarInfo): String
    def asUnicodeParen(scope: Binder.VarInfo, maxPrecedence: Int, assoc: Boolean): String =
      if(precedence < maxPrecedence || (precedence == maxPrecedence && assoc))
        asUnicodeImpl(scope) else "(" + asUnicodeImpl(scope) + ")"

    override def toString: String = asUnicode

    var origin: Option[Origin] = None
    var descendants: ArrayBuffer[Expr] = ArrayBuffer()

    def children: Seq[Expr] = {
      this match {
        case App(_, args) => args
        case QuantVar(_) => Seq()
        case Quantifier(_, triggers, body) => triggers :+ body
        case Lambda(_, body) => Seq(body)
      }
    }

    def inheritFrom(other: Expr): Unit = {
      if(other == this) {
        return
      }

      other.origin match {
        case None =>
          val value = DelayedInherit(other)
          origin match {
            case None =>
              origin = Some(value)
              other.descendants += this
            case Some(oldOrigin) if oldOrigin.precedence < value.precedence =>
              origin = Some(value)
              other.descendants += this
            case _ =>
          }
        case Some(o) =>
          this match {
            case App(f, _) if f.contains("!") && f.contains("@") && o.isInstanceOf[Assertion] =>
              Warning("%s", this)
            case _ =>
          }

          origin match {
            case None =>
              origin = Some(o)
            case Some(oldOrigin) if oldOrigin.precedence < o.precedence =>
              origin = Some(o)
            case _ =>
          }
      }

      children.foreach(_.inheritFrom(other))
    }

    def setOrigin(newOrigin: Origin): Unit = {
      this match {
        case App(f, _) if f.contains("!") && f.contains("@") && newOrigin.isInstanceOf[Assertion] =>
          Warning("%s", this)
        case _ =>
      }

      origin match {
        case None =>
          origin = Some(newOrigin)
        case Some(oldOrigin) if oldOrigin.precedence < newOrigin.precedence =>
          origin = Some(newOrigin)
        case _ =>
      }

      val desc = descendants
      descendants = ArrayBuffer()
      desc.foreach(e => e.origin match {
        case Some(inh@DelayedInherit(_)) if inh.expr == this => e.setOrigin(origin.get)
        case _ =>
      })

      children.foreach(_.setOrigin(newOrigin))
    }
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
      "pattern" -> CustomOp(0, (args, scope) => {
        "{" + args.map(_.asUnicodeImpl(scope)).mkString(", ") + "}"
      })
    )

    val SUBSCRIPTS: Map[Char, Char] = "0123456789".zip("₀₁₂₃₄₅₆₇₈₉").toMap

    def niceName(x: String): String = {
      "^([^@!]+)@([0-9]+)@[0-9]+(![0-9]+)?$".r.findFirstMatchIn(x) match {
        case Some(m) =>
          var result = m.group(1)
          if(m.group(3) != null) {
            result += m.group(3)
          }
          result += m.group(2).map(SUBSCRIPTS)
          result
        case None => x
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
      val (name, _) = if(index < scope.size) scope(index) else (None, None)
      App.niceName(name.getOrElse(s"?$index?"))
    }
  }

  object Binder {
    type VarInfo = Seq[(Option[String], Option[String])]
  }

  abstract sealed class Binder extends Expr {
    var var_desc: Binder.VarInfo = (0 until var_count).map(_ => (None, None))
    def var_count: Int
    def body: Expr

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

  case class Substitution(from: Expr, to: Expr) extends InstantiationCause {
    override def toString: String = s"Substitution($from = $to)"
  }

  sealed trait Instantiation
  case class MatchInstantiation(quantifier: Quantifier, bindings: Seq[Expr])(val trigger: Expr, val causes: Seq[InstantiationCause], val triggerId: String) extends Instantiation
  case class TheoryInstantiation(theory: String, quantifier: Option[Quantifier], bindings: Seq[Expr])(val causes: Seq[InstantiationCause]) extends Instantiation

  sealed abstract class Origin(val precedence: Int)
  case class Assertion(proof: Proof) extends Origin(100)
  case class Enode(inst: Instantiation) extends Origin(70)
  case class ProofOrigin(proof: Proof) extends Origin(70)
  case class DelayedInherit(expr: Expr) extends Origin(50)

  var toolVersion: Option[String] = None

  var defs: mutable.Map[String, Expr] = mutable.Map()
  var proofs: mutable.Map[String, Proof] = mutable.Map()
  var discoveredInstantiations: mutable.Map[String, Instantiation] = mutable.Map()
  var instantiations: mutable.Map[String, ArrayBuffer[Instantiation]] = mutable.Map()
  var instanceStack: mutable.ArrayStack[Instantiation] = mutable.ArrayStack()
  var explainedSubstitution: mutable.Map[Expr, Expr] = mutable.Map()

  def readLine(text: String): Seq[String] = {
    val sb = new StringBuilder()
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
    val appArgs = args.drop(2).map(defs(_))
    defs += args(0) -> App(args(1), appArgs)
  }

  def makeVar(args: Seq[String]): Unit = {
    defs += args(0) -> QuantVar(Integer.parseInt(args(1)))
  }

  def makeQuant(args: Seq[String]): Unit = {
    val triggers = args.drop(3).init.map(defs(_))
    val body = defs(args.last)
    val quantifier = Quantifier(var_count=Integer.parseInt(args(2)), triggers, body)(name=args(1))
    defs += args(0) -> quantifier
  }

  val ALLOW_OPS: Map[String, Set[String]] = Map(
    "refl" -> Set("=", "~"),
    "nnf-pos" -> Set("~"),
    "nnf-neg" -> Set("~"),
    "sk" -> Set("~"),
    "rewrite" -> Set("="),
    "elim-unused" -> Set("="),
    "quant-intro" -> Set("="),
    "monotonicity" -> Set("=", "~"),
    "commutativity" -> Set("="),
    "trans" -> Set("="),
    "trans*" -> Set("="),
    "iff-true" -> Set("="),
    "iff-false" -> Set("="),
  )

  def makeProof(args: Seq[String]): Unit = {
    // The proof ID in an expression refers to the result
    val result = defs(args.last)
    defs += args(0) -> result
    val rule = args(1)
    val proof = Proof(args.drop(2).init.map(proofs(_)), rule, result)
    proofs += args(0) -> proof

    /*
      Nieuw plan:
      - eeh equality is al dan niet een simplifying equality
      - een rewrite bouwt altijd een sipmlifying equality
      - monotonicity bouwt een simplifying equality als de prereqs simplifying zijn
      - combining rules (trans, trans*, mp) bouwen een simplifying equality als de prereqs simplifying zijn
      - combining rules zorgen voor inheritance in simplifying proof-bomen als 1 proof-boom niet simplifying is
     */

    rule match {
      case "asserted" =>
        result.setOrigin(Assertion(proof))
      case "refl" | "rewrite" | "elim-unused" | "quant-intro" | "commutativity" | "iff-true" | "iff-false" | "nnf-pos" | "nnf-neg" =>
        assert(result.isInstanceOf[App])
        val app = result.asInstanceOf[App]
        assert(ALLOW_OPS(rule).contains(app.func))
        assert(app.args.size == 2)
        result.inheritFrom(app.args(0))
      case "proof-bind" =>
        assert(result.isInstanceOf[Lambda])
        val lambda = result.asInstanceOf[Lambda]
        result.inheritFrom(lambda.body)
      case "symm" | "and-elim" | "not-or-elim" =>
        assert(proof.prereqs.size == 1)
        result.inheritFrom(proof.prereqs(0).result)
      case "mp" | "mp~" | "quant-inst" | "hypothesis" | "th-lemma" | "lemma" | "unit-resolution" | "sk" =>
      case "true-axiom" | "def-axiom" | "monotonicity" | "trans*" | "trans" =>
        result.setOrigin(ProofOrigin(proof))
      case _ =>
        result.setOrigin(ProofOrigin(proof))
    }
  }

  def makeLambda(args: Seq[String]): Unit = {
    // args(1) is always "null", not sure why. Maybe an unused name?
    val body = defs(args(3))
    defs += args(0) -> Lambda(Integer.parseInt(args(2)), body)
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

  def getPair(arg: String): (Expr, Expr) = {
    val args = arg.init.tail.split(" ")
    (defs(args(0)), defs(args(1)))
  }

  def getInstantiationCause(arg: String): InstantiationCause = {
    arg.charAt(0) match {
      case '#' => defs(arg)
      case '(' =>
        val pair = getPair(arg)
        Substitution(pair._1, pair._2)
    }
  }

  def newMatch(args: Seq[String]): Unit = {
    val id = args(0)
    val quant = defs(args(1)).asInstanceOf[Quantifier]
    val trigger = defs(args(2))
    val sepIndex = args.indexOf(";")
    val bindings = args.slice(3, sepIndex).map(defs(_))
    val causes = args.drop(sepIndex+1).map(getInstantiationCause)
    val instId = (args(1), args(2))
    discoveredInstantiations += id -> MatchInstantiation(quant, bindings)(trigger, causes, args(2))
  }

  def newInst(args: Seq[String]): Unit = {
    val theory = args(0)
    val id = args(1)
    val sepIndex = args.indexOf(";")
    val bindings = args.slice(3, sepIndex).map(defs(_))
    val causes = args.drop(sepIndex+1).map(getInstantiationCause)

    if(bindings.nonEmpty) {
      val quant = defs(args(2)).asInstanceOf[Quantifier]
      discoveredInstantiations += id -> TheoryInstantiation(theory, Some(quant), bindings)(causes)
    } else {
      discoveredInstantiations += id -> TheoryInstantiation(theory, None, bindings)(causes)
    }
  }

  def printDetailedInstantiation(inst: MatchInstantiation): Unit = {
    Progress("%d instantiations: %s", Int.box(instantiations(inst.triggerId).size), inst.quantifier.name)
    Progress("with trigger: %s", inst.trigger.asUnicodeImpl(inst.quantifier.var_desc))
    Progress("∀%s :: ", inst.quantifier.bindingsAsUnicode)
    Progress("%s", inst.quantifier.body.asUnicodeImpl(inst.quantifier.var_desc))
    Progress("The last bindings were:")
    for((binding, i) <- inst.bindings.zipWithIndex) {
      Progress("%s := %s", QuantVar(i).asUnicodeImpl(inst.quantifier.var_desc), binding.asUnicode)
    }
    Progress("The expressions that caused the instantiation were:")
    for(cause <- inst.causes) {
      Progress("%s", cause)
    }
    Progress("")
  }

//  def collectPathsTo(goal: Quantifier, cause: Expr, maxDepth: Int): Seq[Seq[(InstantiationCause, Instantiation)]] = {
//    cause.origins.flatMap {
//      case Enode(inst@MatchInstantiation(_, _)) =>
//        findPathsTo(goal, inst, maxDepth - 1).map((cause, inst) +: _)
//      case _ => Seq()
//    } ++ (if(explainedSubstitution.contains(cause) && explainedSubstitution(cause) != cause) {
//      collectPathsTo(goal, explainedSubstitution(cause), maxDepth)
//    } else Seq())
//  }
//
//  def findPathsTo(goal: Quantifier, blame: MatchInstantiation, maxDepth: Int): Seq[Seq[(InstantiationCause, Instantiation)]] = {
//    if(maxDepth == 0) {
//      return if(goal == blame.quantifier) Seq(Seq()) else Seq()
//    }
//
//    blame.causes.flatMap {
//      case e: Expr => collectPathsTo(goal, e, maxDepth)
//      case Substitution(from, to) =>
//        collectPathsTo(goal, from, maxDepth) ++ collectPathsTo(goal, to, maxDepth)
//    } ++ (if(goal == blame.quantifier) Seq(Seq()) else Seq())
//  }
//
//  def tryFindCycles(instantiation: MatchInstantiation): Unit = {
//    val paths = findPathsTo(instantiation.quantifier, instantiation, 2)
//
//    if(paths.size > 1) {
//      Output("%s", paths)
//    }
//  }

  var lastInstanceDebug: Instantiation = _

  def startInstance(args: Seq[String]): Unit = {
    val id = args(0)
    val term = defs(args(1))
    val genericInst = discoveredInstantiations(id)
    instanceStack.push(genericInst)
    lastInstanceDebug = genericInst

    if(id == "0") return
    val inst = genericInst.asInstanceOf[MatchInstantiation]

    instantiations.getOrElseUpdate(inst.triggerId, ArrayBuffer()) += inst

    term match {
      case App("or", Seq(
        App("not", Seq(Quantifier(_, _, _))),
        body)) =>
        body.setOrigin(Enode(instanceStack.top))
      case App("=", Seq(
        body,
        App("true", Seq()))) =>
        body.setOrigin(Enode(instanceStack.top))
    }

    if(instantiations(inst.triggerId).size % 30000 == 0) {
      Output("looking for cycles in %s", inst.quantifier)

      val instArrows: mutable.Map[(Quantifier, Quantifier), ArrayBuffer[(MatchInstantiation, MatchInstantiation)]] = mutable.Map()
      val assertArrows: mutable.Map[Quantifier, Int] = mutable.Map()

      for(inst <- instantiations.values.flatten.filter(_.isInstanceOf[MatchInstantiation]).map(_.asInstanceOf[MatchInstantiation])) {
        inst.causes.foreach {
          case e: Expr => e.origin match {
            case Some(Enode(causingInst: MatchInstantiation)) =>
              instArrows.getOrElseUpdate((causingInst.quantifier, inst.quantifier), ArrayBuffer()).+=((causingInst, inst))
            case Some(Assertion(_)) =>
              assertArrows(inst.quantifier) = assertArrows.getOrElse(inst.quantifier, 0) + 1
            case _ =>
              Warning("Don't understand origin :(")
          }
          case _ =>
        }
      }

      val graphQuantifiers = instArrows.keys.flatMap(k => Seq(k._1, k._2)).toSeq ++ assertArrows.keys
      val graphKeys = graphQuantifiers.distinct.zipWithIndex.toMap

      val writer = new PrintWriter(new File("/home/pieter/z3.dot"))
      writer.write("digraph G {\n")

      writer.write("assertion;\n")

      for((quant, key) <- graphKeys) {
        Output("%s: %s", Int.box(key), quant)
        writer.write(s"n$key;\n")
      }

      for((quant, count) <- assertArrows) {
        val style = if(count < 50) { "dotted"
        } else if(count < 100) { "dashed"
        } else if(count < 500) { "solid" } else { "bold" }
        writer.write(s"assertion -> n${graphKeys(quant)} [label=$count,style=$style];\n")
      }

      for((cause, result) <- instArrows.keys) {
        val label = instArrows((cause, result)).size
        val style = if(label < 50) { "dotted"
          } else if(label < 100) { "dashed"
          } else if(label < 500) { "solid" } else { "bold" }
        writer.write(s"n${graphKeys(cause)} -> n${graphKeys(result)} [label=$label,style=$style];\n")
      }

      writer.write("}\n")
      writer.close()
      Output("End of legend")
    }
  }

  def endInstance(args: Seq[String]): Unit = {
    instanceStack.pop()
  }

  def attachEnode(args: Seq[String]): Unit = {
    val term = defs(args(0))
    if(instanceStack.isEmpty) {
    } else {
      term.setOrigin(Enode(instanceStack.top))
    }
  }

  def explainSubtitutionEquality(args: Seq[String]): Unit = {
    val explainedTerm = defs(args(0))
    val reason = args(1)

    if(args.size > 2) {
      val otherTerm = defs(args.last)
//      explainedSubstitution += explainedTerm -> otherTerm
    } else {
//      explainedSubstitution += explainedTerm -> explainedTerm
    }

//    Output("Explaining subtitution of %s", explainedTerm)
//
//    reason match {
//      case "root" =>
//        Output("It is the root of its equivalence class")
//      case "lit" =>
//        val equality = defs(args(2))
//        assert(args(3) == ";")
//        val other = defs(args(4))
//        Output("Due to this equality:")
//        Output("%s", equality)
//        Output("The term is equal to %s", other)
//      case "cg" =>
//        val equalities = args.drop(2).dropRight(2).map(getPair).map((eq) => App("=", Seq(eq._1, eq._2)))
//        val other = defs(args.last)
//        Output("We have:")
//        for(equality <- equalities) {
//          Output("%s", equality)
//        }
//        Output("By monotonicity, the term is equal to %s", other)
//    }
//
//    Output("")
  }

  def addLine(line: String): Unit = {
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

      case "[inst-discovered]" =>
        newInst(vals.tail)

      case "[instance]" => startInstance(vals.tail)
      case "[end-of-instance]" => endInstance(vals.tail)

      case "[attach-enode]" => attachEnode(vals.tail)

      case "[attach-meaning]" =>
        defs(vals(1)).asInstanceOf[App].func = vals(3)

      case "[attach-var-names]" =>
        attachVarNames(vals.tail)

      case "[eq-expl]" =>
        explainSubtitutionEquality(vals.tail)

      case "[begin-check]" =>
        Output("new check! (cleared out %d instantiations)", Int.box(instantiations.size))
        discoveredInstantiations.clear
        instantiations.clear

      case other =>
//        Warning("Kind %s: %s", other, vals.tail)
    }
  }
}
