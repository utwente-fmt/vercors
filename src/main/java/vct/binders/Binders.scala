package vct.binders

import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import vct.antlr4.generated.BinderParser._
import vct.antlr4.generated.BinderParserPatterns._
import vct.antlr4.generated.{BinderLexer, BinderParser}

import scala.collection.mutable

sealed trait Node extends Traversable[Node] {
  override def foreach[U](f: Node => U): Unit = {
    val children = this match {
      case _: Leaf => Seq()
      case Dereference(obj, _) => Seq(obj)
      case SequenceSubscript(seq, idx) => Seq(seq, idx)
      case Apply(method, args) => Seq(method) ++ args
      case Scale(scale, expr) => Seq(scale, expr)
      case MemberOf(x, xs) => Seq(x, xs)
      case Size(x) => Seq(x)
      case Negate(x) => Seq(x)
      case Not(x) => Seq(x)
      case ITE(cond, whenTrue, whenFalse) => Seq(cond, whenTrue, whenFalse)
      case Forall(bindings, trigger, body) =>
        bindings.map(_._1) ++ trigger.toSeq.flatten :+ body
      case Exists(bindings, trigger, body) =>
        bindings.map(_._1) ++ trigger.toSeq.flatten :+ body
      case op: ArithmeticOp => op match {
        case Mul(x, y) => Seq(x, y)
        case Div(x, y) => Seq(x, y)
        case FlooringDiv(x, y) => Seq(x, y)
        case Mod(x, y) => Seq(x, y)
        case Plus(x, y) => Seq(x, y)
        case Minus(x, y) => Seq(x, y)
      }
      case op: RelationalOp => op match {
        case Less(x, y) => Seq(x, y)
        case LessEq(x, y) => Seq(x, y)
        case GreaterEq(x, y) => Seq(x, y)
        case Greater(x, y) => Seq(x, y)
        case Eq(x, y) => Seq(x, y)
        case NotEq(x, y) => Seq(x, y)
      }
      case op: BooleanOp => op match {
        case And(x, y) => Seq(x, y)
        case Or(x, y) => Seq(x, y)
        case Imp(x, y) => Seq(x, y)
      }
    }
    children.foreach(f)
  }

  def flatter: Seq[Node] = {
    this +: this.flatMap(_.flatter).toSeq
  }

  def mentions: Set[String] = this.flatMap(_.mentions).toSet

  def mayOccurInPattern: Boolean =
    this.isInstanceOf[MayOccurInPattern] && this.forall(_.mayOccurInPattern)

  def isValidPattern: Boolean = this match {
    case _: Leaf => false
    case other => other.mayOccurInPattern
  }

  def contains(other: Node): Boolean =
    this == other || exists(_.contains(other))
}

sealed trait MayOccurInPattern

sealed trait Leaf extends Node

sealed trait Constant extends Leaf

case class ConstantInt(i: Int) extends Constant {
  override def toString = i.toString
}
case class ConstantSeq(args: Seq[Node]) extends Constant {
  override def toString = "seq<>{" + args.map(_.toString).mkString(", ") + "}"
}

case class Name(name: String) extends Leaf with MayOccurInPattern {
  override def mentions: Set[String] = Set(name)
  override def toString = name
}

object Type {
  def fromName(name: String): Type = name match {
    case "List" => List
    case "VectorIndex<>" => VectorIndex
    case "VCTArray" => VCTArray
    case "VectorExpression" => VectorExpression
    case "boolean" => TBoolean
    case "Process<>" => Process
    case "VCTFloat<>" => VCTFloat
    case "TYPE<>" => TypeADT
    case "T" => UnknownT
    case "frac<>" => Frac
    case "zfrac<>" => ZFrac
    case "int" => TInt
    case "VCTOption" => VCTOption
    case "MatrixExpression" => MatrixExpression
    case seq: String if seq.startsWith("seq<") && seq.endsWith(">") =>
      val element = seq.substring(4, seq.length - 1)
      Sequence(Type.fromName(element))
  }
}

sealed trait Type extends Leaf {
  override def toString: String = this.getClass.getSimpleName.replace("$", "")
}

case object TBoolean extends Type
case object TInt extends Type
case object UnknownT extends Type

sealed trait Domain extends Type
case object List extends Domain
case object VectorIndex extends Domain
case object VCTArray extends Domain
case object VectorExpression extends Domain
case object Process extends Domain
case object VCTFloat extends Domain
case object TypeADT extends Domain
case object VCTOption extends Domain
case object Frac extends Domain
case object ZFrac extends Domain
case object MatrixExpression extends Domain

case class Sequence(element: Type) extends Type

object Applicable {
  def fromNode(node: Node): Applicable = node match {
    case Name("tail") => Tail
    case Name("Perm") => Perm
    case Name("\\old") => Old
    case Dereference(Name("VCTArray"), "loc") => Loc
    case Dereference(Name("VCTArray"), "alen") => Len
    case Dereference(Name("VCTArray"), "first") => First
    case Dereference(Name("VCTArray"), "second") => Second
    case Name("getVCTOption1") => GetOpt
    case Name("getVCTOption2") => GetOpt
    case Dereference(Name("VCTOption"), "getVCTOption") => ADTGetOpt
    case Dereference(Name("Option"), "adt_get") => ADTGetOpt
    case Dereference(Name("VCTOption"), "VCTNone") => MNone
    case Dereference(Name("Option"), "adt_None") => MNone
    case Dereference(Name("VCTOption"), "VCTSome") => MSome
    case Dereference(Name("Option"), "adt_Some") => MSome
    case Name("new_frac") => NFrac
    case Dereference(Name("frac<>"), "frac_val") => FracVal
    case Dereference(Name("zfrac<>"), "zfrac_val") => ZFracVal
    case Dereference(Name("VCTFloat<>"), "zero") => FZero
    case Dereference(Name("VCTFloat<>"), "fadd") => FAdd
    case Dereference(Name("TYPE<>"), "instanceof") => InstanceOf
    case Dereference(Name("TYPE<>"), cls) => ADTClass(cls)
    case Dereference(Name("Process<>"), "p_merge") => Merge
    case Dereference(Name("Process<>"), "p_empty") => Empty
    case Dereference(Name("Process<>"), "p_seq") => PSeq
    case Dereference(Name("Process<>"), "p_is_choice") => IsChoice
    case Dereference(Name("Process<>"), "p_choice") => Choice
    case Dereference(Name("Process<>"), name) if name.startsWith("p_method_") => PMethod(name.substring("p_method_".length))
    case Name("hist_idle") => HistIdle
    case Dereference(Name("MatrixExpression"), "msum") => MSum
    case Dereference(Name("MatrixExpression"), "mcmp") => MCmp
    case Dereference(Name("MatrixExpression"), "mseq") => MSeq
    case Dereference(Name("MatrixExpression"), "mrep") => MRep
    case Dereference(Name("MatrixIndex<>"), "product") => MProduct
    case Dereference(Name("VectorIndex<>"), "vrange") => VRange
    case Dereference(Name("VectorExpression"), "vadd") => VAdd
    case Dereference(Name("VectorExpression"), "vsum") => VSum
    case Dereference(Name("VectorExpression"), "vseq") => VSeq
    case Dereference(Name("VectorExpression"), "vget") => VGet
    case Dereference(Name("VectorExpression"), "vrep") => VRep
    case Dereference(Name("VectorExpression"), "vcmp") => VRep
    case Dereference(Name("VectorExpression"), "vsize") => VSize
    case Dereference(Name("List"), "adt_length") => LLen
    case Dereference(Name("List"), "adt_cons") => LCons
    case Dereference(Name("List"), "adt_nil") => LNil
    case Name(other) => UnknownMethod(other)
    case _ => println(node); Perm
  }
}

sealed abstract class Applicable(name: String) extends Leaf {
  override def toString(): String = name
}

sealed abstract class TriggerableApplicable(name: String) extends Applicable(name) with MayOccurInPattern

case object Loc extends TriggerableApplicable("loc")
case object Len extends TriggerableApplicable("len")
case object First extends TriggerableApplicable("first")
case object Second extends TriggerableApplicable("second")

case object GetOpt extends TriggerableApplicable("getOpt")
case object ADTGetOpt extends TriggerableApplicable("ADTGetOpt")
case object MNone extends TriggerableApplicable("None")
case object MSome extends TriggerableApplicable("Some")

case object NFrac extends TriggerableApplicable("frac")
case object FracVal extends TriggerableApplicable("frac_val")
case object ZFracVal extends TriggerableApplicable("zfrac_val")

case object FZero extends TriggerableApplicable("f_zero")
case object FAdd extends TriggerableApplicable("f_add")

case class ADTClass(name: String) extends TriggerableApplicable(s"Type.$name")
case object InstanceOf extends TriggerableApplicable("Type.InstanceOf")

case object Merge extends TriggerableApplicable("p_merge")
case object Empty extends TriggerableApplicable("p_empty")
case object PSeq extends TriggerableApplicable("p_seq")
case object IsChoice extends TriggerableApplicable("p_is_choice")
case object Choice extends TriggerableApplicable("p_choice")
case class PMethod(name: String) extends TriggerableApplicable(s"p_method_$name")
case object HistIdle extends TriggerableApplicable("hist_idle")

case object MSum extends TriggerableApplicable("m_sum")
case object MCmp extends TriggerableApplicable("m_cmp")
case object MSeq extends TriggerableApplicable("m_seq")
case object MProduct extends TriggerableApplicable("m_product")
case object MRep extends TriggerableApplicable("m_rep")
case object VRange extends TriggerableApplicable("v_range")
case object VAdd extends TriggerableApplicable("v_add")
case object VSum extends TriggerableApplicable("v_sum")
case object VSeq extends TriggerableApplicable("v_seq")
case object VGet extends TriggerableApplicable("v_get")
case object VRep extends TriggerableApplicable("v_rep")
case object VCmp extends TriggerableApplicable("v_cmp")
case object VSize extends TriggerableApplicable("v_size")

case object LLen extends TriggerableApplicable("length")
case object LCons extends TriggerableApplicable("cons")
case object LNil extends TriggerableApplicable("nil")

case object Perm extends Applicable("Perm")
case object Old extends Applicable("\\old")
case object Tail extends Applicable("tail")

case class UnknownMethod(name: String) extends Applicable(name)

sealed trait Composite extends Node

sealed abstract class BinaryOp(val op: String) extends Composite {
  override def toString(): String = "(" + map(_.toString).mkString(f" $op ") + ")"
}

sealed abstract class CustomOp(val str: () => String) extends Composite {
  override def toString(): String = str()
}

case class Dereference(obj: Node, field: String) extends CustomOp(() => f"$obj.$field") with MayOccurInPattern
case class SequenceSubscript(sequence: Node, idx: Node) extends CustomOp(() => f"$sequence[$idx]") with MayOccurInPattern
case class Apply(method: Applicable, arguments: Seq[Node])
  extends CustomOp(() => f"$method(${arguments.map(_.toString).mkString(", ")})")
    with MayOccurInPattern
case class Scale(scale: Node, expr: Node) extends CustomOp(() => f"[$scale]$expr") with MayOccurInPattern
case class MemberOf(x: Node, xs: Node) extends BinaryOp("∈") with MayOccurInPattern
case class Size(x: Node) extends CustomOp(() => f"|$x|") with MayOccurInPattern
case class Negate(x: Node) extends CustomOp(() => f"-$x")
case class Not(x: Node) extends CustomOp(() => f"!$x")
case class ITE(cond: Node, whenTrue: Node, whenFalse: Node) extends CustomOp(() => f"($cond ? $whenTrue : $whenFalse)")

sealed abstract class ArithmeticOp(op: String) extends BinaryOp(op)
case class Mul(x: Node, y: Node) extends ArithmeticOp("*")
case class Div(x: Node, y: Node) extends ArithmeticOp("\\")
case class FlooringDiv(x: Node, y: Node) extends ArithmeticOp("/")
case class Mod(x: Node, y: Node) extends ArithmeticOp("%")
case class Plus(x: Node, y: Node) extends ArithmeticOp("+")
case class Minus(x: Node, y: Node) extends ArithmeticOp("-")

sealed abstract class RelationalOp(op: String, val left: Node, val right: Node) extends BinaryOp(op)
case class Less(override val left: Node, override val right: Node) extends RelationalOp("<", left, right)
case class LessEq(override val left: Node, override val right: Node) extends RelationalOp("<=", left, right)
case class GreaterEq(override val left: Node, override val right: Node) extends RelationalOp(">=", left, right)
case class Greater(override val left: Node, override val right: Node) extends RelationalOp(">", left, right)
case class Eq(override val left: Node, override val right: Node) extends RelationalOp("=", left, right)
case class NotEq(override val left: Node, override val right: Node) extends RelationalOp("!=", left, right)

sealed abstract class BooleanOp(op: String) extends BinaryOp(op)
case class And(x: Node, y: Node) extends BooleanOp("&&")
case class Or(x: Node, y: Node) extends BooleanOp("||")
case class Imp(x: Node, y: Node) extends BooleanOp("==>")

sealed trait Quantifier extends Composite

case class Forall(bindings: Seq[(Type, String)], trigger: Option[Seq[Node]], body: Node)
  extends Quantifier
{
  def collectPatterns: Map[Set[String], Set[Node]] = {
    val quant = bindings.map(_._2).toSet
    val map = mutable.Map[Set[String], mutable.Set[Node]]()
    for(node <- body.flatter) {
      if(node.isValidPattern) {
        val mentions = node.mentions.intersect(quant)
        if(mentions.nonEmpty) {
          map.getOrElseUpdate(mentions, mutable.Set()).add(node)
        }
      }
    }
    map.map(pair => pair._1 -> pair._2.toSet).toMap
  }

  def powerset[T](x: Set[T]): Seq[Set[T]] = {
    if(x.isEmpty) {
      Seq(Set())
    } else {
      val tail = powerset(x.tail)
      tail ++ tail.map(_ + x.head)
    }
  }

  def wouldLoop(trigger: Set[Node]): Boolean = {
    false
  }

  def getTriggers: Set[Set[Node]] = {
    val quant = bindings.map(_._2).toSet
    val patterns = collectPatterns.values.flatten.toSet
    val patternsDeref = patterns.filter {
      case Dereference(node, _) => !patterns.contains(node)
      case Size(node) => !patterns.contains(node)
      case Apply(Len, Seq(a)) => !patterns.contains(a)
      case _ => true
    }
    val triggers = powerset(patternsDeref)

    val result =
      triggers
        .filter(_.foldLeft(Set[String]())(_ ++ _.mentions).intersect(quant) == quant)
        .filter(set => set.forall(big => set.forall(small => big == small || !big.contains(small))))
        .filterNot(wouldLoop)

    result.toSet
  }

  def chooseTriggers: Option[Set[Set[Node]]] = {
    val triggers = getTriggers
    if(triggers.isEmpty) return None
    if(triggers.size == 1) return Some(Set(triggers.head))

    body match {
      case Imp(_, body) => body match {
        case op: RelationalOp =>
          val leftTriggers = triggers.intersect(op.left.flatter.map(Set(_)).toSet)
          val rightTriggers = triggers.intersect(op.right.flatter.map(Set(_)).toSet)
          if(leftTriggers.size == 1 && rightTriggers.size == 1) {
            Some(Set(leftTriggers.head, rightTriggers.head))
          } else if(leftTriggers.size == 1 && rightTriggers.isEmpty) {
            Some(Set(leftTriggers.head))
          } else if (rightTriggers.size == 1 && leftTriggers.isEmpty) {
            Some(Set(rightTriggers.head))
          } else {
            None
          }
        case _ => None
      }
      case _ => None
    }
  }

  override def toString(): String = {
    "∀ " + bindings.map((decl) => decl._1.toString + " " + decl._2).mkString(", ") + " . " + body.toString
  }
}

case class Exists(bindings: Seq[(Type, String)], trigger: Option[Seq[Node]], body: Node)
  extends Quantifier

case object Binders {
  def main(args: Array[String]): Unit = {
    val stream = CharStreams.fromFileName("binders.txt")
    val lexer = new BinderLexer(stream)
    val parser = new BinderParser(new CommonTokenStream(lexer))
    val binders = convert(parser.top()).distinct

    binders.zipWithIndex.foreach {
      case (f: Forall, i) =>
        f.chooseTriggers match {
          case None =>
            if(f.trigger.isEmpty) {
              println(s"Line $i: Cannot choose triggers")
              println(f)
              for (trigger <- f.getTriggers) {
                println(trigger.mkString("  ,  "))
              }
              println()
            }
          case Some(triggers) =>
            if(f.trigger.nonEmpty && Set(f.trigger.get.toSet) != triggers) {
              println(s"Line $i: Generated triggers differ from custom trigger")
              val origTrigger = f.trigger.get.mkString("  ,  ")
              println(s"Original trigger: $origTrigger")
              println(f)
              for (trigger <- triggers) {
                println(trigger.mkString("  ,  "))
              }
              println()
            }
        }
      case _ =>
    }
  }

  def convert(top: TopContext): Seq[Node] = top match {
    case Top0(lines, _) =>
      lines.map(convert)
  }

  def convert(line: LineContext): Node = line match {
    case Line0(_, expr, _) =>
      convert(expr)
  }

  def convert(maybeArgs: Option[ArgumentsContext]): Seq[Node] = maybeArgs match {
    case None => Seq()
    case Some(args) => convert(args)
  }

  def convert(args: ArgumentsContext): Seq[Node] = args match {
    case Arguments0(expr) => Seq(convert(expr))
    case Arguments1(expr, _, args) => convert(expr) +: convert(args)
  }

  def convert(decls: DeclsContext): Seq[(Type, String)] = decls match {
    case Decls0(decl) => Seq(convert(decl))
    case Decls1(decl, ",", decls) => convert(decl) +: convert(decls)
  }

  def convert(decl: DeclContext): (Type, String) = decl match {
    case Decl0(t, name) => (convert(t), convert(name))
  }

  def convert(t: TypeContext): Type = t match {
    case Type0(id) => Type.fromName(convert(id))
  }

  def convert(id: IdentifierContext): String = id match {
    case Identifier0(name) => name
  }

  def convert(trigger: TriggerContext): Seq[Node] = trigger match {
    case Trigger0(_, args, _) => convert(args)
  }

  def binary(left: ExprContext, op: String, right: ExprContext): Node = op match {
    case "*" => Mul(convert(left), convert(right))
    case "/" => Div(convert(left), convert(right))
    case "\\" => FlooringDiv(convert(left), convert(right))
    case "%" => Mod(convert(left), convert(right))
    case "+" => Plus(convert(left), convert(right))
    case "-" => Minus(convert(left), convert(right))
    case "<" => Less(convert(left), convert(right))
    case "<=" => LessEq(convert(left), convert(right))
    case ">=" => GreaterEq(convert(left), convert(right))
    case ">" => Greater(convert(left), convert(right))
    case "==" => Eq(convert(left), convert(right))
    case "!=" => NotEq(convert(left), convert(right))
    case "&&" => And(convert(left), convert(right))
    case "||" => Or(convert(left), convert(right))
    case "==>" => Imp(convert(left), convert(right))
  }

  def convert(expr: ExprContext): Node = expr match {
    case Expr0(int) =>
      ConstantInt(Integer.parseInt(int))
    case Expr1(name) =>
      Name(name)
    case Expr2("seq<int>", "{", args, "}") =>
      ConstantSeq(convert(args))
    case Expr3(obj, ".", field) =>
      Dereference(convert(obj), field)
    case Expr4(seq, "[", idx, "]") =>
      SequenceSubscript(convert(seq), convert(idx))
    case Expr5(method, "(", args, ")") =>
      Apply(Applicable.fromNode(convert(method)), convert(args))
    case Expr6("[", scale, "]", expr) =>
      Scale(convert(scale), convert(expr))
    case Expr7("(", "\\forall*", decls, maybeTrigger, ";", condition, ";", body, ")") =>
      Forall(convert(decls), maybeTrigger.map(convert), Imp(convert(condition), convert(body)))
    case Expr8("(", "\\forall", decls, maybeTrigger, ";", condition, ";", body, ")") =>
      Forall(convert(decls), maybeTrigger.map(convert), Imp(convert(condition), convert(body)))
    case Expr9("(", "\\exists", decls, maybeTrigger, ";", condition, ";", body, ")") =>
      Exists(convert(decls), maybeTrigger.map(convert), And(convert(condition), convert(body)))
    case Expr10("(", x, "\\memberof", xs, ")") =>
      MemberOf(convert(x), convert(xs))
    case Expr11("(", expr, ")") =>
      convert(expr)
    case Expr12("|", expr, "|") =>
      Size(convert(expr))
    case Expr13(unary, expr) => unary match {
      case "-" => Negate(convert(expr))
      case "!" => Not(convert(expr))
    }
    case Expr14(left, op, right) =>
      binary(left, op, right)
    case Expr15(left, op, right) =>
      binary(left, op, right)
    case Expr16(left, op, right) =>
      binary(left, op, right)
    case Expr17(left, op, right) =>
      binary(left, op, right)
    case Expr18(left, op, right) =>
      binary(left, op, right)
    case Expr19(left, op, right) =>
      binary(left, op, right)
    case Expr20(cond, "?", whenTrue, ":", whenFalse) =>
      ITE(convert(cond), convert(whenTrue), convert(whenFalse))
  }
}
