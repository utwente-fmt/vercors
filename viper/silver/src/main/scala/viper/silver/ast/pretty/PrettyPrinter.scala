package viper.silver.ast.pretty

import scala.language.implicitConversions
import scala.collection.immutable.Queue
import scala.collection.immutable.Queue.{empty => emptyDq}
import viper.silver.ast._

import scala.annotation.tailrec


/**
  * Trampoline implementation based on http://blog.richdougherty.com/2009/04/tail-calls-tailrec-and-trampolines.html,
  * with some extensions from "Stackless Scala with Free Monads" by Runar Oli Bjarnason.
  */
sealed trait Bounce [+ A] {
  @tailrec
  final def runT : A =
    resume match {
      case Left (k) => k().runT
      case Right (v) => v
    }

  @tailrec
  final def resume : Either[() => Bounce[A], A] =
    this match {
      case Done(v)       => Right (v)
      case Call(k)       => Left (k)
      case FlatMap (a,f) =>
        a match {
          case Done (v)     => f (v).resume
          case Call (k)     => Left (() => k () flatMap f)
          case FlatMap(b,g) => b.flatMap ((x : Any) => g (x) flatMap f).resume
        }
    }

  def flatMap[B] (f : A => Bounce[B]) : Bounce[B] =
    this match {
      case FlatMap (a, g) =>
        FlatMap (a, (x: Any) => g (x) flatMap f)
      case x =>
        FlatMap (x, f)
    }

  def map[B] (f : A => B) : Bounce[B] =
    flatMap (a => Done (f (a)))
}
case class Call[+A](k: () => Bounce[A]) extends Bounce[A]
case class Done [+A](result: A) extends Bounce [A]
case class FlatMap[A,+B] (ta : Bounce[A], k : A => Bounce[B]) extends Bounce[B]


/**
  * Primitive methods for pretty printing, based on "Linear, Bounded, Functional Pretty-Printing" by S. Doaitse
  * Swierstra and Olaf Chitil, extended to use trampolines to limit the stack size.
  */
trait PrettyPrintPrimitives {

  type Indent = Int
  type Width = Int
  type Layout = String

  type Position = Int
  type Remaining = Int
  type Horizontal = Boolean

  type Spec = (Indent, Width) => Horizontal => Position => Remaining => (Position, Remaining, Layout)

  type Horizontals = Queue[Horizontal]
  type P = Horizontals
  type C = Horizontals

  type State = (Position, Dq, C, Remaining)
  type Lazy = (Indent, Width) => State => (State, Layout, P)

  type Out = Remaining => Bounce[Seq[Layout]]
  type OutGroup = Horizontal => Out => Bounce[Out]

  type Dq = Queue[(Position, OutGroup)]
  type TreeCont = Position => Dq => Bounce[Out]
  type IW = (Indent, Width)
  type Cont = IW => TreeCont => Bounce[TreeCont]

  implicit def text(t: String): Cont = {
    if (t == "") {
      nil
    } else {
      (iw: IW) => {
        val l = t.length
        val outText =
          (_: Horizontal) =>
            (c: Out) => step(
              (r: Remaining) =>
                Call(() =>
                  for {
                    t2 <- c (r - l)
                  } yield t +: t2))
        scan (l, outText)
      }
    }
  }

  def line(repl: Layout) : Cont =
    (iw: IW) => {
      val (i, w) = iw
      val outLine =
        (h: Horizontal) =>
          (c: Out) => Done(
            (r: Remaining) => {
              if (h) {
                Call(() =>
                  for {
                    t <- c(r - repl.length)
                  } yield repl +: t)
              } else {
                Call(() =>
                  for {
                    t <- c(w - i)
                  } yield "\n" +: (" " * i) +: t )
              }
            })
      scan (1, outLine)
    }

  def group(d: Cont): Cont =
    (iw: IW) =>
      (c: TreeCont) =>
        Call(() =>
          for {
            t <- d(iw) (leave (c))
          } yield (p: Position) =>
            (dq: Dq) => { t (p) (dq :+ (p, (h: Horizontal) => (c: Out) => Done(c))) })


  def nest(j: Indent, d: Cont) : Cont =
    (iw: IW) => {
      val (i, w) = iw
      d ((i+j, w))
    }

  def pretty(w: Width, d: Cont) : Layout = {
    val res = for {
      t <- d((0, w)) ((p: Position) => (dq: Dq) => step((r: Remaining) => Done(Seq[String] ())))
      t2 <- t (0) (emptyDq)
      t3 <- t2 (w)
    } yield t3
    res.runT.mkString
  }

  def nil: Cont =
    (iw: IW) =>
      (c: TreeCont) =>
        Done(c)

  def scan(l: Width, out: OutGroup) (c: TreeCont): Bounce[TreeCont] =
    step(
      (p: Position) =>
        (dq: Dq) => {
          if (dq.isEmpty){
            Call(() =>
              for {
                t <- (c(p + l) (emptyDq))
                t2 <- out(false) (t)
              } yield t2)
          }else{
            val (s, grp) = dq.last
            prune (c) (p + l) (dq.init :+ (s, (h: Horizontal) => (o: Out) => Call(() => {
              for {
                t <- out(h)(o)
                t2 <- grp(h) (t)
              } yield t2
            })))
          }
        })

  def leave(c: TreeCont) : TreeCont =
    (p: Position) =>
      (dq: Dq) => {
        if (dq.isEmpty){
          c(p)(emptyDq)
        }else if (dq.length == 1){
          val (s1, grp1) = dq.last
          Call(() =>
            for {
              t <- c(p) (emptyDq)
              t2 <- grp1(true)(t)
            } yield t2)
        }else{
          val (s1, grp1) = dq.last
          val (s2, grp2) = dq.init.last
          c(p) (dq.init.init :+ (s2, (h: Horizontal) => {
            (c0: Out) => {
              val t = (r: Remaining) => {
                Call(() =>
                  for {
                    t <- grp1(p <= s1 + r)(c0)
                    t2 <- t (r)
                  } yield t2)
              }
              Call(() =>
                for {
                  t2 <- grp2(h)(t)
                } yield t2
              )
            }
          }))
        }
      }

  def prune(c: TreeCont) : TreeCont =
    (p: Position) =>
      (dq: Dq) => Done(
        (r: Remaining) => {
          if (dq.isEmpty) {
            Call(() =>
              for {
                t <- c (p) (emptyDq)
                t2 <- t(r)
              } yield t2)
          }else{
            val (s, grp) = dq.head
            if (p > s + r) {
              Call(() =>
                for {
                  t <- prune(c)(p)(dq.tail)
                  t2 <- grp(false)(t)
                  t3 <- t2(r)
                } yield t3)
            }else{
              Call(() =>
                for {
                  t <- c(p)(dq)
                  t2 <- t (r)
                } yield t2)
            }
          }
        })

  def step[A] (a : => A) : Bounce[A] =
    Call (() => Done (a))
}


/**
 * Pretty printing functions build on the above primtives, aimed to be compatible with Kiama's because the existing
 * pretty printers were built using that.
 */
trait FastPrettyPrinterBase extends PrettyPrintPrimitives {

  /**
    * Default indentation is four spaces.
    */
  val defaultIndent = 4

  /**
    * Default layout width is 75 characters.
    */
  val defaultWidth = 75

  /**
    * The operations provided by a pretty-printable document that don't
    * depend on the document's representation type.
    */

  trait PrettyPrintable {
    def toDoc : Cont = value (this)
  }

  implicit def anyToPrettyPrintable (a : Any) : PrettyPrintable =
    new PrettyPrintable {
      override def toDoc : Cont = value (a)
    }


  def string (s : String) : Cont = {
    if (s == "") {
      nil
    } else if (s.charAt(0) == '\n') {
      line <> string(s.tail)
    } else {
      val (xs, ys) = s.span(_ != '\n')
      text(xs) <> string(ys)
    }
  }

  /**
    * Convert a character to a document.  The character can be a newline.
    */
  implicit def char (c : Char) : Cont =
    if (c == '\n')
      line
    else
      text (c.toString)

  def any (a : Any) : Cont =
    if (a == null)
      "null"
    else
      a match {
        case Nil           => "Nil"
        case (l, r)        => any (l) <+> "->" <+> any (r)
        case None          => "None"
        case s : String    => char('\"') <> text (s) <> char('\"')
        case _             => a.toDoc
      }


  def folddoc (ds : Seq[Cont], f : (Cont, Cont) => Cont) =
    if (ds.isEmpty)
      nil
    else
      ds.tail.foldLeft (ds.head) (f)

  def ssep (ds : Seq[Cont], sep : Cont) : Cont =
    folddoc (ds, (_ <> sep <> _))


  def lsep (ds : Seq[Cont], sep : Cont) : Cont =
    if (ds.isEmpty)
      nil
    else
      linebreak <> folddoc (ds, _ <> sep <@> _)


  def value (v : Any) : Cont =
    if (v == null)
      "null"
    else
      string (v.toString)


  def surround (d : Cont, b : Cont) : Cont =
    b <> d <> b


  def braces (d : Cont) : Cont =
    char ('{') <> d <> char ('}')


  def parens (d : Cont) : Cont =
    char ('(') <> d <> char (')')



  def brackets (d : Cont) : Cont =
    char ('[') <> d <> char (']')


  def space : Cont =
    char (' ')


  def linebreak : Cont =
    line ("\n")


  implicit class ContOps(dl: Cont) {
    def <>(dr: Cont) : Cont =
      (iw: IW) =>
        (c: TreeCont) => {
          Call(() =>
          for {
            t <- (dr (iw)) (c)
            t2 <- (dl (iw)) (t)
          } yield t2)
        }

    def <+> (dr : Cont) : Cont =
      dl <> space <> dr

    def <@> (dr: Cont) : Cont =
      dl <> line <> dr
  }

  def line: Cont = line(" ")


}


/**
  * Classes representing associativity and fixity of operations, to be used for pretty printing.
  */
abstract class Associativity

trait PrettyExpression
case object LeftAssociative extends Associativity
case object RightAssociative extends Associativity
case object NonAssociative extends Associativity

abstract class Fixity
case object Prefix extends Fixity
case object Postfix extends Fixity
case class Infix (assoc : Associativity) extends Fixity



trait PrettyOperatorExpression extends PrettyExpression {
  def priority : Int
  def fixity : Fixity
}

trait PrettyBinaryExpression extends PrettyOperatorExpression {
  def left : PrettyExpression
  def op : String
  def right : PrettyExpression
}
trait PrettyUnaryExpression extends PrettyOperatorExpression {
  def op : String
  def exp : PrettyExpression
}


/**
  * Pretty printer for expressions that uses as few parentheses as possible, based on algorithm in
  * "Unparsing expressions with prefix and postfix operators", Ramsey, SP&E, 28 (12), October 1998.
  */
trait BracketPrettyPrinter extends FastPrettyPrinterBase {
  def toParenDoc(e: PrettyExpression): Cont

  //uses to implement the paper algo, if sees that it has to be bracketed, brackets else not
  def bracket (inner : PrettyOperatorExpression, outer : PrettyOperatorExpression,
               side : Associativity) : Cont = {
    val d = toParenDoc (inner)
    if (noparens (inner, outer, side)) d else parens (d)
  }


  def noparens (inner : PrettyOperatorExpression, outer : PrettyOperatorExpression,
                side : Associativity) : Boolean = {
    val pi = inner.priority
    val po = outer.priority
    lazy val fi = inner.fixity
    lazy val fo = outer.fixity
    (pi < po) ||
      ((fi, side) match {
        case (Postfix, LeftAssociative) =>
          true
        case (Prefix, RightAssociative) =>
          true
        case (Infix (LeftAssociative), LeftAssociative) =>
          (pi == po) && (fo == Infix (LeftAssociative))
        case (Infix (RightAssociative), RightAssociative) =>
          (pi == po) && (fo == Infix (RightAssociative))
        case (_, NonAssociative) =>
          fi == fo
        case _ =>
          false
      })
  }
}

/**
  * Pretty printer for the Silver language
  */
object FastPrettyPrinter extends FastPrettyPrinterBase with BracketPrettyPrinter {

  override val defaultIndent = 2

  lazy val uninitialized: Cont = value("<not initialized>")

  override implicit def text(t: String): Cont = {
    super.text(t)
  }

  /** Pretty-print any AST node. */
  def pretty(n: Node): String = {
    super.pretty(defaultWidth, show(n))
  }


  /** Show any AST node. */
  def show(n: Node): Cont = n match {
    case exp: Exp => toParenDoc(exp)
    case stmt: Stmt => showStmt(stmt)
    case typ: Type => showType(typ)
    case p: Program => showProgram(p)
    case m: Member => showMember(m)
    case v: LocalVarDecl => showVar(v)
    case dm: DomainMember => showDomainMember(dm)
    case Trigger(exps) =>
      text("{") <+> ssep(exps.to[collection.immutable.Seq] map show, char (',')) <+> "}"
    case null => uninitialized
  }

  /** Show a program. */
  def showProgram(p: Program): Cont = {
    val Program(domains, fields, functions, predicates, methods) = p
    showComment(p) <>
      ssep((domains ++ fields ++ functions ++ predicates ++ methods).to[collection.immutable.Seq] map show, line <> line)
  }

  /** Show a domain member. */
  def showDomainMember(m: DomainMember): Cont = {
    val memberDoc = m match {
      case f @ DomainFunc(_, _, _, unique) =>
        if (unique) text("unique") <+> showDomainFunc(f) else showDomainFunc(f)
      case DomainAxiom(name, exp) =>
        text("axiom") <+> name <+>
          braces(nest(defaultIndent,
            line <> show(exp)
          ) <> line)
    }
    showComment(m) <> memberDoc
  }


  def showDomainFunc(f: DomainFunc) = {
    val DomainFunc(name, formalArgs, typ, _) = f
    text("function") <+> name <> parens(showVars(formalArgs)) <> ":" <+> show(typ)
  }

  /** Show a program member. */
  def showMember(m: Member): Cont = {
    val memberDoc = m match {
      case f@Field(name, typ) =>
        text("field") <+> name <> ":" <+> show(typ)
      case m@Method(name, formalArgs, formalReturns, pres, posts, locals, body) =>
        text("method") <+> name <> parens(showVars(formalArgs)) <> {
          if (formalReturns.isEmpty) nil
          else nil <+> "returns" <+> parens(showVars(formalReturns))
        } <>
          nest(defaultIndent,
            showContracts("requires", pres) <>
            showContracts("ensures", posts)
          ) <>
          line <>
          braces(nest(defaultIndent,
            lineIfSomeNonEmpty(locals, if (body == null) null else body.children) <>
              ssep(
                ((if (locals == null) Nil else locals map (text("var") <+> showVar(_))) ++
                  Seq(showStmt(body))).to[collection.immutable.Seq], line)
          ) <> line)
      case p@Predicate(name, formalArgs, body) =>
        text("predicate") <+> name <> parens(showVars(formalArgs)) <+> (body match {
          case None => nil
          case Some(exp) => braces(nest(defaultIndent, line <> show(exp)) <> line)
        })
      case p@Function(name, formalArgs, typ, pres, posts, optBody) =>
        text("function") <+> name <> parens(showVars(formalArgs)) <>
          ":" <+> show(typ) <>
          nest(defaultIndent,
            showContracts("requires", pres) <>
            showContracts("ensures", posts)
          ) <>
          line <>
          (optBody match {
            case None => nil
            case Some(exp) => braces(nest(defaultIndent, line <> show(exp)) <> line)
            case _ => uninitialized
          })
      case d: Domain =>
        showDomain(d)
    }
    showComment(m) <> memberDoc
  }

  /** Shows contracts and use `name` as the contract name (usually `requires` or `ensures`). */
  def showContracts(name: String, contracts: Seq[Exp]): Cont = {
    if (contracts == null)
      line <> name <+> uninitialized
    else
      lineIfSomeNonEmpty(contracts) <> ssep((contracts map (text(name) <+> show(_))).to[collection.immutable.Seq], line)
  }

  /** Returns `n` lines if at least one element of `s` is non-empty, and an empty document otherwise. */
  def lineIfSomeNonEmpty[T](s: Seq[T]*)(implicit n: Int = 1) = {
    if (s.forall(e => e != null && e.isEmpty)) nil
    else {
      var r = nil
      for (i <- 1 to n) r = r <> line
      r
    }
  }

  /** Show a list of formal arguments. */
  def showVars(vars: Seq[LocalVarDecl]): Cont = ssep((vars map showVar).to[collection.immutable.Seq], char (',') <> space)
  /** Show a variable name with the type of the variable (e.g. to be used in formal argument lists). */
  def showVar(v: LocalVarDecl): Cont = text(v.name) <> ":" <+> showType(v.typ)

  /** Show field name */
  private def showLocation(loc: Location): Cont = loc.name

  /** Show a user-defined domain. */
  def showDomain(d: Domain): Cont = {
    d match {
      case Domain(name, functions, axioms, typVars) =>
        text("domain") <+> name <>
          (if (typVars.isEmpty) nil else text("[") <> ssep((typVars map show).to[collection.immutable.Seq], char (',') <> space) <> "]") <+>
          braces(nest(defaultIndent,
            line <> line <>
              ssep(((functions ++ axioms) map show).to[collection.immutable.Seq], line <> line)
          ) <> line)
    }
  }

  /** Show a type. */
  def showType(typ: Type): Cont = {
    typ match {
      case Bool => "Bool"
      case Int => "Int"
      case Ref => "Ref"
      case Perm => "Perm"
      case InternalType => "InternalType"
      case Wand => "$WandType"
      case SeqType(elemType) => text("Seq") <> "[" <> show(elemType) <> "]"
      case SetType(elemType) => text("Set") <> "[" <> show(elemType) <> "]"
      case MultisetType(elemType) => text("Multiset") <> "[" <> show(elemType) <> "]"
      case TypeVar(v) => v
      case dt@DomainType(domainName, typVarsMap) =>
        val typArgs = dt.typeParameters map (t => show(typVarsMap.getOrElse(t, t)))
        text(domainName) <> (if (typArgs.isEmpty) nil else brackets(ssep(typArgs.to[collection.immutable.Seq], char (',') <> space)))
    }
  }

  /** Show some node inside square braces (with nesting). */
  def showBlock(stmt: Stmt): Cont = {
    braces(nest(defaultIndent,
      lineIfSomeNonEmpty(stmt.children) <>
        showStmt(stmt)
    ) <> line)
  }

  /** Show a statement. */
  def showStmt(stmt: Stmt): Cont = {
    val stmtDoc = stmt match {
      case NewStmt(target, fields) =>
        show(target) <+> ":=" <+> "new(" <> ssep((fields map (f => value(f.name))).to[collection.immutable.Seq], char(',') <> space) <>")"
      case LocalVarAssign(lhs, rhs) => show(lhs) <+> ":=" <+> show(rhs)
      case FieldAssign(lhs, rhs) => show(lhs) <+> ":=" <+> show(rhs)
      case Fold(e) => text("fold") <+> show(e)
      case Unfold(e) => text("unfold") <+> show(e)
      case Package(e) => text("package") <+> show(e)
      case Apply(e) => text("apply") <+> show(e)
      case Inhale(e) => text("inhale") <+> show(e)
      case Exhale(e) => text("exhale") <+> show(e)
      case Assert(e) => text("assert") <+> show(e)
      case Fresh(vars) =>
        text("fresh") <+> ssep((vars map show).to[collection.immutable.Seq], char (',') <> space)
      case Constraining(vars, body) =>
        text("constraining") <> parens(ssep((vars map show).to[collection.immutable.Seq], char (',') <> space)) <+> showBlock(body)
      case MethodCall(mname, args, targets) =>
        val call = text(mname) <> parens(ssep((args map show).to[collection.immutable.Seq], char (',') <> space))
        targets match {
          case Nil => call
          case _ => ssep((targets map show).to[collection.immutable.Seq], char (',') <> space) <+> ":=" <+> call
        }
      case Seqn(ss) =>
        val sss = ss filter (s => !(s.isInstanceOf[Seqn] && s.children.isEmpty))
        ssep((sss map show).to[collection.immutable.Seq], line)
      case While(cond, invs, locals, body) =>
        text("while") <+> parens(show(cond)) <>
          nest(defaultIndent,
            showContracts("invariant", invs)
          ) <+> lineIfSomeNonEmpty(invs) <>
          braces(nest(defaultIndent,
            lineIfSomeNonEmpty(locals, body.children) <>
              ssep(
                ((if (locals == null) Nil else locals map (text("var") <+> showVar(_))) ++
                  Seq(showStmt(body))).to[collection.immutable.Seq], line)
          ) <> line)
      case If(cond, thn, els) =>
        text("if") <+> parens(show(cond)) <+> showBlock(thn) <> showElse(els)
      case Label(name, invs) =>
        text("label") <+> name <>
          nest(defaultIndent,
            showContracts("invariant", invs)
          )
      case Goto(target) =>
        text("goto") <+> target
      case null => uninitialized
    }
    showComment(stmt) <> stmtDoc
  }

  def showElse(els: Stmt): Cont = els match {
    case Seqn(Seq()) => nil
    case Seqn(Seq(s)) => showElse(s)
    case If(cond1, thn1, els1) => nil <+> "elseif" <+> parens(show(cond1)) <+> showBlock(thn1) <> showElse(els1)
    case _ => nil <+> "else" <+> showBlock(els)
  }

  /** Outputs the comments attached to `n` if there is at least one. */
  def showComment(n: Infoed): Cont = {
    if (n == null)
      nil
    else {
      val comment = n.info.comment
      if (comment.nonEmpty) ssep((comment map (text("//") <+> _)).to[collection.immutable.Seq], line) <> line
      else nil
    }
  }

  override def toParenDoc(e: PrettyExpression): Cont = e match {
    case IntLit(i) => value(i)
    case BoolLit(b) => value(b)
    case NullLit() => value(null)
    case AbstractLocalVar(n) => n
    case FieldAccess(rcv, field) =>
      show(rcv) <> "." <> field.name
    case PredicateAccess(params, predicateName) =>
      text(predicateName) <> parens(ssep((params map show).to[collection.immutable.Seq], char (',') <> space))
    case Unfolding(acc, exp) =>
      parens(text("unfolding") <+> show(acc) <+> "in" <+> show(exp))
    case UnfoldingGhostOp(acc, exp) =>
      parens(text("unfolding") <+> show(acc) <+> "in" <+> show(exp))
    case FoldingGhostOp(acc, exp) =>
      parens(text("folding") <+> show(acc) <+> "in" <+> show(exp))
    case PackagingGhostOp(wand, in) =>
      parens(text("packaging") <+> show(wand) <+> "in" <+> show(in))
    case ApplyingGhostOp(wand, in) =>
      parens(text("applying") <+> show(wand) <+> "in" <+> show(in))
    case Old(exp) =>
      text("old") <> parens(show(exp))
    case LabelledOld(exp,label) =>
      text("old") <> brackets(label) <> parens(show(exp))
    case ApplyOld(exp) =>
      text("given") <> parens(show(exp))
    case Let(v, exp, body) =>
      parens(text("let") <+> show(v) <+> "==" <+> show(exp) <+> "in" <+> show(body))
    case CondExp(cond, thn, els) =>
      parens(show(cond) <+> "?" <+> show(thn) <+> ":" <+> show(els))
    case Exists(v, exp) =>
      parens(text("exists") <+> showVars(v) <+> "::" <+> show(exp))
    case Forall(v, triggers, exp) =>
      parens(text("forall") <+> showVars(v) <+> "::" <>
        (if (triggers.isEmpty) nil else space <> ssep((triggers map show).to[collection.immutable.Seq], space)) <+>
        show(exp))
    case ForPerm(v, fields, exp) =>
      parens(text("forperm")
        <+> brackets(ssep((fields map showLocation).to[collection.immutable.Seq], char (',') <> space))
        <+> v.name <+> "::" <+> show(exp))

    case InhaleExhaleExp(in, ex) =>
      brackets(show(in) <> char (',') <+> show(ex))
    case WildcardPerm() =>
      "wildcard"
    case FullPerm() =>
      "write"
    case NoPerm() =>
      "none"
    case EpsilonPerm() =>
      "epsilon"
    case CurrentPerm(loc) =>
      text("perm") <> parens(show(loc))
    case AccessPredicate(loc, perm) =>
      text("acc") <> parens(show(loc) <> "," <+> show(perm))
    case FuncApp(funcname, args) =>
      text(funcname) <> parens(ssep((args map show).to[collection.immutable.Seq], char (',') <> space))
    case DomainFuncApp(funcname, args, _) =>
      text(funcname) <> parens(ssep((args map show).to[collection.immutable.Seq], char (',') <> space))

    case EmptySeq(elemTyp) =>
      text("Seq[") <> showType(elemTyp) <> "]()"
    case ExplicitSeq(elems) =>
      text("Seq") <> parens(ssep((elems map show).to[collection.immutable.Seq], char (',') <> space))
    case RangeSeq(low, high) =>
      text("[") <> show(low) <> ".." <> show(high) <> ")"
    case SeqIndex(seq, idx) =>
      show(seq) <> brackets(show(idx))
    case SeqTake(seq, n) =>
      show(seq) <> brackets(text("..") <> show(n))
    case SeqDrop(SeqTake(seq, n1), n2) =>
      show(seq) <> brackets(show(n2) <> ".." <> show(n1))
    case SeqDrop(seq, n) =>
      show(seq) <> brackets(show(n) <> "..")
    case SeqUpdate(seq, idx, elem) =>
      show(seq) <> brackets(show(idx) <+> ":=" <+> show(elem))
    case SeqLength(seq) =>
      surround(show(seq),char ('|'))
    case SeqContains(elem, seq) =>
      parens(show(elem) <+> "in" <+> show(seq))

    case EmptySet(elemTyp) =>
      text("Set[") <> showType(elemTyp) <> "]()"
    case ExplicitSet(elems) =>
      text("Set") <> parens(ssep((elems map show).to[collection.immutable.Seq], char (',') <> space))
    case EmptyMultiset(elemTyp) =>
      text("Multiset[") <> showType(elemTyp) <> "]()"
    case ExplicitMultiset(elems) =>
      text("Multiset") <> parens(ssep((elems map show).to[collection.immutable.Seq], char (',') <> space))
    case AnySetUnion(left, right) =>
      show(left) <+> "union" <+> show(right)
    case AnySetIntersection(left, right) =>
      show(left) <+> "intersection" <+> show(right)
    case AnySetSubset(left, right) =>
      show(left) <+> "subset" <+> show(right)
    case AnySetMinus(left, right) =>
      show(left) <+> "setminus" <+> show(right)
    case AnySetContains(elem, s) =>
      parens(show(elem) <+> "in" <+> show(s))
    case AnySetCardinality(s) =>
      surround(show(s),char ('|'))

    case null => uninitialized
    case _: PrettyUnaryExpression | _: PrettyBinaryExpression => {
      e match {
        case b: PrettyBinaryExpression =>
          val ld =
            b.left match {
              case l: PrettyOperatorExpression =>
                bracket(l, b, LeftAssociative)
              case l =>
                toParenDoc(l)
            }
          val rd =
            b.right match {
              case r: PrettyOperatorExpression =>
                bracket(r, b, RightAssociative)
              case r =>
                toParenDoc(r)
            }
          ld <+> text(b.op) <+> rd

        case u: PrettyUnaryExpression =>
          val ed =
            u.exp match {
              case e: PrettyOperatorExpression =>
                bracket(e, u, NonAssociative)
              case e =>
                toParenDoc(e)
            }
          if (u.fixity == Prefix)
            text(u.op) <> ed
          else
            ed <> text(u.op)

      }
    }
    case _ => sys.error(s"unknown expression: ${e.getClass}")
  }

}
