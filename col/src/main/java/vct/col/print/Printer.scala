package vct.col.print

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.origin._
import vct.col.resolve.ctx.{BuiltinField, BuiltinInstanceMethod, CDerefTarget, CInvocationTarget, CNameTarget, CTypeNameTarget, ImplicitDefaultJavaConstructor, ImplicitDefaultPVLConstructor, JavaConstructorTarget, JavaDerefTarget, JavaInvocationTarget, JavaNameTarget, JavaTypeNameTarget, PVLBuiltinInstanceMethod, PVLConstructorTarget, PVLDerefTarget, PVLInvocationTarget, PVLNameTarget, PVLTypeNameTarget, RefADTAxiom, RefADTFunction, RefAxiomaticDataType, RefCFunctionDefinition, RefCGlobalDeclaration, RefCLocalDeclaration, RefCParam, RefCTranslationUnit, RefClass, RefCudaVec, RefCudaVecDim, RefField, RefFunction, RefInstanceFunction, RefInstanceMethod, RefInstancePredicate, RefJavaAnnotationMethod, RefJavaClass, RefJavaConstructor, RefJavaField, RefJavaLocalDeclaration, RefJavaMethod, RefJavaNamespace, RefJavaSharedInitialization, RefLabelDecl, RefModel, RefModelAction, RefModelField, RefModelProcess, RefPVLConstructor, RefParBlockDecl, RefParInvariantDecl, RefPredicate, RefProcedure, RefRunMethod, RefSendDecl, RefSilverField, RefSimplificationRule, RefUnloadedJavaNamespace, RefVariable, Referrable, ResultTarget, ThisTarget}
import vct.col.util.AstBuildHelpers
import vct.col.util.AstBuildHelpers.foldStar

import scala.annotation.nowarn
import scala.collection.mutable

sealed trait Syntax
case object PVL extends Syntax
case object Silver extends Syntax
case object Java extends Syntax
case object C extends Syntax
case object Cuda extends Syntax
case object OpenCL extends Syntax

sealed trait PrinterState {
  def say(text: String)(implicit printer: Printer): PrinterState
  def space()(implicit printer: Printer): PrinterState
  def newline()(implicit printer: Printer): PrinterState
  def doubleNewline()(implicit printer: Printer): PrinterState

  def openInlineSpec()(implicit printer: Printer): PrinterState
  def closeInlineSpec()(implicit printer: Printer): PrinterState
  def openSpec()(implicit printer: Printer): PrinterState
  def closeSpec()(implicit printer: Printer): PrinterState
  def openBanNewlines()(implicit printer: Printer): PrinterState
  def closeBanNewlines()(implicit printer: Printer): PrinterState
  def indent()(implicit printer: Printer): PrinterState
  def dedent()(implicit printer: Printer): PrinterState
  def discardPendingNewlines()(implicit printer: Printer): PrinterState
}

case class InLine(lastWasSpace: Boolean, specDepth: Int, banNewlinesDepth: Int, indent: Int) extends PrinterState {
  override def say(text: String)(implicit printer: Printer): PrinterState = {
    printer.out.append(text)
    InLine(if(text.nonEmpty) text.last == ' ' else false, specDepth, banNewlinesDepth, indent)
  }

  override def space()(implicit printer: Printer): PrinterState = {
    if(!lastWasSpace) printer.out.append(' ')
    InLine(lastWasSpace = true, specDepth, banNewlinesDepth, indent)
  }

  def toLines: Lines = Lines(0, specDepth, indent)
  def toLazy: LazyInLine = LazyInLine(lastWasSpace, specDepth, specDepth, banNewlinesDepth, indent)

  override def newline()(implicit printer: Printer): PrinterState =
    if(banNewlinesDepth > 0) space()
    else toLines.newline()

  override def doubleNewline()(implicit printer: Printer): PrinterState =
    if(banNewlinesDepth > 0) space()
    else toLines.doubleNewline()

  override def openInlineSpec()(implicit printer: Printer): PrinterState = toLazy.openInlineSpec()
  override def closeInlineSpec()(implicit printer: Printer): PrinterState = toLazy.closeInlineSpec()

  override def openSpec()(implicit printer: Printer): PrinterState = toLines.openSpec()
  override def closeSpec()(implicit printer: Printer): PrinterState = toLines.closeSpec()

  def openBanNewlines()(implicit printer: Printer): PrinterState =
    InLine(lastWasSpace, specDepth, banNewlinesDepth + 1, indent)
  def closeBanNewlines()(implicit printer: Printer): PrinterState =
    InLine(lastWasSpace, specDepth, banNewlinesDepth - 1, indent)

  override def indent()(implicit printer: Printer): PrinterState =
    InLine(lastWasSpace, specDepth, banNewlinesDepth, indent + 1)

  override def dedent()(implicit printer: Printer): PrinterState =
    InLine(lastWasSpace, specDepth, banNewlinesDepth, indent - 1)

  override def discardPendingNewlines()(implicit printer: Printer): PrinterState = this
}
case class LazyInLine(lastWasSpace: Boolean, specDepth: Int, wantSpec: Int, banNewlinesDepth: Int, indent: Int) extends PrinterState {
  def commit()(implicit printer: Printer): PrinterState = {
    val res = InLine(lastWasSpace, wantSpec, banNewlinesDepth, indent)
    if(specDepth > 0 && wantSpec == 0) res.space().say("@*/")
    else if(specDepth == 0 && wantSpec > 0) res.space().say("/*@ ")
    else res
  }

  override def say(text: String)(implicit printer: Printer): PrinterState = commit().say(text)
  override def space()(implicit printer: Printer): PrinterState = commit().space()
  override def newline()(implicit printer: Printer): PrinterState = commit().newline()
  override def doubleNewline()(implicit printer: Printer): PrinterState = commit().doubleNewline()

  override def openInlineSpec()(implicit printer: Printer): PrinterState =
    LazyInLine(lastWasSpace, specDepth, wantSpec+1, banNewlinesDepth, indent)
  override def closeInlineSpec()(implicit printer: Printer): PrinterState =
    LazyInLine(lastWasSpace, specDepth, wantSpec-1, banNewlinesDepth, indent)

  override def openSpec()(implicit printer: Printer): PrinterState = commit().openSpec()
  override def closeSpec()(implicit printer: Printer): PrinterState = commit().closeSpec()

  override def openBanNewlines()(implicit printer: Printer): PrinterState =
    LazyInLine(lastWasSpace, specDepth, wantSpec, banNewlinesDepth+1, indent)
  override def closeBanNewlines()(implicit printer: Printer): PrinterState =
    LazyInLine(lastWasSpace, specDepth, wantSpec, banNewlinesDepth-1, indent)

  override def indent()(implicit printer: Printer): PrinterState =
    LazyInLine(lastWasSpace, specDepth, wantSpec, banNewlinesDepth, indent+1)
  override def dedent()(implicit printer: Printer): PrinterState =
    LazyInLine(lastWasSpace, specDepth, wantSpec, banNewlinesDepth, indent-1)

  override def discardPendingNewlines()(implicit printer: Printer): PrinterState = this
}
case class Lines(newLines: Int, specDepth: Int, indent: Int) extends PrinterState {
  def toInLine: InLine = InLine(lastWasSpace = true, specDepth, 0, indent)

  override def say(text: String)(implicit printer: Printer): PrinterState = toInLine.say(text)
  override def space()(implicit printer: Printer): PrinterState = toInLine.space()

  override def newline()(implicit printer: Printer): PrinterState =
    LazyLines(newLines, wantNewLines = 1, specDepth, specDepth, indent)
  override def doubleNewline()(implicit printer: Printer): PrinterState =
    LazyLines(newLines, wantNewLines = 2, specDepth, specDepth, indent)

  override def openInlineSpec()(implicit printer: Printer): PrinterState = toInLine.openInlineSpec()
  override def closeInlineSpec()(implicit printer: Printer): PrinterState = toInLine.closeInlineSpec()

  override def openSpec()(implicit printer: Printer): PrinterState =
    LazyLines(newLines, newLines, specDepth, specDepth+1, indent)
  override def closeSpec()(implicit printer: Printer): PrinterState =
    LazyLines(newLines, newLines, specDepth, specDepth-1, indent)

  override def openBanNewlines()(implicit printer: Printer): PrinterState = toInLine.openBanNewlines()
  override def closeBanNewlines()(implicit printer: Printer): PrinterState = toInLine.closeBanNewlines()

  override def indent()(implicit printer: Printer): PrinterState =
    Lines(newLines, specDepth, indent+1)
  override def dedent()(implicit printer: Printer): PrinterState =
    Lines(newLines, specDepth, indent-1)

  override def discardPendingNewlines()(implicit printer: Printer): PrinterState = this
}
case class LazyLines(newLines: Int, wantNewLines: Int, specDepth: Int, wantSpecDepth: Int, indent: Int) extends PrinterState {
  def commit()(implicit printer: Printer): PrinterState = {
    if(specDepth > 0 && wantSpecDepth == 0) {
      printer.out.append(printer.newlineText)
      (0 until indent).foreach(_ => printer.out.append(printer.indentText))
      printer.out.append("@*/")
    }

    if(wantNewLines > newLines) {
      (0 until wantNewLines-newLines).foreach(_ => printer.out.append(printer.newlineText))
      (0 until indent).foreach(_ => printer.out.append(printer.indentText))
    }

    if(specDepth == 0 && wantSpecDepth > 0) {
      printer.out.append("/*@")
      printer.out.append(printer.newlineText)
      (0 until indent).foreach(_ => printer.out.append(printer.indentText))
    }

    Lines(wantNewLines, wantSpecDepth, indent)
  }

  override def say(text: String)(implicit printer: Printer): PrinterState = commit().say(text)
  override def space()(implicit printer: Printer): PrinterState = commit().space()

  override def newline()(implicit printer: Printer): PrinterState =
    LazyLines(newLines, wantNewLines.max(1), specDepth, wantSpecDepth, indent)
  override def doubleNewline()(implicit printer: Printer): PrinterState =
    LazyLines(newLines, wantNewLines.max(2), specDepth, wantSpecDepth, indent)

  override def openInlineSpec()(implicit printer: Printer): PrinterState = commit().openInlineSpec()
  override def closeInlineSpec()(implicit printer: Printer): PrinterState = commit().closeInlineSpec()

  override def openSpec()(implicit printer: Printer): PrinterState =
    LazyLines(newLines, wantNewLines, specDepth, wantSpecDepth+1, indent)
  override def closeSpec()(implicit printer: Printer): PrinterState =
    LazyLines(newLines, wantNewLines, specDepth, wantSpecDepth-1, indent)

  override def openBanNewlines()(implicit printer: Printer): PrinterState = commit().openBanNewlines()
  override def closeBanNewlines()(implicit printer: Printer): PrinterState = commit().closeBanNewlines()

  override def indent()(implicit printer: Printer): PrinterState =
    LazyLines(newLines, wantNewLines, specDepth, wantSpecDepth, indent+1)
  override def dedent()(implicit printer: Printer): PrinterState =
    LazyLines(newLines, wantNewLines, specDepth, wantSpecDepth, indent-1)

  override def discardPendingNewlines()(implicit printer: Printer): PrinterState =
    LazyLines(wantNewLines, wantNewLines, specDepth, wantSpecDepth, indent)
}

case class PrependAfterWhitespace(inner: PrinterState, prepend: PrinterState => PrinterState) extends PrinterState {
  override def say(text: String)(implicit printer: Printer): PrinterState = prepend(inner).say(text)
  override def space()(implicit printer: Printer): PrinterState = PrependAfterWhitespace(inner.space(), prepend)
  override def newline()(implicit printer: Printer): PrinterState = PrependAfterWhitespace(inner.newline(), prepend)
  override def doubleNewline()(implicit printer: Printer): PrinterState = PrependAfterWhitespace(inner.doubleNewline(), prepend)
  override def openInlineSpec()(implicit printer: Printer): PrinterState = prepend(inner).openInlineSpec()
  override def closeInlineSpec()(implicit printer: Printer): PrinterState = prepend(inner).closeInlineSpec()
  override def openSpec()(implicit printer: Printer): PrinterState = prepend(inner).openSpec()
  override def closeSpec()(implicit printer: Printer): PrinterState = prepend(inner).closeSpec()
  override def openBanNewlines()(implicit printer: Printer): PrinterState = prepend(inner).openBanNewlines()
  override def closeBanNewlines()(implicit printer: Printer): PrinterState = prepend(inner).closeBanNewlines()
  override def indent()(implicit printer: Printer): PrinterState = prepend(inner).indent()
  override def dedent()(implicit printer: Printer): PrinterState = prepend(inner).dedent()
  override def discardPendingNewlines()(implicit printer: Printer): PrinterState = PrependAfterWhitespace(inner.discardPendingNewlines(), prepend)
}

//PB TODO: make printer complete once we're nearing the end of making new nodes.
//PB TODO: document the printer once it's not terrible.
//@nowarn("msg=xhaust")
case class Printer(out: Appendable,
                   syntax: Syntax = Java,
                   permissive: Boolean = true,
                   newlineText: String = "\n",
                   indentText: String = "    ",
) {
  implicit def printer: Printer = this

  sealed trait Phrase
  implicit class Text(val text: String) extends Phrase
  implicit class NodePhrase(val node: Node[_]) extends Phrase
  implicit class NodesPhrase(val nodes: Seq[Node[_]]) extends Phrase
  case object space extends Phrase
  case object newline extends Phrase
  case object doubleline extends Phrase
  case class Do(f: () => Unit) extends Phrase

  var state: PrinterState = Lines(1000, 0, 0)

  def say(phrases: Phrase*): Unit = {
    phrases.foreach {
      case text: Text => state = state.say(text.text)
      case node: NodePhrase => print(node.node)
      case nodes: NodesPhrase => nodes.nodes.foreach(print)
      case `space` => state = state.space()
      case `newline` => state = state.newline()
      case `doubleline` => state = state.doubleNewline()
      case f: Do => f.f()
    }
  }

  def phrase(phrases: Phrase*): Phrase = Do(() => {
    say(phrases:_*)
  })

  def inlineSpec(phrases: Phrase*): Phrase = Do(() => {
    state = state.openInlineSpec()
    say(phrases:_*)
    state = state.closeInlineSpec()
  })

  def spec(phrases: Phrase*): Phrase = Do(() => {
    syntax match {
      case PVL | Silver => say(phrases:_*)
      case _ =>
        state = state.openSpec()
        say(phrases:_*)
        state = state.closeSpec()
    }
  })

  def indent(phrases: Phrase*): Phrase = Do(() => {
    state = state.indent()
    say(phrases:_*)
    state = state.dedent()
  })

  def forceInline(phrases: Phrase*): Phrase = Do(() => {
    state = state.openBanNewlines()
    say(phrases:_*)
    state = state.closeBanNewlines()
  })

  def controls(branches: Seq[(Phrase, Node[_])]): Phrase = Do(() => {
    say(doubleline)

    var lastWasBlock = false

    say(branches.head._1)
    branches.head._2 match {
      case block: Block[_] =>
        say(printBlock(block, newline = false))
        lastWasBlock = true
      case other =>
        say(indent(other))
    }

    for((control, impl) <- branches.tail) {
      state = state.discardPendingNewlines()

      if(lastWasBlock) state = state.space()
      else state = state.newline()

      say(control)

      impl match {
        case block: Block[_] =>
          say(printBlock(block, newline = false))
          lastWasBlock = true
        case other =>
          say(indent(other))
          lastWasBlock = false
      }
    }

    say(doubleline)
  })

  def control(control: Phrase, impl: Node[_]): Phrase =
    controls(Seq((control, impl)))

  def intersperse(spacing: Phrase, nodes: Seq[Phrase]): Do = Do(() => {
    for ((node, i) <- nodes.zipWithIndex) {
      if (i != 0) say(spacing)
      say(node)
    }
  })

  def commas(nodes: Seq[Phrase]): Do =
    intersperse(", ", nodes)

  def spaced(nodes: Seq[Phrase]): Do =
    intersperse(space, nodes)

  def prepend(writeLabel: Phrase, target: Phrase): Do = Do(() => {
    state = PrependAfterWhitespace(state, out => {
      state = out
      say(writeLabel)
      state
    })
    say(target)
  })

  def syntax(fsSeq: (Syntax, Phrase)*): Phrase = Do(() => {
    val fs = fsSeq.toMap
    val f = syntax match {
      case Cuda => fs.get(Cuda).orElse(fs.get(C))
      case OpenCL => fs.get(OpenCL).orElse(fs.get(C))
      case other => fs.get(other)
    }

    if(f.isEmpty) {
      if(permissive) say(fs.values.head)
      else ???
    } else say(f.get)
  })

  def clauses(contract: Expr[_], clauseKeyword: String): Do = Do(() => {
    for(clause <- AstBuildHelpers.unfoldStar(contract)) {
      say(newline, clauseKeyword, space, clause, ";", newline)
    }
  })

  def clauses(contract: AccountedPredicate[_], clauseKeyword: String): Do =
    clauses(foldStar(contract)(DiagnosticOrigin), clauseKeyword)

  def statement(phrases: Phrase*): Do = Do(() => {
    say(newline)
    say(phrases:_*)
    say(syntax(
      C -> phrase(";"),
      Java -> phrase(";"),
      PVL -> phrase(";"),
      Silver -> phrase(),
    ))
    say(newline)
  })

  def javaDecls(decls: Seq[JavaVariableDeclaration[_]]): Phrase = commas(for(JavaVariableDeclaration(name, dims, init) <- decls)
    yield init match {
      case Some(value) => phrase(name, "[]".repeat(dims), space, "=", space, value)
      case None => phrase(name, "[]".repeat(dims))
    }
  )

  var names: ScopedStack[mutable.Map[Referrable[_], String]] = ScopedStack()
  names.push(mutable.Map())
  var usedNames: ScopedStack[mutable.Set[(String, Int)]] = ScopedStack()
  usedNames.push(mutable.Set())

  def unfmt(name: String): (String, Int) = {
    if(name.last.isDigit) {
      val overeenkomst = "^(.*)(0|[1-9][0-9]*)$".r.findFirstMatchIn(name).get
      (overeenkomst.group(1), Integer.parseInt(overeenkomst.group(2)))
    } else {
      (name, -1)
    }
  }

  def nextName(name: String): String = {
    var (baseName, idx) = unfmt(name)
    while(usedNames.exists(_.contains((baseName, idx)))) {
      idx += 1
    }
    usedNames.top += ((baseName, idx))

    if(idx == -1) baseName
    else s"$baseName$idx"
  }

  def name(decl: Declaration[_]): String =
    name(Referrable.from(decl).head)(decl.o.preferredName)

  def name(decl: Referrable[_])(preferredName: String = decl.name): String =
    names.find(_.contains(decl))
      .getOrElse(names.top)
      .getOrElseUpdate(decl, nextName(preferredName))

  def printBlock(block: Block[_], newline: Boolean): Phrase =
    phrase(if(newline) doubleline else space, "{", indent(block.statements.map(NodePhrase) : _*), "}", if(newline) doubleline else space)

  def printProgram(program: Program[_]): Unit =
    say(program.declarations)

  def printStatement(stat: Statement[_]): Unit = say(stat match {
    case Commit(obj) => phrase("\\commit(", obj, ")")
    case CDeclarationStatement(decl) =>
      statement(syntax(C -> phrase(intersperse(" ", decl.decl.specs.map(NodePhrase)), space, commas(decl.decl.inits.map(NodePhrase)))))
    case ref @ CGoto(label) =>
      statement(syntax(C -> phrase("goto", space, Text(ref.ref.map(name).getOrElse(label)))))
    case GpgpuBarrier(requires, ensures, specifier) =>
      statement(spec(clauses(requires, "requires"), clauses(ensures, "ensures")), syntax(
        Cuda -> phrase("__syncthreads();"),
        OpenCL -> phrase("barrier(", intersperse(" | ", specifier.map(NodePhrase)), ");"),
      ))
    case GpgpuAtomic(impl, before, after) =>
      syntax(C -> statement("__vercors_atomic__", space, forceInline(impl), space,
        spec("with", space, before, space, "then", space, before)))
    case JavaLocalDeclarationStatement(decl) =>
      statement(decl.t, space, javaDecls(decl.decls))
    case SilverNewRef(v, fields) =>
      syntax(Silver -> statement(name(v.decl), space, ":=", space, "new(", commas(fields.map(_.decl).map(name).map(Text))))
    case SilverFieldAssign(obj, field, value) =>
      syntax(Silver -> statement(obj, ".", name(field.decl), space, ":=", space, value))
    case SilverLocalAssign(v, value) =>
      syntax(Silver -> statement(name(v.decl), space, ":=", space, value))
    case Eval(expr) =>
      statement(expr)
    case LocalDecl(local) =>
      statement(local.t, space, name(local))
    case Return(result) =>
      statement("return", space, result)
    case Assign(target, value) =>
      statement(target, space, "=", space, value)
    case block: Block[_] =>
      printBlock(block, newline = true)
    case Scope(locals, body) =>
      implicit val o: Origin = DiagnosticOrigin
      if(locals.nonEmpty)
        printBlock(Block(locals.map(LocalDecl(_)(o)) :+ body), newline = true)
      else
        NodePhrase(body)
    case Branch(branches) =>
      val `if` = (phrase("if(", branches.head._1, ")"), branches.head._2)
      val others = branches.tail.map {
        case (cond, impl) => (phrase("else if(", cond, ")"), impl)
      }
      controls(`if` +: others)
    case Switch(expr, body) =>
      control(phrase("switch(", expr, ")"), body)
    case Loop(init, cond, update, invariant, body) =>
      control(phrase("for(", init, "; ", cond, "; ", update, ")"), body)
    case TryCatchFinally(body, after, catches) =>
      controls(
        Seq((phrase("try"), body)) ++
          catches.map(clause => (phrase("catch(", clause.decl, ")"), clause.body)) ++
          Seq((phrase("finally"), after))
      )
    case Synchronized(obj, body) =>
      control(phrase("synchronized(", obj, ")"), body)
    case ParInvariant(decl, inv, content) =>
      control(phrase("invariant", space, name(decl), "(", inv, ")"), content)
    case ParAtomic(inv, content) =>
      control(phrase("atomic(", commas(inv.map(_.decl).map(name).map(Text)), ")"), content)
    case ParBarrier(block, invs, requires, ensures, content) =>
      val tags = invs match {
        case Nil => phrase()
        case invs => phrase(";", commas(invs.map(_.decl).map(name).map(Text)))
      }
      val contract = spec(clauses(requires, "requires"), clauses(ensures, "ensures"))

      syntax(
        PVL -> phrase("barrier(", name(block.decl), tags, ")", contract, newline, content),
      )
//    case ParRegion(requires, ensures, blocks) =>
//      phrase(
//        doubleline,
//        spec(clauses(requires, "requires"), clauses(ensures, "ensures")),
//        newline, "par", space, blocks.head, newline,
//        phrase(blocks.tail.map(block => phrase("and", space, block)):_*),
//        doubleline
//      )
    case Throw(e) =>
      statement("throw", space, e)
    case DefaultCase() =>
      phrase(
        Do(() => state = state.dedent()),
        statement("default:"),
        Do(() => state = state.indent()),
      )
    case Case(pattern) =>
      phrase(
        Do(() => state = state.dedent()),
        statement("case", space, pattern, ":"),
        Do(() => state = state.indent()),
      )
    case Label(decl, impl) =>
      statement(syntax(
        Java -> control(phrase(name(decl), ":"), impl),
        C -> control(phrase(name(decl), ":"), impl),
        Silver -> phrase("label", space, name(decl)),
        PVL -> phrase("label", space, name(decl)),
      ))
    case Goto(lbl) =>
      statement("goto", space, name(lbl.decl))
    case Exhale(res) =>
      spec(statement("exhale", space, res))
    case Assert(assn) =>
      spec(statement("assert", space, assn))
    case Refute(assn) =>
      spec(statement("refute", space, assn))
    case Inhale(res) =>
      spec(statement("inhale", space, res))
    case Assume(assn) =>
      spec(statement("assume", space, assn))
    case SpecIgnoreStart() =>
      spec(newline, "spec_ignore {", newline)
    case SpecIgnoreEnd() =>
      spec(newline, "spec_ignore }", newline)
    case Wait(obj) =>
      syntax(PVL -> statement("wait", space, obj))
    case Notify(obj) =>
      syntax(PVL -> statement("notify", space, obj))
    case Fork(runnable) =>
      syntax(PVL -> statement("fork", space, runnable))
    case Join(runnable) =>
      syntax(PVL -> statement("join", space, runnable))
    case Lock(obj) =>
      syntax(PVL -> statement("lock", space, obj))
    case Unlock(obj) =>
      syntax(PVL -> statement("unlock", space, obj))
    case Fold(pred) =>
      spec(statement("fold", space, pred))
    case Unfold(pred) =>
      spec(statement("unfold", space, pred))
    case WandPackage(expr, state) =>
      spec(control(phrase("package", expr), state))
    case WandApply(wand) =>
      spec(statement("apply", space, wand))
    case Havoc(loc) =>
      ???
    case Break(label) =>
      label match {
        case Some(label) => statement("break", space, name(label.decl))
        case None => statement("break")
      }
    case Continue(label) =>
      label match {
        case Some(label) => statement("continue", space, name(label.decl))
        case None => statement("continue")
      }
    case InvokeMethod(obj, ref, args, outArgs, typeArgs, givenMap, yields) =>
      statement(assoc(100, obj), ".", name(ref.decl), "(", commas(args.map(NodePhrase)), ")")
    case InvokeProcedure(ref, args, outArgs, typeArgs, givenMap, yields) =>
      statement(name(ref.decl), "(", commas(args.map(NodePhrase)), ")")
  })

  def printExpr(e: Expr[_]): Unit =
    say(expr(e)._1)

  def bind(wantPrecedence: Int, e: Expr[_]): Phrase = {
    val (output, precedence) = expr(e)
    if(precedence > wantPrecedence) output
    else phrase("(", output, ")")
  }

  def assoc(wantPrecedence: Int, e: Expr[_]): Phrase =
    bind(wantPrecedence - 1, e)

  def expr(e: Expr[_]): (Phrase, Int) = e match {
    case CLocal(nodeName) => (phrase(nodeName), 110)
    case PVLLocal(name) => (phrase(name), 110)
    case CInvocation(applicable, args, _, _) =>
      (phrase(assoc(100, applicable), "(", commas(args.map(NodePhrase)), ")"), 100)
    case PVLDeref(obj, field) =>
      (phrase(assoc(100, obj), ".", field), 100)
    case CStructAccess(struct, field) =>
      (phrase(assoc(100, struct), ".", field), 100)
    case CStructDeref(struct, field) =>
      (phrase(assoc(100, struct), "->", field), 100)
    case GpgpuCudaKernelInvocation(kernel, blocks, threads, args, _, _) =>
      (phrase(kernel, "<<", blocks, ", ", threads, ">>(", commas(args.map(NodePhrase)), ")"), 100)
    case JavaLocal(name) => (phrase(name), 110)
    case JavaDeref(obj, field) =>
      (phrase(assoc(100, obj), ".", field), 100)
    case JavaLiteralArray(exprs) =>
      (phrase("{", commas(exprs.map(NodePhrase)), "}"), 120)
    case JavaStringLiteral(data) =>
      (phrase(s""""${data}""""), 100)
    case StringLiteral(data) =>
      (phrase(s""""${data}""""), 100)
    case InternedString(data, interner) =>
      (phrase("\\internedString(", data, ", ", interner.decl.o.preferredName, ")"), 100)
    case JavaInvocation(obj, typeParams, method, arguments, _, _) =>
      (obj match {
        case Some(obj) =>
          phrase(assoc(100, obj), ".", method, "(", commas(arguments.map(NodePhrase)), ")")
        case None =>
          phrase(method, "(", commas(arguments.map(NodePhrase)), ")")
      }, 100)
    case JavaNewClass(args, typeArgs, name, givenMap, yields, isBipJob) =>
      (phrase("new", space, if(isBipJob) "/*@ vercorsBipJob @*/" else "", space, name, "(", commas(args.map(NodePhrase)), ")"), 100)
    case JavaNewLiteralArray(baseType, dims, initializer) =>
      (phrase("new", space, baseType, "[]".repeat(dims), initializer), 100)
    case JavaNewDefaultArray(baseType, specifiedDims, moreDims) =>
      (phrase("new", space, baseType, phrase(specifiedDims.map(phrase("[", _, "]")):_*), "[]".repeat(moreDims)), 100)
    case SilverDeref(obj, field) =>
      (phrase(assoc(100, obj), ".", name(field.decl)), 100)
    case SilverCurFieldPerm(obj, field) => ???
    case SilverCurPredPerm(ref, args) => ???
    case SilverIntToRat(inner) => expr(inner)
    case Sum(bindings, condition, main) =>
      (phrase("(", "\\sum", commas(bindings.map(NodePhrase)), "; ", condition, ";", main, ")"), 120)
    case Product(bindings, condition, main) =>
      ???
    case Length(arr) =>
      (phrase(assoc(100, arr), ".length"), 100)
    case Size(obj) =>
      (phrase("|", obj, "|"), 120)
    case Empty(obj) =>
      (phrase("isEmpty(", obj, ")"), 100)
    case BagMemberCount(x, xs) =>
      (phrase("(", x, "\\memberof", xs, ")"), 120)
    case BooleanValue(value) =>
      (phrase(if(value) "true" else "false"), 100)
    case MapEq(left, right) =>
      (phrase("equalsMap(", left, ",", space, right, ")"), 100)
    case MapDisjoint(left, right) =>
      (phrase("disjointMap(", left, ",", space, right, ")"), 100)
    case Forall(bindings, triggers, body) =>
      (phrase("(", "\\forall", space, commas(bindings.map(NodePhrase)), "; true; ", body, ")"), 120)
    case Exists(bindings, triggers, body) =>
      (phrase("(", "\\exists", space, commas(bindings.map(NodePhrase)), "; true; ", body, ")"), 120)
    case ValidArray(arr, len) =>
      (phrase("\\array(", arr, ",", space, len, ")"), 100)
    case ValidMatrix(mat, w, h) =>
      (phrase("\\matrix(", mat, ",", space, w, ",", space, h, ")"), 100)
    case SetMember(x, xs) =>
      (phrase("(", x, "\\memberof", xs, ")"), 120)
    case SeqMember(x, xs) =>
      (phrase("(", x, "\\memberof", xs, ")"), 120)
    case MapMember(x, xs) =>
      (phrase("(", x, "\\memberof", xs, ")"), 120)
    case Permutation(xs, ys) =>
      ???
    case Held(obj) =>
      (phrase("held", "(", obj, ")"), 100)
    case IdleToken(thread) =>
      (phrase("idle", "(", thread, ")"), 100)
    case JoinToken(thread) =>
      (phrase("running", "(", thread, ")"), 100)
    case Starall(bindings, triggers, body) =>
      (phrase("(", "\\forall*", space, commas(bindings.map(NodePhrase)), "; true; ", body, ")"), 120)
    case Star(left, right) =>
      (phrase(assoc(40, left), space, "**", space, assoc(40, right)), 40)
    case Wand(left, right) =>
      (phrase(bind(30, left), space, "-*", space, assoc(30, right)), 30)
    case Scale(scale, res) =>
      (phrase("[", scale, "]", assoc(90, res)), 90)
    case ScaleByParBlock(block, res) =>
      (phrase("[", block.decl, "]", assoc(90, res)), 90)
    case Perm(loc, perm) =>
      (phrase("Perm(", loc, ",", space, perm, ")"), 100)
    case PointsTo(loc, perm, value) =>
      (phrase("PointsTo(", loc, ",", space, perm, ",", space, value, ")"), 100)
    case PermPointer(p, len, perm) =>
      (phrase("\\pointer(", p, ",", space, len, ",", space, perm, ")"), 100)
    case PermPointerIndex(p, idx, perm) =>
      (phrase("\\pointer_index(", p, ",", space, idx, ",", space, perm, ")"), 100)
    case ModelPerm(loc, perm) =>
      (phrase("HPerm(", loc, ",", space, perm, ")"), 100)
    case ActionPerm(loc, perm) =>
      (phrase("APerm(", loc, ",", space, perm, ")"), 100)
    case NoPerm() =>
      (phrase("none"), 110)
    case ReadPerm() =>
      (phrase("read"), 110)
    case WritePerm() =>
      (phrase("write"), 110)
    case EmptyProcess() =>
      (phrase("empty"), 110)
    case ActionApply(action, args) =>
      (phrase(name(action.decl), "(", commas(args.map(NodePhrase)), ")"), 100)
    case ProcessApply(process, args) =>
      (phrase(name(process.decl), "(", commas(args.map(NodePhrase)), ")"), 100)
    case ProcessSeq(left, right) =>
      (phrase(assoc(80, left), space, "*", space, assoc(80, right)), 80)
    case ProcessChoice(left, right) =>
      (phrase(assoc(70, left), space, "+", space, assoc(70, right)), 70)
    case ProcessPar(left, right) =>
      (phrase(assoc(30, left), space, "||", space, assoc(30, right)), 30)
    case ProcessSelect(cond, whenTrue, whenFalse) =>
      (phrase(bind(20, cond), space, "?", space, bind(20, whenTrue), space, ":", space, assoc(20, whenFalse)), 20)
    case IntegerValue(value) =>
      (phrase(value.toString(radix = 10)), 110)
    case LiteralSeq(element, values) =>
      (phrase("seq<", element, ">{", commas(values.map(NodePhrase)), "}"), 100)
    case LiteralSet(element, values) =>
      (phrase("set<", element, ">{", commas(values.map(NodePhrase)), "}"), 100)
    case UntypedLiteralSeq(values) =>
      (phrase("[", commas(values.map(NodePhrase)), "]"), 120)
    case UntypedLiteralSet(values) =>
      (phrase("{", commas(values.map(NodePhrase)), "}"), 120)
    case UntypedLiteralBag(values) =>
      (phrase("b{", commas(values.map(NodePhrase)), "}"), 120)
    case Void() =>
      (phrase("void"), 110)
    case AmbiguousThis() =>
      (phrase("this"), 110)
    case ThisModel(_) =>
      (phrase("this"), 110)
    case ThisObject(_) =>
      (phrase("this"), 110)
    case AmbiguousResult() =>
      (phrase("\\result"), 110)
    case Result(_) =>
      (phrase("\\result"), 110)
    case CurrentThreadId() =>
      (phrase("\\current_thread"), 110)
    case Null() =>
      (phrase("null"), 110)
    case Range(from, to) =>
      (phrase("{", from, space, "..", space, to, "}"), 120)
    case Values(arr, from, to) =>
      (phrase("\\values(", arr, ",", space, from, ",", space, to, ")"), 100)
    case OptSome(e) =>
      (phrase("Some(", e, ")"), 100)
    case OptNone() =>
      (phrase("None"), 110)
    case MapCons(map, k, v) =>
      (phrase("buildMap(", map, ",", space, k, ",", space, v, ")"), 100)
    case MapKeySet(map) =>
      (phrase("keysMap(", map, ")"), 100)
    case MapValueSet(map) =>
      (phrase("valuesMap(", map, ")"), 100)
    case MapItemSet(map) =>
      (phrase("itemsMap(", map, ")"), 100)
    case MapRemove(map, k) =>
      (phrase("removeFromMap(", map, ",", space, k, ")"), 100)
    case Let(binding, value, main) =>
      (phrase("(", "\\let", space, binding, space, "=", space, value, ";", space, main, ")"), 120)
    case InlinePattern(inner, parent, group) =>
      (phrase("{:", space, inner, space, ":}"), 120)
    case Local(ref) =>
      (phrase(name(ref.decl)), 110)
    case Deref(obj, ref) =>
      (phrase(assoc(100, obj), ".", name(ref.decl)), 100)
    case ModelDeref(obj, ref) =>
      (phrase(assoc(100, obj), ".", name(ref.decl)), 100)
    case DerefPointer(pointer) =>
      (phrase("*", assoc(90, pointer)), 90)
    case PointerAdd(pointer, offset) =>
      (phrase(assoc(70, pointer), space, "+", space, assoc(70, offset)), 70)
    case AddrOf(e) =>
      (phrase("&", assoc(90, e)), 90)
    case PredicateApply(ref, args, perm) =>
      (phrase(name(ref.decl), "(", commas(args.map(NodePhrase)), ")"), 100)
    case InstancePredicateApply(obj, ref, args, perm) =>
      (phrase(assoc(100, obj), ".", name(ref.decl), "(", commas(args.map(NodePhrase)), ")"), 100)
    case ADTFunctionInvocation(tArgs, ref, args) =>
      (phrase(name(ref.decl), "(", commas(args.map(NodePhrase)), ")"), 100)
    case ProcedureInvocation(ref, args, outArgs, typeArgs, givenMap, yields) =>
      (phrase(name(ref.decl), "(", commas(args.map(NodePhrase)), ")"), 100)
    case FunctionInvocation(ref, args, typeArgs, givenMap, yields) =>
      (phrase(name(ref.decl), "(", commas(args.map(NodePhrase)), ")"), 100)
    case MethodInvocation(obj, ref, args, outArgs, typeArgs, givenMap, yields) =>
      (phrase(assoc(100, obj), ".", name(ref.decl), "(", commas(args.map(NodePhrase)), ")"), 100)
    case InstanceFunctionInvocation(obj, ref, args, typeArgs, givenMap, yields) =>
      (phrase(assoc(100, obj), ".", name(ref.decl), "(", commas(args.map(NodePhrase)), ")"), 100)
    case UMinus(arg) =>
      (phrase("-", assoc(90, arg)), 90)
    case BitNot(arg) =>
      (phrase("~", assoc(90, arg)), 90)
    case Not(arg) =>
      (phrase("!", assoc(90, arg)), 90)
    case Exp(left, right) =>
      (phrase(bind(85, left), space, "^^", space, assoc(85, right)), 85)
    case AmbiguousPlus(left, right) =>
      (phrase(assoc(70, left), space, "+", space, assoc(70, right)), 70)
    case Plus(left, right) =>
      (phrase(assoc(70, left), space, "+", space, assoc(70, right)), 70)
    case JavaStringConcat(left, right) =>
      (phrase(assoc(70, left), space, "+", space, assoc(70, right)), 70)
    case Minus(left, right) =>
      (phrase(assoc(70, left), space, "-", space, bind(70, right)), 70)
    case AmbiguousMult(left, right) =>
      (phrase(assoc(80, left), space, "*", space, assoc(80, right)), 80)
    case Mult(left, right) =>
      (phrase(assoc(80, left), space, "*", space, assoc(80, right)), 80)
    case Div(left, right) =>
      (phrase(assoc(80, left), space, "\\", space, bind(80, right)), 80)
    case Mod(left, right) =>
      (phrase(assoc(80, left), space, "%", space, bind(80, right)), 80)
    case FloorDiv(left, right) =>
      (phrase(assoc(80, left), space, "/", space, bind(80, right)), 80)
    case BitAnd(left, right) =>
      (phrase(assoc(46, left), space, "&", space, assoc(46, right)), 46)
    case BitOr(left, right) =>
      (phrase(assoc(42, left), space, "|", space, assoc(42, right)), 42)
    case BitXor(left, right) =>
      (phrase(assoc(44, left), space, "|", space, assoc(44, right)), 44)
    case BitShl(left, right) =>
      (phrase(bind(65, left), space, "<<", space, bind(65, right)), 65)
    case BitShr(left, right) =>
      (phrase(bind(65, left), space, ">>", space, bind(65, right)), 65)
    case BitUShr(left, right) =>
      (phrase(bind(65, left), space, ">>>", space, bind(65, right)), 65)
    case And(left, right) =>
      (phrase(assoc(40, left), space, "&&", space, assoc(40, right)), 40)
    case AmbiguousOr(left, right) =>
      (phrase(assoc(30, left), space, "||", space, assoc(30, right)), 30)
    case Or(left, right) =>
      (phrase(assoc(30, left), space, "||", space, assoc(30, right)), 30)
    case Implies(left, right) =>
      (phrase(bind(30, left), space, "==>", space, assoc(30, right)), 30)
    case Eq(left, right) =>
      (phrase(bind(50, left), space, "==", space, bind(50, right)), 50)
    case Neq(left, right) =>
      (phrase(bind(50, left), space, "!=", space, bind(50, right)), 50)
    case Greater(left, right) =>
      (phrase(bind(60, left), space, ">", space, bind(60, right)), 60)
    case AmbiguousLess(left, right) =>
      (phrase(bind(60, left), space, "<", space, bind(60, right)), 60)
    case AmbiguousGreater(left, right) =>
      (phrase(bind(60, left), space, ">", space, bind(60, right)), 60)
    case Less(left, right) =>
      (phrase(bind(60, left), space, "<", space, bind(60, right)), 60)
    case AmbiguousGreaterEq(left, right) =>
      (phrase(bind(60, left), space, ">=", space, bind(60, right)), 60)
    case GreaterEq(left, right) =>
      (phrase(bind(60, left), space, ">=", space, bind(60, right)), 60)
    case LessEq(left, right) =>
      (phrase(bind(60, left), space, "<=", space, bind(60, right)), 60)
    case SubSet(left, right) =>
      ???
    case SubSetEq(left, right) =>
      ???
    case Unfolding(pred, body) =>
      (phrase("\\unfolding", pred, "\\in", body))
      ???
    case CurPerm(loc) =>
      (phrase("perm(", loc, ")"), 100)
    case Select(condition, whenTrue, whenFalse) =>
      (phrase(bind(20, condition), space, "?", space, bind(20, whenTrue), space, ":", space, assoc(20, whenFalse)), 20)
    case NewObject(cls) =>
      (phrase("new", space, name(cls.decl), "()"), 100)
    case NewArray(element, dims, moreDims) =>
      (phrase("new", space, element, phrase(dims.map(phrase("[", _, "]")):_*), "[]".repeat(moreDims)), 100)
    case Old(expr, at) =>
      (phrase("\\old(", expr, ")"), 100)
    case AmbiguousSubscript(collection, index) =>
      (phrase(assoc(100, collection), "[", index, "]"), 100)
    case SeqSubscript(seq, index) =>
      (phrase(assoc(100, seq), "[", index, "]"), 100)
    case ArraySubscript(arr, index) =>
      (phrase(assoc(100, arr), "[", index, "]"), 100)
    case PointerSubscript(pointer, index) =>
      (phrase(assoc(100, pointer), "[", index, "]"), 100)
    case PointerBlockOffset(pointer) =>
      (phrase("pointer_block(", pointer ,")"), 100)
    case PointerBlockLength(pointer) =>
      (phrase("block_length(", pointer ,")"), 100)
    case PointerLength(pointer) =>
      (phrase("pointer_length(", pointer ,")"), 100)
    case Cons(x, xs) =>
      (phrase(bind(87, x), space, "::", space, assoc(87, xs)), 87)
    case Head(xs) =>
      (phrase("head(", xs, ")"), 100)
    case Tail(xs) =>
      (phrase("tail(", xs, ")"), 100)
    case Drop(xs, count) =>
      (phrase(assoc(100, xs), "[", count, "..]"), 100)
    case Take(xs, count) =>
      (phrase(assoc(100, xs), "[..", count, "]"), 100)
    case Slice(xs, from, to) =>
      (phrase(assoc(100, xs), "[", from, "..", to, "]"), 100)
    case SeqUpdate(xs, i, x) =>
      (phrase(assoc(100, xs), "[", i, space, "->", space, x, "]"), 100)
    case Concat(xs, ys) =>
      (phrase(assoc(87, xs), space, "++", space, assoc(87, ys)), 87)
    case RemoveAt(xs, i) =>
      (phrase("removeAt(", xs, ",", space, i, ")"), 100)
    case AmbiguousMember(x, xs) =>
      (phrase("(", x, "\\memberof", xs, ")"), 120)
    case OptGet(opt) =>
      (phrase("getOption(", opt, ")"), 100)
    case OptGetOrElse(opt, alt) =>
      (phrase("getOrElseOption(", opt, ",", space, alt, ")"), 100)
    case MapGet(map, k) =>
      (phrase("getFromMap(", map, ",", space, k, ")"), 100)
    case TupGet(tup, index) =>
      (index match {
        case 0 => phrase("getFst(", tup, ")")
        case 1 => phrase("getSnd(", tup, ")")
        case _ => ???
      }, 100)
    case TypeValue(value) =>
      (phrase(value), 110)
    case TypeOf(expr) =>
      (phrase("\\typeof(", expr, ")"), 100)
    case InstanceOf(value, typeValue) =>
      (phrase(bind(55, value), space, "instanceof", space, bind(55, typeValue)), 55)
    case Cast(value, typeValue) =>
      syntax match {
        case C => (phrase("(", typeValue, ")", space, assoc(85, value)), 85)
        case Java => (phrase("(", typeValue, ")", space, assoc(97, value)), 97)
        case _ => ???
      }
    case SubType(left, right) => ???
    case SuperType(left, right) => ???
    case PreAssignExpression(target, value) =>
      (phrase(bind(10, target), space, "=", space, bind(10, value)), 10)
    case PostAssignExpression(target, value) =>
      (phrase(bind(10, target), space, "=", space, bind(10, value)), 10)
    case With(pre, value) => ???
    case Then(value, post) => ???
    case FunctionOf(v, vars) =>
      (phrase("(", name(v.decl), "!", commas(vars.map(ref => name(ref.decl))), ")"), 100)
  }

  def printType(t: Type[_]): Unit = say(t match {
    case CPrimitiveType(specifiers) =>
      spaced(specifiers.map(NodePhrase))
    case TUnion(types) =>
      intersperse(phrase(space, "|", space), types.map(NodePhrase))
    case JavaNamedType(names) =>
      intersperse(".", names.map(_._1).map(Text))
    case PVLNamedType(name, typeArgs) =>
      typeArgs match {
        case Nil => phrase(name)
        case some => phrase(name, "<", commas(some.map(NodePhrase)), ">")
      }
    case TVoid() => phrase("void")
    case TBool() => syntax(
      C -> phrase("_Bool"),
      Java -> phrase("boolean"),
      PVL -> phrase("boolean"),
      Silver -> phrase("Bool"),
    )
    case TFloat(exponent, mantissa) => phrase(s"float[$exponent, $mantissa]")
    case TChar() => phrase("char")
    case TString() => phrase("String")
    case TRef() => phrase("Ref")
    case TArray(element) => phrase(element, "[]")
    case TPointer(element) => phrase(element, "*")
    case TProcess() => phrase("process")
    case TModel(model) => phrase(name(model.decl))
    case TAxiomatic(adt, args) => phrase(name(adt.decl))
    case TOption(element) => phrase("option<", element, ">")
    case TTuple(Seq(t1, t2)) => phrase("tuple<", t1, ",", space, t2, ">")
    case TTuple(_) => ???
    case TSeq(element) => phrase("seq<", element, ">")
    case TSet(element) => phrase("set<", element, ">")
    case TBag(element) => phrase("bag<", element, ">")
    case TMap(key, value) => phrase("map<", key, ",", space, value, ">")
    /* case TVector(t) => phrase("vector<", t, ">") // FIXME does not parse (should it?) */
    case TMatrix(t) => phrase("matrix<", t, ">")
    case TType(t) => phrase(t)
    case t: TNotAValue[_] => s"<<Not a type: declaration ${t.decl.get.name}>>"
    case TAny() => phrase("any")
    case TNothing() => phrase("nothing")
    case TNull() => ???
    case TResource() => phrase("resource")
    case TInt() => phrase("int")
    case TBoundedInt(gte, lt) => phrase("{", gte.toString, "..", lt.toString, "}")
    case TRational() => phrase("rational")
    case TFraction() => phrase("frac")
    case TZFraction() => phrase("zfrac")
    case TClass(cls) => phrase(name(cls.decl))
    case TVar(ref) => phrase(name(ref.decl))
  })

  def printDeclaration(decl: Declaration[_]): Unit = say(decl match {
    case globalDecl: CGlobalDeclaration[_] =>
      val decl = globalDecl.decl
      phrase(doubleline,
        spec(decl.contract, clauses(decl.kernelInvariant, "kernel_invariant")),
        spaced(decl.specs.map(NodePhrase)), space, spaced(decl.inits.map(NodePhrase)),
        doubleline)
    case param: CParam[_] =>
      phrase(spaced(param.specifiers.map(NodePhrase)), space, param.declarator)
    case local: JavaLocalDeclaration[_] =>
      phrase(spaced(local.modifiers.map(NodePhrase)), space, local.t, space, javaDecls(local.decls))
    case defn: CFunctionDefinition[_] =>
      control(phrase(spaced(defn.specs.map(NodePhrase)), space, defn.declarator), defn.body)
    case ns: JavaNamespace[_] =>
      phrase(
        if(ns.pkg.nonEmpty) statement("package", space, ns.pkg.get) else phrase(),
        phrase(ns.imports.map(NodePhrase):_*),
        doubleline,
        phrase(ns.declarations.map(NodePhrase):_*),
      )
    case cls: JavaClass[_] =>
      phrase(
        doubleline,
        phrase(cls.modifiers.map(NodePhrase):_*), space, "class", space, cls.name, space,
        "extends", space, cls.ext, cls.imp match {
          case Nil => phrase()
          case ts => phrase("implements ", commas(ts.map(NodePhrase)))
        }, space, "{",
        indent(phrase(cls.decls.map(NodePhrase):_*)),
        "}",
        doubleline,
      )
    case int: JavaInterface[_] =>
      phrase(
        doubleline,
        phrase(int.modifiers.map(NodePhrase):_*), space, "interface", space, int.name,
        int.ext match {
          case Nil => phrase()
          case ts => phrase("extends ", commas(ts.map(NodePhrase)))
        }, space, "{",
        indent(phrase(int.decls.map(NodePhrase):_*)),
        "}",
        doubleline,
      )
    case field: SilverField[_] =>
      statement("field ", name(field), ": ", field.t)
    case rule: SimplificationRule[_] =>
      statement("axiom", space, name(rule), space, "{", newline, indent(rule.axiom), "}")
    case dataType: AxiomaticDataType[_] =>
      statement(s"axiomatic datatype ${dataType.o.preferredName} { ... omitted ... }")
    case function: Function[_] =>
      phrase(
        doubleline,
        spec(function.contract),
        if(function.inline) phrase("inline") else phrase(), space, "pure", space, function.returnType, space,
        name(function), "(", commas(function.args.map(NodePhrase)), ")",
        function.body match {
          case Some(body) => phrase(space, "=", newline, indent(body))
          case None => phrase(";")
        },
        doubleline,
      )
    case procedure: Procedure[_] =>
      val header = phrase(
        spec(procedure.contract),
        procedure.returnType, space, name(procedure), "(", commas(procedure.args.map(NodePhrase)), ")",
      )

      procedure.body match {
        case Some(body) =>  control(header, body)
        case None => phrase(doubleline, header, ";", doubleline)
      }
    case predicate: Predicate[_] =>
      val header = phrase(
        "resource", space, name(predicate), "(", commas(predicate.args.map(NodePhrase)), ")",
      )

      predicate.body match {
        case Some(body) => control(header, body)
        case None => phrase(doubleline, header, ";", doubleline)
      }
    case clazz: Class[_] =>
      phrase(
        doubleline,
        "class", space, name(clazz), space, "{",
        indent(phrase(clazz.declarations.map(NodePhrase):_*)),
        "}",
        doubleline,
      )
    case model: Model[_] =>
      phrase(
        doubleline,
        spec(
          "model", space, name(model), space, "{",
          indent(phrase(model.declarations.map(NodePhrase):_*)),
          "}"
        ),
        doubleline,
      )
    case init: JavaSharedInitialization[_] =>
      phrase(
        doubleline,
        if(init.isStatic) phrase("static", space) else phrase(),
        init.initialization,
        doubleline,
      )
    case fields: JavaFields[_] =>
      statement(spaced(fields.modifiers.map(NodePhrase)), space, fields.t, space, spaced(fields.decls.map {
        case JavaVariableDeclaration(name, dims, init) =>
          phrase(name, "[]".repeat(dims), if(init.isEmpty) phrase() else phrase(space, "=", space, init.get))
      }))
    case cons: JavaConstructor[_] =>
      control(
        phrase(cons.contract, spaced(cons.modifiers.map(NodePhrase)), space, cons.name, "(", commas(cons.parameters.map(NodePhrase)), ")"),
        cons.body,
      )
    case method: JavaMethod[_] =>
      val header = phrase(
        method.contract,
        spaced(method.modifiers.map(NodePhrase)), space, method.returnType, space, method.name, "[]".repeat(method.dims),
        "(", commas(method.parameters.map(NodePhrase)), ")",
      )

      method.body match {
        case Some(body) => control(header, body)
        case None => phrase(doubleline, header, ";", doubleline)
      }
    case function: InstanceFunction[_] =>
      val header = phrase(
        function.contract,
        if(function.inline) phrase("inline", space) else phrase(),
        "pure", space, function.returnType, space, name(function), "(", commas(function.args.map(NodePhrase)), ")",
      )

      function.body match {
        case Some(body) => phrase(doubleline, header, space, "=", space, newline, indent(body, ";"), doubleline)
        case None => phrase(doubleline, header, ";", doubleline)
      }
    case method: InstanceMethod[_] =>
      val header = phrase(
        method.contract,
        if(method.inline) phrase("inline", space) else phrase(),
        if(method.pure) phrase("pure", space) else phrase(),
        method.returnType, space, name(method), "(", commas(method.args.map(NodePhrase)), ")",
      )

      method.body match {
        case Some(body) => control(header, body)
        case None => phrase(doubleline, header, ";", doubleline)
      }
    case predicate: InstancePredicate[_] =>
      val header = phrase(
        if(predicate.threadLocal) phrase("thread_local") else phrase(),
        if(predicate.inline) phrase("inline", space) else phrase(),
        "resource", space, predicate.returnType, space, name(predicate), "(", commas(predicate.args.map(NodePhrase)), ")",
      )

      predicate.body match {
        case Some(body) => phrase(doubleline, header, space, "=", space, newline, indent(body, ";"), doubleline)
        case None => phrase(doubleline, header, ";", doubleline)
      }
    case field: InstanceField[_] =>
      statement(field.t, space, name(field))
    case variable: Variable[_] =>
      phrase(variable.t, space, name(variable))
    case decl: CLocalDeclaration[_] =>
      phrase(decl.decl)
    case decl: CGlobalDeclaration[_] =>
      phrase(decl.decl)
    case decl: LabelDecl[_] =>
      ???
    case decl: ParBlockDecl[_] =>
      ???
    case decl: ParInvariantDecl[_] =>
      ???
    case axiom: ADTAxiom[_] =>
      ???
    case function: ADTFunction[_] =>
      ???
    case process: ModelProcess[_] =>
      ???
    case action: ModelAction[_] =>
      ???
    case field: ModelField[_] =>
      ???
  })

  def printApplicableContract(node: ApplicableContract[_]): Unit =
    say(spec(
      phrase(node.givenArgs.map(v => phrase(newline, "given", space, v, ";", newline)):_*),
      clauses(node.contextEverywhere, "context_everywhere"),
      clauses(node.requires, "requires"),
      clauses(node.ensures, "ensures"),
      phrase(node.signals.map(NodePhrase):_*),
      phrase(node.yieldsArgs.map(v => phrase(newline, "yields", space, v, ";", newline)):_*),
    ))

  def printParBlock(parBlock: ParBlock[_], label: String): Unit = {
    val header = phrase(label, space, name(parBlock.decl), "(", commas(parBlock.iters.map(NodePhrase)), ")")
    val contract = spec(clauses(parBlock.requires, "requires"), clauses(parBlock.ensures, "ensures"))
    say(
      newline,
      header,
      newline,
      contract,
      newline,
      parBlock.content,
    )
  }

  def printCatchClause(catchClause: CatchClause[_]): Unit =
    ???

  def printSignalsClause(node: SignalsClause[_]): Unit =
    say(phrase(newline, "signals", space, "(", node.binding, ")", space, node.assn, ";", newline))

  def printFieldFlag(fieldFlag: FieldFlag[_]): Unit = fieldFlag match {
    case value: Final[_] => say("final")
  }

  def printIterVariable(iterVariable: IterVariable[_]): Unit =
    say(iterVariable.variable, space, "=", space, iterVariable.from, space, "..", space, iterVariable.to)

  def printCDeclarator(node: CDeclarator[_]): Unit = node match {
    case CPointerDeclarator(pointers, inner) =>
      say("*".repeat(pointers.size), inner)
    case CArrayDeclarator(qualifiers, Some(size), inner) =>
      say(inner, "[", size ,"]")
    case CArrayDeclarator(qualifiers, None, inner) =>
      say(inner, "[]")
    case CTypedFunctionDeclarator(params, varargs, inner) =>
      say(inner, "(", commas(params.map(NodePhrase)), ")")
    case CAnonymousFunctionDeclarator(params, inner) =>
      ???
    case CName(name) => say(name)
  }

  def printCDeclarationSpecifier(cDeclSpec: CDeclarationSpecifier[_]): Unit = cDeclSpec match {
    case CPure() => say("pure")
    case CInline() => say("inline")
    case CTypedef() => say("typedef")
    case CStatic() => say("static")
    case CVoid() => say("void")
    case CChar() => say("char")
    case CShort() => say("short")
    case CInt() => say("int")
    case CLong() => say("long")
    case CSigned() => say("signed")
    case CUnsigned() => say("unsigned")
    case CBool() => say("bool")
    case CTypedefName(name) => say(name)
    case CSpecificationType(t) => say(t)
    case CTypeQualifierDeclarationSpecifier(typeQual) => say(typeQual)
    case CExtern() => say("extern")
    case OpenCLKernel() => say("__kernel")
    case CUDAKernel() => say("__global__")
    case GPULocal() => say(syntax(
      Cuda -> phrase("__shared__"),
      OpenCL -> phrase("__local"),
    ))
    case GPUGlobal() => say(syntax(
      OpenCL -> phrase("__global"),
    ))
  }

  def printCTypeQualifier(node: CTypeQualifier[_]): Unit = node match {
    case CConst() => say("const")
    case CRestrict() => say("restrict")
    case CVolatile() => say("volatile")
    case CAtomic() => say("_Atomic")
  }

  def printCPointer(node: CPointer[_]): Unit =
    ???

  def printCInit(node: CInit[_]): Unit =
    node.init match {
      case Some(value) => say(node.decl, space, "=", space, value)
      case None => say(node.decl)
    }

  def printGpuMemoryFence(node: GpuMemoryFence[_]): Unit = node match {
    case GpuLocalMemoryFence() => say("CLK_LOCAL_MEM_FENCE")
    case GpuGlobalMemoryFence() => say("CLK_GLOBAL_MEM_FENCE")
    case GpuZeroMemoryFence(i) => say(f"$i")
  }

  def printJavaModifier(node: JavaModifier[_]): Unit = node match {
    case JavaPublic() => say("public")
    case JavaProtected() => say("protected")
    case JavaPrivate() => say("private")
    case JavaStatic() => say("static")
    case JavaAbstract() => say("abstract")
    case JavaFinal() => say("final")
    case JavaStrictFP() => say("strictfp")
    case JavaNative() => say("native")
    case JavaSynchronized() => say("synchronized")
    case JavaTransient() => say("transient")
    case JavaVolatile() => say("volatile")
    case JavaPure() => say(inlineSpec("pure"))
    case JavaInline() => say(inlineSpec("inline"))
  }

  def printJavaImport(node: JavaImport[_]): Unit =
    say(statement(
      "import", space,
      if(node.isStatic) phrase("static", space) else phrase(),
      node.name,
      if(node.star) phrase(".*") else phrase()))

  def printJavaName(node: JavaName[_]): Unit =
    say(node.names.mkString("."))

  def printLocation(loc: Location[_]): Unit = loc match {
//    case FieldLocation(obj, field) =>
//    case ModelLocation(obj, field) =>
//    case SilverFieldLocation(obj, field) =>
    case ArrayLocation(array, subscript) => (phrase(assoc(100, array), "[", subscript, "]"), 100)
    case PointerLocation(pointer) => say(pointer)
//    case PredicateLocation(predicate, args) =>
//    case InstancePredicateLocation(predicate, obj, args) =>
    case AmbiguousLocation(expr) => say(expr)
    case x =>
      say(s"Unknown node type in Printer.scala: ${x.getClass.getCanonicalName}")
  }

  def printVerification(node: Verification[_]): Unit =
    node.tasks.foreach(print)

  def printVerificationContext(node: VerificationContext[_]): Unit = {
    say(newline, "// === Verification context ===", newline)
    node.expectedErrors.foreach{ ee =>
      say(newline, s"""// Expected error "${ee.errorCode}" at ${ee.errorRegion.shortPosition}""", newline)
    }
    print(node.program)
  }

  def printCDeclaration(node: CDeclaration[_]): Unit = {
    say(newline, node.contract, newline)
    node.kernelInvariant match {
      case BooleanValue(true) =>
      case _ => say("kernel_invariant: ", node.kernelInvariant, space)
    }
    say(spaced(node.specs.map(NodePhrase)), space)
    say(spaced(node.inits.map(NodePhrase)))
  }

  def print(node: Node[_]): Unit = node match {
    case program: Program[_] => printProgram(program)
    case stat: Statement[_] => printStatement(stat)
    case e: Expr[_] => printExpr(e)
    case t: Type[_] => printType(t)
    case decl: Declaration[_] => printDeclaration(decl)
    case node: ApplicableContract[_] => printApplicableContract(node)
    case parBlock: ParBlock[_] => printParBlock(parBlock, "par")
    case catchClause: CatchClause[_] => printCatchClause(catchClause)
    case node: SignalsClause[_] => printSignalsClause(node)
    case fieldFlag: FieldFlag[_] => printFieldFlag(fieldFlag)
    case iterVariable: IterVariable[_] => printIterVariable(iterVariable)
    case node: CDeclarator[_] => printCDeclarator(node)
    case cDeclSpec: CDeclarationSpecifier[_] => printCDeclarationSpecifier(cDeclSpec)
    case node: CTypeQualifier[_] => printCTypeQualifier(node)
    case node: CPointer[_] => printCPointer(node)
    case node: CInit[_] => printCInit(node)
    case node: GpuMemoryFence[_] => printGpuMemoryFence(node)
    case node: JavaModifier[_] => printJavaModifier(node)
    case node: JavaImport[_] => printJavaImport(node)
    case node: JavaName[_] => printJavaName(node)
    case node : Location[_] => printLocation(node)
    case node: Verification[_] => printVerification(node)
    case node: VerificationContext[_] => printVerificationContext(node)
    case node: CDeclaration[_] => printCDeclaration(node)
    case x =>
      say(s"Unknown node type in Printer.scala: ${x.getClass.getCanonicalName}")
  }
}
