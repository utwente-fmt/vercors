package vct.parsers.transform

import hre.util.FuncTools
import org.antlr.v4.runtime.ParserRuleContext
import vct.col.ast._
import vct.antlr4.generated.JavaParser._
import vct.antlr4.generated.JavaParserPatterns._
import vct.col.{ast => col}
import vct.col.origin._
import vct.antlr4.generated.{JavaParserPatterns => parse}
import vct.col.ast.Constant._
import vct.col.resolve.Java

import scala.collection.mutable

case class JavaToCol(override val originProvider: OriginProvider, override val blameProvider: BlameProvider, override val errors: mutable.Map[(Int, Int), String])
  extends ToCol(originProvider, blameProvider, errors) {
  def convert(implicit unit: CompilationUnitContext): Seq[GlobalDeclaration] = unit match {
    case CompilationUnit0(pkg, imports, decls, _) =>
      Seq(new JavaNamespace(pkg.map(convert(_)), imports.map(convert(_)), decls.flatMap(convert(_))))
  }

  def convert(implicit pkg: PackageDeclarationContext): JavaName = pkg match {
    case PackageDeclaration0(_, _, name, _) => convert(name)
  }

  def convert(implicit imp: ImportDeclarationContext): JavaImport = imp match {
    case ImportDeclaration0(_, isStatic, name, star, _) => JavaImport(isStatic.nonEmpty, convert(name), star.nonEmpty)
  }

  def convert(implicit decl: TypeDeclarationContext): Seq[GlobalDeclaration] = decl match {
    case TypeDeclaration0(mods, ClassDeclaration0(_, name, args, ext, imp, ClassBody0(_, decls, _))) =>
      Seq(new JavaClass(convert(name), mods.map(convert(_)), args.map(convert(_)).getOrElse(Nil),
        ext.map(convert(_)).getOrElse(Java.JAVA_LANG_OBJECT),
        imp.map(convert(_)).getOrElse(Nil), decls.flatMap(convert(_))))
    case TypeDeclaration1(mods, enum) => fail(enum, "Enums are not supported.")
    case TypeDeclaration2(mods, InterfaceDeclaration0(_, name, args, ext, InterfaceBody0(_, decls, _))) =>
      Seq(new JavaInterface(convert(name), mods.map(convert(_)), args.map(convert(_)).getOrElse(Nil),
        ext.map(convert(_)).getOrElse(Nil), decls.flatMap(convert(_))))
    case TypeDeclaration3(_, annotation) => fail(annotation, "Annotations are note supported.")
    case TypeDeclaration4(inner) => convert(inner)
    case TypeDeclaration5(_) => Nil
  }

  def convert(implicit modifier: ModifierContext): JavaModifier = modifier match {
    case Modifier0(modifier) => convert(modifier)
    case Modifier1(name) => name match {
      case "native" => JavaNative()
      case "synchronized" => JavaSynchronized()
      case "transient" => JavaTransient()
      case "volatile" => JavaVolatile()
    }
    case Modifier2(mods) => withModifiers(mods, m => {
      if(m.consume(m.pure)) JavaPure()
      else if(m.consume(m.inline)) JavaInline()
      else fail(m.nodes.head, "This modifier cannot be attached to a declaration in Java")
    })
  }

  def convert(implicit modifier: ClassOrInterfaceModifierContext): JavaModifier = modifier match {
    case ClassOrInterfaceModifier0(annotation) => ??(annotation)
    case ClassOrInterfaceModifier1(name) => name match {
      case "public" => JavaPublic()
      case "protected" => JavaProtected()
      case "private" => JavaPrivate()
      case "static" => JavaStatic()
      case "abstract" => JavaAbstract()
      case "final" => JavaFinal()
      case "strictfp" => JavaStrictFP()
    }
  }

  def convert(implicit modifier: VariableModifierContext): JavaModifier = modifier match {
    case VariableModifier0(_) => JavaFinal()
    case VariableModifier1(annotation) => ??(annotation)
  }

  def convert(implicit args: TypeParametersContext): Seq[Variable] = args match {
    case TypeParameters0(_, args, _) => convert(args)
  }

  def convert(implicit args: TypeParameterListContext): Seq[Variable] = args match {
    case TypeParameterList0(arg) => Seq(convert(arg))
    case TypeParameterList1(arg, _, args) => convert(arg) +: convert(args)
  }

  def convert(implicit arg: TypeParameterContext): Variable = arg match {
    case TypeParameter0(id, bound) => bound match {
      case None =>
        new Variable(TType(Java.JAVA_LANG_OBJECT))(SourceNameOrigin(convert(id), origin(arg)))
      case Some(TypeParameterBound0(_, TypeBound0(ext))) =>
        new Variable(TType(convert(ext)))(SourceNameOrigin(convert(id), origin(arg)))
      case Some(TypeParameterBound0(_, TypeBound1(_, _, moreBounds))) =>
        fail(moreBounds, "Multiple upper type bounds are not supported")
    }
  }

  def convert(implicit decl: ClassBodyDeclarationContext): Seq[ClassDeclaration] = decl match {
    case ClassBodyDeclaration0(_) => Nil
    case ClassBodyDeclaration1(isStatic, body) => Seq(new JavaSharedInitialization(isStatic.nonEmpty, convert(body)))
    case ClassBodyDeclaration2(contract, mods, decl) =>
      withContract(contract, c => {
        convert(decl, mods.map(convert(_)), c)
      })
    case ClassBodyDeclaration3(inner) => convert(inner)
  }

  def convert(implicit decl: InterfaceBodyDeclarationContext): Seq[ClassDeclaration] = decl match {
    case InterfaceBodyDeclaration0(contract, mods, decl) =>
      withContract(contract, c => {
        convert(decl, mods.map(convert(_)), c)
      })
    case InterfaceBodyDeclaration1(inner) => convert(inner)
    case InterfaceBodyDeclaration2(_) => Nil
  }

  def convert(implicit decl: MemberDeclarationContext, mods: Seq[JavaModifier], c: ContractCollector): Seq[ClassDeclaration] = decl match {
    case MemberDeclaration0(MethodDeclaration0(returnType, name, params, dims, signals, body)) =>
      Seq(new JavaMethod(mods, convert(returnType), dims.map(convert(_)).getOrElse(0),
        convert(name), convert(params), Nil, signals.map(convert(_)).getOrElse(Nil),
        convert(body), c.consumeApplicableContract())(blame(decl)))
    case MemberDeclaration1(GenericMethodDeclaration0(typeParams, MethodDeclaration0(
      returnType, name, params, dims, signals, body))) =>
      Seq(new JavaMethod(mods, convert(returnType), dims.map(convert(_)).getOrElse(0),
        convert(name), convert(params), convert(typeParams), signals.map(convert(_)).getOrElse(Nil),
        convert(body), c.consumeApplicableContract())(blame(decl)))
    case MemberDeclaration2(FieldDeclaration0(t, decls, _)) =>
      // Ignore the contract collector, so that complains about being non-empty
      Seq(new JavaFields(mods, convert(t), convert(decls)))
    case MemberDeclaration3(ConstructorDeclaration0(name, params, signals, ConstructorBody0(body))) =>
      Seq(new JavaConstructor(mods, convert(name), convert(params), Nil,
        signals.map(convert(_)).getOrElse(Nil), convert(body), c.consumeApplicableContract()))
    case MemberDeclaration4(GenericConstructorDeclaration0(typeParams,
      ConstructorDeclaration0(name, params, signals, ConstructorBody0(body)))) =>
      Seq(new JavaConstructor(mods, convert(name), convert(params), convert(typeParams),
        signals.map(convert(_)).getOrElse(Nil), convert(body), c.consumeApplicableContract()))
    case MemberDeclaration5(interface) => fail(interface, "Inner interfaces are not supported.")
    case MemberDeclaration6(annotation) => fail(annotation, "Annotations are not supported.")
    case MemberDeclaration7(cls) => fail(cls, "Inner classes are not supported.")
    case MemberDeclaration8(enum) => fail(enum, "Enums are not supported.")
  }

  def convert(implicit decl: InterfaceMemberDeclarationContext, mods: Seq[JavaModifier], c: ContractCollector): Seq[ClassDeclaration] = decl match {
    case InterfaceMemberDeclaration0(ConstDeclaration0(t, decls, _)) =>
      // JLS SE 7 - 9.3
      Seq(new JavaFields(Seq(JavaPublic(), JavaStatic(), JavaFinal()) ++ mods, convert(t), convert(decls)))
    case InterfaceMemberDeclaration1(InterfaceMethodDeclaration0(t, name, params, dims, signals, _)) =>
      // JLS SE 7 - 9.4
      Seq(new JavaMethod(Seq(JavaPublic(), JavaAbstract()) ++ mods, convert(t), dims.map(convert(_)).getOrElse(0),
        convert(name), convert(params), Nil, signals.map(convert(_)).getOrElse(Nil), None, c.consumeApplicableContract())(blame(decl)))
    case InterfaceMemberDeclaration2(GenericInterfaceMethodDeclaration0(typeParams, InterfaceMethodDeclaration0(
      t, name, params, dims, signals, _))) =>
      Seq(new JavaMethod(Seq(JavaPublic(), JavaAbstract()) ++ mods, convert(t), dims.map(convert(_)).getOrElse(0),
        convert(name), convert(params), convert(typeParams), signals.map(convert(_)).getOrElse(Nil),
        None, c.consumeApplicableContract())(blame(decl)))
    case InterfaceMemberDeclaration3(interface) => fail(interface, "Inner interfaces are not supported.")
    case InterfaceMemberDeclaration4(annotation) => fail(annotation, "Annotations are not supported.")
    case InterfaceMemberDeclaration5(cls) => fail(cls, "Inner classes are not supported.")
    case InterfaceMemberDeclaration6(enum) => fail(enum, "Enums are not supported.")
  }

  def convert(implicit decls: VariableDeclaratorsContext): Seq[(String, Int, Option[Expr])] = decls match {
    case VariableDeclarators0(decl) => Seq(convert(decl))
    case VariableDeclarators1(decl, _, decls) => convert(decl) +: convert(decls)
  }

  def convert(implicit decl: VariableDeclaratorContext): (String, Int, Option[Expr]) = decl match {
    case VariableDeclarator0(VariableDeclaratorId0(name, dims), init) =>
      (convert(name), dims.map(convert(_)).getOrElse(0), init.map(convert(_)))
  }

  def convert(implicit decls: ConstantDeclaratorListContext): Seq[(String, Int, Option[Expr])] = decls match {
    case ConstantDeclaratorList0(decl) => Seq(convert(decl))
    case ConstantDeclaratorList1(decl, _, decls) => convert(decl) +: convert(decls)
  }

  def convert(implicit decl: ConstantDeclaratorContext): (String, Int, Option[Expr]) = decl match {
    case ConstantDeclarator0(name, dims, _, init) =>
      (convert(name), dims.map(convert(_)).getOrElse(0), Some(convert(init)))
  }

  def convert(implicit params: FormalParametersContext): Seq[Variable] = params match {
    case FormalParameters0(_, params, _) => params.map(convert(_)).getOrElse(Nil)
  }

  def convert(implicit params: FormalParameterListContext): Seq[Variable] = params match {
    case FormalParameterList0(varargs) => ??(varargs)
    case FormalParameterList1(params) => convert(params)
    case FormalParameterList2(_, _, varargs) => ??(varargs)
  }

  def convert(implicit params: InitFormalParameterListContext): Seq[Variable] = params match {
    case InitFormalParameterList0(param) => Seq(convert(param))
    case InitFormalParameterList1(param, _, params) => convert(param) +: convert(params)
  }

  def convert(implicit param: FormalParameterContext): Variable = param match {
    case FormalParameter0(_, tNode, nameDims) =>
      val (name, dims) = convert(nameDims)
      val t = FuncTools.repeat(TArray(_), dims, convert(tNode))
      new Variable(t)(SourceNameOrigin(name, origin(param)))
  }

  def convert(implicit dims: DimsContext): Int = dims match {
    case Dims0(dims) => dims.size
  }

  def convert(implicit names: QualifiedNameListContext): Seq[JavaName] = names match {
    case QualifiedNameList0(name) => Seq(convert(name))
    case QualifiedNameList1(name, _, names) => convert(name) +: convert(names)
  }

  def convert(implicit name: QualifiedNameContext): JavaName = name match {
    case QualifiedName0(id) => JavaName(Seq(convert(id)))
    case QualifiedName1(id, _, names) => JavaName(convert(id) +: convert(names).names)
  }

  def convert(implicit ts: IntExtContext): Seq[Type] = ts match {
    case IntExt0(_, ts) => convert(ts)
  }

  def convert(implicit t: ExtContext): Type = t match {
    case Ext0(_, t) => convert(t)
  }

  def convert(implicit ts: ImpContext): Seq[Type] = ts match {
    case Imp0(_, ts) => convert(ts)
  }

  def convert(implicit ts: TypeListContext): Seq[Type] = ts match {
    case TypeList0(t) => Seq(convert(t))
    case TypeList1(t, _, ts) => convert(t) +: convert(ts)
  }

  def convert(implicit signals: ThrowyContext): Seq[JavaName] = signals match {
    case Throwy0(_, signals) => convert(signals)
  }

  def convert(implicit stat: MethodBodyOrEmptyContext): Option[Statement] = stat match {
    case MethodBodyOrEmpty0(_) => None
    case MethodBodyOrEmpty1(MethodBody0(body)) => Some(convert(body))
  }

  def convert(implicit stat: BlockContext): Statement = stat match {
    case Block0(_, stats, _) => Scope(Nil, Block(stats.map(convert(_))))
  }

  def convert(implicit stat: BlockStatementContext): Statement = stat match {
    case BlockStatement0(decl) => convert(decl)
    case BlockStatement1(statement) => convert(statement)
    case BlockStatement2(decl) => ??(decl)
    case BlockStatement3(inner) => convert(inner)
  }

  def convert(implicit decl: LocalVariableDeclarationStatementContext): Statement = decl match {
    case LocalVariableDeclarationStatement0(decl, _) => convert(decl)
  }

  def convert(implicit decl: LocalVariableDeclarationContext): Statement = decl match {
    case LocalVariableDeclaration0(mods, t, decls) =>
      JavaLocalDeclarationStatement(new JavaLocalDeclaration(mods.map(convert(_)), convert(t), convert(decls)))
  }

  def convert(implicit stat: ElseBlockContext): Statement = stat match {
    case ElseBlock0(_, stat) => convert(stat)
  }

  def convert(implicit stat: StatementContext): Statement = stat match {
    case Statement0(block) => convert(block)
    case Statement1(_, assn, _, _) => Assert(convert(assn))(blame(stat))
    case Statement2(_, cond, body, otherwise) =>
      Branch(Seq((convert(cond), convert(body))) ++ (otherwise match {
        case None => Nil
        case Some(otherwise) => Seq((BooleanValue(true), convert(otherwise)))
      }))
    case Statement3(contract1, label, _, _, control, _, contract2, body) =>
      val loop = withContract(contract1, contract2, c => {
        control match {
          case ForControl0(foreach) => ??(foreach)
          case ForControl1(init, _, cond, _, update) =>
            Scope(Nil, Loop(
              init.map(convert(_)).getOrElse(Block(Nil)),
              cond.map(convert(_)).getOrElse(true),
              update.map(convert(_)).getOrElse(Block(Nil)),
              c.consumeLoopContract(),
              convert(body)
            ))
        }
      })

      label match {
        case None => loop
        case Some(label) => Block(Seq(convert(label), loop))
      }
    case Statement4(contract1, label, _, cond, contract2, body) =>
      val loop = withContract(contract1, contract2, c => {
        Scope(Nil, Loop(Block(Nil), convert(cond), Block(Nil), c.consumeLoopContract(), convert(body)))
      })

      label match {
        case None => loop
        case Some(label) => Block(Seq(convert(label), loop))
      }
    case Statement5(_, _, _, _, _) => ??(stat)
    case Statement6(_, attempt, grab, eventually) =>
      TryCatchFinally(convert(attempt), eventually.map(convert(_)).getOrElse(Block(Nil)), grab.map(convert(_)))
    case Statement7(_, attempt, eventually) =>
      TryCatchFinally(convert(attempt), convert(eventually), Nil)
    case Statement8(_, _, _, _, _) => ??(stat)
    case Statement9(_, expr, _, casedStatements, trailingCases, _) =>
      Switch(convert(expr), Block(casedStatements.flatMap(convert(_)) ++ trailingCases.map(convert(_))))
    case Statement10(_, obj, inner) => Synchronized(convert(obj), convert(inner))
    case Statement11(_, expr, _) => Return(expr.map(convert(_)).getOrElse(Void()))
    case Statement12(_, exc, _) => Throw(convert(exc))
    case Statement13(_, label, _) => Break(label.map(convert(_)).map(new UnresolvedRef[LabelDecl](_)))
    case Statement14(_, label, _) => Continue(label.map(convert(_)).map(new UnresolvedRef[LabelDecl](_)))
    case Statement15(_) => Block(Nil)
    case Statement16(expr, _) => Eval(convert(expr))
    case Statement17(label, _, statement) => Block(Seq(
      Label(new LabelDecl()(SourceNameOrigin(convert(label), origin(stat)))),
      convert(statement),
    ))
    case Statement18(inner) => convert(inner)
  }

  def convert(implicit label: LoopLabelContext): Statement = label match {
    case LoopLabel0(name, _) => Label(new LabelDecl()(SourceNameOrigin(convert(name), origin(label))))
  }

  def convert(implicit switchBlock: SwitchBlockStatementGroupContext): Seq[Statement] = switchBlock match {
    case SwitchBlockStatementGroup0(labels, statements) =>
      labels.map(convert(_)) ++ statements.map(convert(_))
  }

  def convert(implicit switchLabel: SwitchLabelContext): Statement = switchLabel match {
    case SwitchLabel0(_, expr, _) => Case(convert(expr))
    case SwitchLabel1(_, enum, _) => ??(enum)
    case SwitchLabel2(_, _) => DefaultCase()
  }

  def convert(implicit stat: ForInitContext): Statement = stat match {
    case ForInit0(locals) => convert(locals)
    case ForInit1(exprs) => Block(convert(exprs).map(Eval(_)))
  }

  def convert(implicit stat: ForUpdateContext): Statement = stat match {
    case ForUpdate0(exprs) => Block(convert(exprs).map(Eval(_)))
  }

  def convert(implicit stat: FinallyBlockContext): Statement = stat match {
    case FinallyBlock0(_, block) => convert(block)
  }

  def convert(implicit grab: CatchClauseContext): CatchClause = grab match {
    case CatchClause0(_, _, _mods, ts, id, _, body) =>
      CatchClause(new Variable(convert(ts))(SourceNameOrigin(convert(id), origin(grab))), convert(body))
  }

  def convert(implicit ts: CatchTypeContext): TUnion = ts match {
    case CatchType0(name) => TUnion(Seq(convert(name)))
    case CatchType1(name, _, names) => TUnion(convert(name) +: convert(names).types)
  }

  def convert(implicit ts: NonWildcardTypeArgumentsContext): Seq[Type] = ts match {
    case NonWildcardTypeArguments0(_, ts, _) => convert(ts)
  }

  def convert(implicit t: TypeOrVoidContext): Type = t match {
    case TypeOrVoid0(_) => TVoid()
    case TypeOrVoid1(t) => convert(t)
  }

  def convert(implicit t: TypeContext): Type = t match {
    case Type0(inner) => convert(inner)
    case Type1(element, dims) => FuncTools.repeat(TArray(_), dims.map(convert(_)).getOrElse(0), convert(element))
    case Type2(element, dims) => FuncTools.repeat(TArray(_), dims.map(convert(_)).getOrElse(0), convert(element))
  }

  def convert(implicit t: ClassOrInterfaceTypeContext): JavaNamedType = t match {
    case ClassOrInterfaceType0(name, args) =>
      JavaNamedType(Seq((convert(name), args.map(convert(_)))))
    case ClassOrInterfaceType1(names, _, name, args) =>
      JavaNamedType(convert(names).names :+ (convert(name), args.map(convert(_))))
  }

  def convert(implicit t: CreatedNameContext): Type = t match {
    case CreatedName0(t) => convert(t)
    case CreatedName1(t) => convert(t)
  }

  def convert(implicit t: ClassTypeDiamondListContext): JavaNamedType = t match {
    case ClassTypeDiamondList0(name, typeArgs) =>
      JavaNamedType(Seq((convert(name), typeArgs.map(convert(_)))))
    case ClassTypeDiamondList1(name, typeArgs, _, more) =>
      JavaNamedType((convert(name), typeArgs.map(convert(_))) +: convert(more).names)
  }

  def convert(implicit ts: TypeArgumentsOrDiamondContext): Seq[Type] = ts match {
    case TypeArgumentsOrDiamond0(_, _) => Nil
    case TypeArgumentsOrDiamond1(ts) => convert(ts)
  }

  def convert(implicit t: PrimitiveTypeContext): Type = t match {
    case PrimitiveType0(name) => name match {
      case "boolean" => TBool()
      case "char" => ??(t)
      case "byte" => TInt()
      case "short" => TInt()
      case "int" => TInt()
      case "long" => TInt()
      case "float" => TFloat()
      case "double" => TFloat()
    }
  }

  def convert(implicit ts: TypeArgumentsContext): Seq[Type] = ts match {
    case TypeArguments0(_, ts, _) => convert(ts)
  }

  def convert(implicit ts: TypeArgumentListContext): Seq[Type] = ts match {
    case TypeArgumentList0(arg) => Seq(convert(arg))
    case TypeArgumentList1(arg, _, args) => convert(arg) +: convert(args)
  }

  def convert(implicit t: TypeArgumentContext): Type = t match {
    case TypeArgument0(t) => convert(t)
    case other: TypeArgument1Context => ??(other)
  }

  def convert(implicit expr: VariableDeclaratorInitContext): Expr = expr match {
    case VariableDeclaratorInit0(_, init) => convert(init)
  }

  def convert(implicit expr: VariableInitializerContext): Expr = expr match {
    case VariableInitializer0(arrayInit) => convert(arrayInit)
    case VariableInitializer1(expr) => convert(expr)
  }

  def convert(implicit expr: ArrayInitializerContext): Expr = expr match {
    case ArrayInitializer0(_, _) => JavaLiteralArray(Nil)
    case ArrayInitializer1(_, exprs, _, _) => JavaLiteralArray(convert(exprs))
  }

  def convert(implicit expr: ParExpressionContext): Expr = expr match {
    case ParExpression0(_, expr, _) => convert(expr)
  }

  def convert(implicit exprs: ExpressionListContext): Seq[Expr] = exprs match {
    case ExpressionList0(expr) => Seq(convert(expr))
    case ExpressionList1(expr, _, exprs) => convert(expr) +: convert(exprs)
  }

  def convert(implicit exprs: VariableInitializerListContext): Seq[Expr] = exprs match {
    case VariableInitializerList0(expr) => Seq(convert(expr))
    case VariableInitializerList1(expr, _, exprs) => convert(expr) +: convert(exprs)
  }

  def convert(implicit exprs: SpecifiedDimsContext): Seq[Expr] = exprs match {
    case SpecifiedDims0(dim) => Seq(convert(dim))
    case SpecifiedDims1(dim, dims) => convert(dim) +: convert(dims)
  }

  def convert(implicit exprs: ArgumentsContext): Seq[Expr] = exprs match {
    case Arguments0(_, exprs, _) => exprs.map(convert(_)).getOrElse(Nil)
  }

  def convert(implicit expr: ConstantExpressionContext): Expr = expr match {
    case ConstantExpression0(expr) => convert(expr)
  }

  def convert(implicit expr: StatementExpressionContext): Expr = expr match {
    case StatementExpression0(expr) => convert(expr)
  }

  def convert(implicit expr: SpecifiedDimContext): Expr = expr match {
    case SpecifiedDim0(_, expr, _) => convert(expr)
  }

  def convert(implicit expr: ExpressionContext): Expr = expr match {
    case Expression0(whiff, inner, den) =>
      convertEmbedWith(whiff, convertEmbedThen(den, convert(inner)))
  }

  def convert(implicit expr: ExprContext): Expr = expr match {
    case JavaValPrimary(inner) => convert(inner)
    case JavaPrimary(inner) => convert(inner)
    case parse.JavaDeref(obj, _, field) => col.JavaDeref(convert(obj), convert(field))(blame(expr))
    case JavaPinnedThis(innerOrOuterClass, _, _) => ??(expr)
    case JavaPinnedOuterClassNew(pinnedOuterClassObj, _, _, _, _) => ??(expr)
    case JavaSuper(_, _, _, _) => ??(expr)
    case JavaGenericInvocation(obj, _, ExplicitGenericInvocation0(typeArgs, invocation)) =>
      convert(invocation, Some(convert(obj)), convert(typeArgs))
    case JavaSubscript(ar, _, idx, _) => AmbiguousSubscript(convert(ar), convert(idx))
    case JavaNonNullInvocation(obj, _, name, args) =>
      Implies(
        Neq(convert(obj), Null()),
        col.JavaInvocation(Some(convert(obj)), Nil, convert(name), convert(args), Nil, Nil)(blame(expr)),
      )
    case parse.JavaInvocation(obj, _, name, familyType, given, args, yields) =>
      failIfDefined(familyType, "Predicate families not supported (for now)")
      col.JavaInvocation(
        Some(convert(obj)), Nil, convert(name), convert(args),
        convertEmbedGiven(given), convertEmbedYields(yields))(
        blame(expr))
    case JavaValPostfix(expr, PostfixOp0(valPostfix)) => convert(valPostfix, convert(expr))
    case JavaNew(given, _, creator, yields) =>
      convert(creator)
    case JavaCast(_, t, _, inner) => Cast(convert(inner), TypeValue(convert(t)))
    case JavaPostfixIncDec(inner, postOp) =>
      val target = convert(inner)
      postOp match {
        case "++" => PostAssignExpression(target, Plus(target, 1))
        case "--" => PostAssignExpression(target, Minus(target, 1))
      }
    case JavaPrefixOp(preOp, inner) =>
      val target = convert(inner)
      preOp match {
        case "+" => target // TODO PB: not sure if this is true for IEEE floats
        case "-" => UMinus(target)
        case "++" => PreAssignExpression(target, Plus(target, 1))
        case "--" => PreAssignExpression(target, Minus(target, 1))
      }
    case JavaPrefixOp2(preOp, inner) => preOp match {
      case "~" => BitNot(convert(inner))
      case "!" => Not(convert(inner))
    }
    case JavaMul(leftNode, mul, rightNode) =>
      val (left, right) = (convert(leftNode), convert(rightNode))
      mul match {
        case MulOp0(op) => op match {
          case "*" => AmbiguousMult(left, right)
          case "/" => FloorDiv(left, right)(blame(expr))
          case "%" => Mod(left, right)(blame(expr))
        }
        case MulOp1(specOp) => convert(specOp, left, right)
      }
    case JavaAdd(left, op, right) => op match {
      case "+" => AmbiguousPlus(convert(left), convert(right))
      case "-" => Minus(convert(left), convert(right))
    }
    case JavaShift(left, shift, right) => shift match {
      case ShiftOp0(_, _) => BitShl(convert(left), convert(right))
      case ShiftOp1(_, _, _) => BitUShr(convert(left), convert(right))
      case ShiftOp2(_, _) => BitShr(convert(left), convert(right))
    }
    case JavaRel(left, comp, right) => comp match {
      case RelOp0("<=") => LessEq(convert(left), convert(right))
      case RelOp0(">=") => GreaterEq(convert(left), convert(right))
      case RelOp0(">") => Greater(convert(left), convert(right))
      case RelOp0("<") => Less(convert(left), convert(right))
      case RelOp1(valOp) => convert(valOp, convert(left), convert(right))
    }
    case JavaInstanceOf(obj, _, t) => InstanceOf(convert(obj), TypeValue(convert(t)))
    case JavaEquals(left, eq, right) => eq match {
      case "==" => Eq(convert(left), convert(right))
      case "!=" => Neq(convert(left), convert(right))
    }
    case JavaBitAnd(left, _, right) => AmbiguousComputationalAnd(convert(left), convert(right))
    case JavaBitXor(left, _, right) => AmbiguousComputationalXor(convert(left), convert(right))
    case JavaBitOr(left, _, right) => AmbiguousComputationalOr(convert(left), convert(right))
    case JavaAnd(left, and, right) => and match {
      case AndOp0(_) => And(convert(left), convert(right))
      case AndOp1(specOp) => convert(specOp, convert(left), convert(right))
    }
    case JavaOr(left, _, right) => AmbiguousOr(convert(left), convert(right))
    case JavaValImp(left, imp, right) => imp match {
      case ImpOp0(specOp) => convert(specOp, convert(left), convert(right))
    }
    case JavaSelect(cond, _, whenTrue, _, whenFalse) =>
      Select(convert(cond), convert(whenTrue), convert(whenFalse))
    case JavaAssign(left, AssignOp0(op), right) =>
      val target = convert(left)
      val value = convert(right)
      PreAssignExpression(target, op match {
        case "=" => value
        case "+=" => Plus(target, value)
        case "-=" => Minus(target, value)
        case "*=" => Mult(target,  value)
        case "/=" => FloorDiv(target,  value)(blame(expr))
        case "&=" => BitAnd(target, value)
        case "|=" => BitOr(target, value)
        case "^=" => BitXor(target, value)
        case ">>=" => BitShr(target, value)
        case ">>>=" => BitUShr(target, value)
        case "<<=" => BitShl(target, value)
        case "%=" => Mod(target, value)(blame(expr))
      })
  }

  def convert(implicit invocation: ExplicitGenericInvocationSuffixContext,
              obj: Option[Expr], typeArgs: Seq[Type]): Expr = invocation match {
    case ExplicitGenericInvocationSuffix0(_, _) => ??(invocation)
    case ExplicitGenericInvocationSuffix1(name, familyType, arguments) =>
      // FIXME PB: should support this again somehow, maybe reuse open/close with type syntax instead of fold/unfold?
      failIfDefined(familyType, "Predicate families not supported (for now)")
      col.JavaInvocation(obj, typeArgs, convert(name), convert(arguments), Nil, Nil)(blame(invocation))
  }

  def convert(implicit expr: CreatorContext): Expr = expr match {
    case Creator0(typeArgs, name, creator) =>
      convert(creator, convert(typeArgs), convert(name))
    case Creator1(name, creator) => creator match {
      case CreatorRest0(array) => convert(array, convert(name))
      case CreatorRest1(cls) => convert(cls, Nil, convert(name))
    }
  }

  def convert(implicit creator: ClassCreatorRestContext, ts: Seq[Type], name: Type): Expr = creator match {
    case ClassCreatorRest0(args, impl) =>
      failIfDefined(impl, "Anonymous classes are not supported")
      JavaNewClass(convert(args), ts, name)(blame(creator))
  }

  def convert(implicit creator: ArrayCreatorRestContext, name: Type): Expr = creator match {
    case ArrayCreatorRest0(dims, init) => JavaNewLiteralArray(name, convert(dims), convert(init))
    case ArrayCreatorRest1(specDims, extraDims) => JavaNewDefaultArray(name, convert(specDims), extraDims.map(convert(_)).getOrElse(0))
  }

  def convert(implicit expr: AnnotatedPrimaryContext): Expr = expr match {
    case AnnotatedPrimary0(pre, inner, post) =>
      convertEmbedWith(pre, convertEmbedThen(post, convert(inner)))
  }

  def convert(implicit expr: PrimaryContext): Expr = expr match {
    case Primary0(_, expr, _) => convert(expr)
    case Primary1(_) => AmbiguousThis()
    case Primary2(_) => ??(expr)
    case Primary3(literal) => convert(literal)
    case Primary4(name) => name match {
      case JavaIdentifier0(specInSpec) => convert(specInSpec)
      case JavaIdentifier1(name) => JavaLocal(name)(blame(expr))
      case JavaIdentifier2(_) => JavaLocal(convert(name))(blame(expr))
    }
    case Primary5(name, familyType, given, args, yields) =>
      failIfDefined(familyType, "Predicate families are unsupported (for now)")
      col.JavaInvocation(None, Nil, convert(name), convert(args),
        convertEmbedGiven(given), convertEmbedYields(yields))(
        blame(expr))
    case Primary6(_, _, _) => ??(expr)
    case Primary7(_, _, _) => ??(expr)
    case _: Primary8Context => ??(expr)
  }

  def convert(implicit expr: LiteralContext): Expr = expr match {
    case Literal0(i) => Integer.parseInt(i)
    case Literal1(_) => ??(expr)
    case Literal2(_) => ??(expr)
    case Literal3(_) => ??(expr)
    case Literal4(value) => value match {
      case "true" => true
      case "false" => false
    }
    case Literal5(_) => Null()
  }

  def convert(implicit id: VariableDeclaratorIdContext): (String, Int) = id match {
    case VariableDeclaratorId0(name, Some(dims)) => (convert(name), convert(dims))
    case VariableDeclaratorId0(name, None) => (convert(name), 0)
  }

  def convert(implicit id: JavaIdentifierContext): String = id match {
    case JavaIdentifier0(specInSpec) => specInSpec match {
      case ValIdEscape(id) => id.substring(1, id.length-1)
      case other => fail(other,
        f"This identifier is reserved, and cannot be declared or used in specifications. " +
          f"You might want to escape the identifier with backticks: `${other.getText}`")
    }
    case JavaIdentifier1(id) => id
    case JavaIdentifier2(specOutOfSpec) =>
      val text = specOutOfSpec.getText
      if(text.matches("[a-zA-Z_]+")) text
      else fail(specOutOfSpec, f"This identifier is not allowed in Java.")
  }

  def convert(implicit decl: LangGlobalDeclContext): Seq[GlobalDeclaration] = decl match {
    case LangGlobalDecl0(typeDecl) => convert(typeDecl)
  }

  def convert(implicit decl: LangClassDeclContext): Seq[ClassDeclaration] = decl match {
    case LangClassDecl0(classDecl) => convert(classDecl)
  }

  def convert(implicit stat: LangStatementContext): Statement = stat match {
    case LangStatement0(block) => convert(block)
  }

  def convert(implicit t: LangTypeContext): Type = t match {
    case LangType0(t) => convert(t)
  }

  def convert(implicit e: LangExprContext): Expr = e match {
    case LangExpr0(e) => convert(e)
  }

  def convert(implicit id: LangIdContext): String = id match {
    case LangId0(id) => convert(id)
  }

  def local(ctx: ParserRuleContext, name: String): Expr =
    JavaLocal(name)(blame(ctx))(origin(ctx))

  def withCollector[T](collector: ContractCollector, f: ContractCollector => T): T = {
    val result = f(collector)
    collector.nodes.headOption match {
      case Some(node) => fail(node, "This specification clause may not occur here")
      case None => result
    }
  }

  def withContract[T](node: Option[ValEmbedContractContext], f: ContractCollector => T): T = {
    val collector = new ContractCollector()
    node.foreach(convert(_, collector))
    withCollector(collector, f)
  }

  def withContract[T](node1: Option[ValEmbedContractContext], node2: Option[ValEmbedContractContext], f: ContractCollector => T): T = {
    val collector = new ContractCollector()
    node1.foreach(convert(_, collector))
    node2.foreach(convert(_, collector))
    withCollector(collector, f)
  }

  def withContract[T](node: Seq[ValContractClauseContext], f: ContractCollector => T): T = {
    val collector = new ContractCollector()
    node.foreach(convert(_, collector))
    withCollector(collector, f)
  }

  def withCollector[T](collector: ModifierCollector, f: ModifierCollector => T): T = {
    val result = f(collector)
    collector.nodes.headOption match {
      case Some(node) => fail(node, "This modifier cannot be attached to this declaration")
      case None => result
    }
  }

  def withModifiers[T](node: Seq[ValModifierContext], f: ModifierCollector => T): T = {
    val collector = new ModifierCollector()
    node.foreach(convert(_, collector))
    withCollector(collector, f)
  }

  def withModifiers[T](node: ValEmbedModifierContext, f: ModifierCollector => T): T = {
    val collector = new ModifierCollector()
    convert(node, collector)
    withCollector(collector, f)
  }

  def convert(contract: ValEmbedContractContext, collector: ContractCollector): Unit = contract match {
    case ValEmbedContract0(blocks) => blocks.foreach(convert(_, collector))
  }

  def convert(contract: ValEmbedContractBlockContext, collector: ContractCollector): Unit = contract match {
    case ValEmbedContractBlock0(_, clauses, _) => clauses.foreach(convert(_, collector))
    case ValEmbedContractBlock1(clauses) => clauses.foreach(convert(_, collector))
  }

  def convert(contract: ValContractClauseContext, collector: ContractCollector): Unit = contract match {
    case ValContractClause0(_, ids, _) => collector.modifies ++= convert(ids).map((contract, _))
    case ValContractClause1(_, ids, _) => collector.accessible ++= convert(ids).map((contract, _))
    case ValContractClause2(_, exp, _) => collector.requires += ((contract, convert(exp)))
    case ValContractClause3(_, exp, _) => collector.ensures += ((contract, convert(exp)))
    case ValContractClause4(_, t, id, _) =>
      val variable = new Variable(convert(t))(SourceNameOrigin(convert(id), origin(contract)))
      collector.given += ((contract, variable))
    case ValContractClause5(_, t, id, _) =>
      val variable = new Variable(convert(t))(SourceNameOrigin(convert(id), origin(contract)))
      collector.yields += ((contract, variable))
    case ValContractClause6(_, exp, _) => collector.context_everywhere += ((contract, convert(exp)))
    case ValContractClause7(_, exp, _) =>
      val specification = (contract, convert(exp))
      collector.requires += specification
      collector.ensures += specification
    case ValContractClause8(_, exp, _) => collector.loop_invariant += ((contract, convert(exp)))
    case ValContractClause9(_, exp, _) => collector.kernel_invariant += ((contract, convert(exp)))
    case ValContractClause10(_, _, t, id, _, exp, _) =>
      val variable = new Variable(convert(t))(SourceNameOrigin(convert(id), origin(contract)))
      collector.signals += ((contract, SignalsClause(variable, convert(exp))(originProvider(contract))))
  }

  def convert(mod: ValEmbedModifierContext, collector: ModifierCollector): Unit = mod match {
    case ValEmbedModifier0(_, mod, _) => convert(mod, collector)
    case ValEmbedModifier1(mod) => convert(mod, collector)
  }

  def convert(mod: ValModifierContext, collector: ModifierCollector): Unit = mod match {
    case ValModifier0(name) => name match {
      case "pure" => collector.pure += mod
      case "inline" => collector.inline += mod
      case "thread_local" => collector.threadLocal += mod
    }
    case ValStatic(_) => collector.static += mod
  }

  def convertEmbedWith(implicit whiff: Option[ValEmbedWithContext], inner: Expr): Expr = whiff match {
    case None => inner
    case Some(ValEmbedWith0(_, whiff, _)) => convertWith(whiff, inner)
    case Some(ValEmbedWith1(whiff)) => convertWith(Some(whiff), inner)
  }

  def convertWith(implicit whiff: Option[ValWithContext], inner: Expr): Expr = whiff match {
    case None => inner
    case Some(whiff @ ValWith0(_, stat)) => With(convert(stat), inner)(origin(whiff))
  }

  def convertEmbedThen(implicit den: Option[ValEmbedThenContext], inner: Expr): Expr = den match {
    case None => inner
    case Some(ValEmbedThen0(_, den, _)) => convertThen(den, inner)
    case Some(ValEmbedThen1(den)) => convertThen(Some(den), inner)
  }

  def convertThen(implicit den: Option[ValThenContext], inner: Expr): Expr = den match {
    case None => inner
    case Some(den @ ValThen0(_, stat)) => Then(inner, convert(stat))(origin(den))
  }

  def convert(implicit whiff: ValEmbedWithContext): Statement = whiff match {
    case ValEmbedWith0(_, Some(whiff), _) => convert(whiff)
    case ValEmbedWith0(_, None, _) => Block(Nil)
    case ValEmbedWith1(whiff) => convert(whiff)
  }

  def convert(implicit whiff: ValWithContext): Statement = whiff match {
    case ValWith0(_, stat) => convert(stat)
  }

  def convert(implicit whiff: ValEmbedThenContext): Statement = whiff match {
    case ValEmbedThen0(_, Some(whiff), _) => convert(whiff)
    case ValEmbedThen0(_, None, _) => Block(Nil)
    case ValEmbedThen1(whiff) => convert(whiff)
  }

  def convert(implicit whiff: ValThenContext): Statement = whiff match {
    case ValThen0(_, stat) => convert(stat)
  }

  def convertEmbedGiven(implicit given: Option[ValEmbedGivenContext]): Seq[(String, Expr)] = given match {
    case None => Nil
    case Some(ValEmbedGiven0(_, inner, _)) => convertGiven(inner)
    case Some(ValEmbedGiven1(inner)) => convertGiven(Some(inner))
  }

  def convertGiven(implicit given: Option[ValGivenContext]): Seq[(String, Expr)] = given match {
    case None => Nil
    case Some(ValGiven0(_, _, mappings, _)) => convert(mappings)
  }

  def convert(implicit mappings: ValGivenMappingsContext): Seq[(String, Expr)] = mappings match {
    case ValGivenMappings0(arg, _, v) => Seq((convert(arg), convert(v)))
    case ValGivenMappings1(arg, _, v, _, more) => (convert(arg), convert(v)) +: convert(more)
  }

  def convertEmbedYields(implicit given: Option[ValEmbedYieldsContext]): Seq[(Expr, String)] = given match {
    case None => Nil
    case Some(ValEmbedYields0(_, inner, _)) => convertYields(inner)
    case Some(ValEmbedYields1(inner)) => convertYields(Some(inner))
  }

  def convertYields(implicit given: Option[ValYieldsContext]): Seq[(Expr, String)] = given match {
    case None => Nil
    case Some(ValYields0(_, _, mappings, _)) => convert(mappings)
  }

  def convert(implicit mappings: ValYieldsMappingsContext): Seq[(Expr, String)] = mappings match {
    case ValYieldsMappings0(target, _, res) => Seq((convert(target), convert(res)))
    case ValYieldsMappings1(target, _, res, _, more) => (convert(target), convert(res)) +: convert(more)
  }

  def convert(implicit exprs: ValExpressionListContext): Seq[Expr] = exprs match {
    case ValExpressionList0(expr) => Seq(convert(expr))
    case ValExpressionList1(head, _, tail) => convert(head) +: convert(tail)
  }

  def convert(implicit ids: ValIdListContext): Seq[String] = ids match {
    case ValIdList0(id) => Seq(convert(id))
    case ValIdList1(id, _, ids) => convert(id) +: convert(ids)
  }

  def convert(implicit ts: ValTypeListContext): Seq[Type] = ts match {
    case ValTypeList0(t) => Seq(convert(t))
    case ValTypeList1(t, _, ts) => convert(t) +: convert(ts)
  }

  def convert(implicit impOp: ValImpOpContext, left: Expr, right: Expr): Expr = impOp match {
    case ValImpOp0(_) => Wand(left, right)(origin(impOp))
    case ValImpOp1(_) => Implies(left, right)(origin(impOp))
  }

  def convert(implicit andOp: ValAndOpContext, left: Expr, right: Expr): Expr = andOp match {
    case ValAndOp0(_) => col.Star(left, right)(origin(andOp))
  }

  def convert(implicit inOp: ValInOpContext, left: Expr, right: Expr): Expr = inOp match {
    case ValInOp0(_) => AmbiguousMember(left, right)
  }

  def convert(implicit mulOp: ValMulOpContext, left: Expr, right: Expr): Expr = mulOp match {
    case ValMulOp0(_) => col.Div(left, right)(blame(mulOp))
  }

  def convert(implicit prependOp: ValPrependOpContext, left: Expr, right: Expr): Expr = prependOp match {
    case ValPrependOp0(_) => Cons(left, right)
  }

  def convert(implicit postfixOp: ValPostfixContext, xs: Expr): Expr = postfixOp match {
    case ValPostfix0(_, _, to, _) => Take(xs, convert(to))
    case ValPostfix1(_, from, _, None, _) => Drop(xs, convert(from))
    case ValPostfix1(_, from, _, Some(to), _) => Slice(xs, convert(from), convert(to))
    case ValPostfix2(_, idx, _, v, _) => SeqUpdate(xs, convert(idx), convert(v))
  }

  def convert(implicit block: ValEmbedStatementBlockContext): Block = block match {
    case ValEmbedStatementBlock0(_, stats, _) => Block(stats.map(convert(_)))
    case ValEmbedStatementBlock1(stats) => Block(stats.map(convert(_)))
  }

  def convert(implicit stat: ValStatementContext): Statement = stat match {
    case ValCreateWand(_, block) => WandCreate(convert(block))
    case ValQedWand(_, wand, _) => WandQed(convert(wand))
    case ValApplyWand(_, wand, _) => WandApply(convert(wand))
    case ValUseWand(_, wand, _) => WandUse(convert(wand))
    case ValFold(_, predicate, _) =>
      Fold(convert(predicate))
    case ValUnfold(_, predicate, _) =>
      Unfold(convert(predicate))
    case ValOpen(_, _, _) => ??(stat)
    case ValClose(_, _, _) => ??(stat)
    case ValAssert(_, assn, _) => Assert(convert(assn))(blame(stat))
    case ValAssume(_, assn, _) => Assume(convert(assn))
    case ValInhale(_, resource, _) => Inhale(convert(resource))
    case ValExhale(_, resource, _) => Exhale(convert(resource))(blame(stat))
    case ValLabel(_, label, _) =>
      Label(new LabelDecl()(SourceNameOrigin(convert(label), origin(stat))))
    case ValRefute(_, assn, _) => Refute(convert(assn))
    case ValWitness(_, _, _) => ??(stat)
    case ValGhost(_, stat) => convert(stat)
    case ValSend(_, resource, _, label, _, offset, _) =>
      Send(convert(resource), new UnresolvedRef[LabelDecl](convert(label)), convert(offset))
    case ValRecv(_, resource, _, label, _, offset, _) =>
      Recv(convert(resource), new UnresolvedRef[LabelDecl](convert(label)), convert(offset))
    case ValTransfer(_, _, _) => ??(stat)
    case ValCslSubject(_, _, _) => ??(stat) // FIXME PB: csl_subject seems to be used
    case ValSpecIgnoreStart(_, _) => SpecIgnoreEnd()
    case ValSpecIgnoreEnd(_, _) => SpecIgnoreStart()
    case ValActionModel(_, _, model, _, perm, _, after, _, action, _, impl) =>
      ModelDo(convert(model), convert(perm), convert(after), convert(action), impl match {
        case ValActionImpl0(_) => Block(Nil)
        case ValActionImpl1(inner) => convert(inner)
      })
    case ValAtomic(_, _, invariant, _, body) =>
      ParAtomic(Seq(new UnresolvedRef[ParInvariantDecl](convert(invariant))), convert(body))
  }

  def convert(implicit block: ValBlockContext): Seq[Statement] = block match {
    case ValBlock0(_, stats, _) => stats.map(convert(_))
  }

  def convert(implicit arg: ValArgContext): Variable = arg match {
    case ValArg0(t, id) => new Variable(convert(t))(SourceNameOrigin(convert(id), origin(arg)))
  }

  def convert(implicit args: ValArgListContext): Seq[Variable] = args match {
    case ValArgList0(arg) => Seq(convert(arg))
    case ValArgList1(arg, _, args) => convert(arg) +: convert(args)
  }

  def convert(implicit decl: ValEmbedGlobalDeclarationBlockContext): Seq[GlobalDeclaration] = decl match {
    case ValEmbedGlobalDeclarationBlock0(_, globals, _) => globals.flatMap(convert(_))
    case ValEmbedGlobalDeclarationBlock1(globals) => globals.flatMap(convert(_))
  }

  def convert(implicit decl: ValGlobalDeclarationContext): Seq[GlobalDeclaration] = decl match {
    case ValAxiom(_, name, _, axiom, _) =>
      Seq(new SimplificationRule(convert(axiom))(SourceNameOrigin(convert(name), origin(decl))))
    case ValPredicate(modifiers, _, name, _, args, _, definition) =>
      withModifiers(modifiers, mods =>
        Seq(new Predicate(args.map(convert(_)).getOrElse(Nil), convert(definition),
          mods.consume(mods.threadLocal), mods.consume(mods.inline))
        (SourceNameOrigin(convert(name), origin(decl)))))
    case ValFunction(contract, modifiers, _, t, name, typeArgs, _, args, _, definition) =>
      Seq(withContract(contract, c =>
        withModifiers(modifiers, m => {
          val namedOrigin = SourceNameOrigin(convert(name), origin(decl))
          new Function(
            convert(t),
            args.map(convert(_)).getOrElse(Nil),
            typeArgs.map(convert(_)).getOrElse(Nil),
            convert(definition),
            c.consumeApplicableContract(),
            m.consume(m.inline))(blame(decl))(namedOrigin)
        })
      ))
    case ValModel(_, name, _, decls, _) =>
      Seq(new Model(decls.map(convert(_)))(SourceNameOrigin(convert(name), origin(decl))))
    case ValGhostDecl(_, inner) =>
      convert(inner)
    case ValAdtDecl(_, name, typeArgs, _, decls, _) =>
      Seq(new AxiomaticDataType(decls.map(convert(_)), typeArgs.map(convert(_)).getOrElse(Nil))(
        SourceNameOrigin(convert(name), origin(decl))))
  }

  def convert(implicit decl: ValEmbedClassDeclarationBlockContext): Seq[ClassDeclaration] = decl match {
    case ValEmbedClassDeclarationBlock0(_, decls, _) => decls.flatMap(convert(_, x => x))
    case ValEmbedClassDeclarationBlock1(decls) => decls.flatMap(convert(_, x => x))
  }

  def convert[T](implicit decl: ValClassDeclarationContext, transform: ClassDeclaration => T): Seq[T] = decl match {
    case ValInstancePredicate(modifiers, _, name, _, args, _, definition) =>
      Seq(withModifiers(modifiers, mods => {
        transform(new InstancePredicate(args.map(convert(_)).getOrElse(Nil), convert(definition),
          mods.consume(mods.threadLocal), mods.consume(mods.inline))(
          SourceNameOrigin(convert(name), origin(decl))))
      }))
    case ValInstanceFunction(contract, modifiers, _, t, name, typeArgs, _, args, _, definition) =>
      Seq(withContract(contract, c => {
        withModifiers(modifiers, m => {
          transform(new InstanceFunction(
            convert(t),
            args.map(convert(_)).getOrElse(Nil),
            typeArgs.map(convert(_)).getOrElse(Nil),
            convert(definition),
            c.consumeApplicableContract(), m.consume(m.inline))(
            blame(decl))(
            SourceNameOrigin(convert(name), origin(decl))))
        })
      }))
    case ValInstanceGhostDecl(_, decl) => convert(decl).map(transform)
  }

  def convert(implicit decl: ValModelDeclarationContext): ModelDeclaration = decl match {
    case ValModelField(t, name, _) =>
      new ModelField(convert(t))(SourceNameOrigin(convert(name), origin(decl)))
    case ValModelProcess(contract, _, name, _, args, _, _, definition, _) =>
      withContract(contract, c => {
        new ModelProcess(args.map(convert(_)).getOrElse(Nil), convert(definition),
          col.And.fold(c.consume(c.requires)), col.And.fold(c.consume(c.ensures)),
          c.consume(c.modifies).map(new UnresolvedRef[ModelField](_)), c.consume(c.accessible).map(new UnresolvedRef[ModelField](_)))(
          SourceNameOrigin(convert(name), origin(decl)))
      })
    case ValModelAction(contract, _, name, _, args, _, _) =>
      withContract(contract, c => {
        new ModelAction(args.map(convert(_)).getOrElse(Nil),
          col.And.fold(c.consume(c.requires)), col.And.fold(c.consume(c.ensures)),
          c.consume(c.modifies).map(new UnresolvedRef[ModelField](_)), c.consume(c.accessible).map(new UnresolvedRef[ModelField](_)))(
          SourceNameOrigin(convert(name), origin(decl)))
      })
  }

  def convert(implicit ts: ValTypeVarsContext): Seq[Variable] = ts match {
    case ValTypeVars0(_, names, _) =>
      convert(names).map(name => new Variable(TType(TAny()))(SourceNameOrigin(name, origin(ts))))
  }

  def convert(implicit decl: ValAdtDeclarationContext): ADTDeclaration = decl match {
    case ValAdtAxiom(_, ax, _) => new ADTAxiom(convert(ax))
    case ValAdtFunction(_, returnType, name, _, args, _, _) =>
      new ADTFunction(args.map(convert(_)).getOrElse(Nil), convert(returnType))(
        SourceNameOrigin(convert(name), origin(decl)))
  }

  def convert(implicit definition: ValDefContext): Option[Expr] = definition match {
    case ValAbstractBody(_) => None
    case ValBody(_, expr, _) => Some(convert(expr))
  }

  def convert(implicit t: ValTypeContext): Type = t match {
    case ValPrimaryType(name) => name match {
      case "resource" => TResource()
      case "process" => TProcess()
      case "frac" => TFraction()
      case "zfrac" => TZFraction()
      case "rational" => TRational()
      case "bool" => TBool()
      case "ref" => TRef()
    }
    case ValSeqType(_, _, element, _) => TSeq(convert(element))
    case ValSetType(_, _, element, _) => TSet(convert(element))
    case ValBagType(_, _, element, _) => TBag(convert(element))
    case ValOptionType(_, _, element, _) => TOption(convert(element))
    case ValMapType(_, _, key, _, value, _) => TMap(convert(key), convert(value))
    case ValTupleType(_, _, t1, _, t2, _) => TTuple(Seq(convert(t1), convert(t2)))
    case ValPointerType(_, _, element, _) => TPointer(convert(element))
  }

  def convert(implicit e: ValPrimarySeqContext): Expr = e match {
    case ValCardinality(_, xs, _) => Size(convert(xs))
    case ValArrayValues(_, _, a, _, from, _, to, _) => Values(convert(a), convert(from), convert(to))
  }

  def convert(implicit e: ValPrimaryOptionContext): Expr = e match {
    case ValSome(_, _, v, _) => OptSome(convert(v))
  }

  // valsetcompselectors
  // valMapPairs

  def convert(implicit e: ValPrimaryCollectionConstructorContext): Expr = e match {
    case ValTypedLiteralSeq(_, _, t, _, _, exprs, _) => LiteralSeq(convert(t), exprs.map(convert(_)).getOrElse(Nil))
    case ValTypedLiteralSet(_, _, t, _, _, exprs, _) => LiteralSet(convert(t), exprs.map(convert(_)).getOrElse(Nil))
    case ValSetComprehension(_, _, t, _, _, value, _, selectors, _, something, _) => ??(e)
    case ValTypedLiteralBag(_, _, t, _, _, exprs, _) => LiteralBag(convert(t), exprs.map(convert(_)).getOrElse(Nil))
    case ValTypedLiteralMap(_, _, key, _, value, _, _, pairs, _) => ??(e)
    case ValTypedTuple(_, _, t1, _, t2, _, _, v1, _, v2, _) => ??(e)
    case ValLiteralSeq(_, exprs, _) => UntypedLiteralSeq(convert(exprs))
    case ValLiteralSet(_, exprs, _) => UntypedLiteralSet(convert(exprs))
    case ValLiteralBag(_, exprs, _) => UntypedLiteralBag(convert(exprs))
    case ValEmptySeq(_, t, _) => LiteralSeq(convert(t), Nil)
    case ValEmptySet(_, t, _) => LiteralSet(convert(t), Nil)
    case ValEmptyBag(_, t, _) => LiteralBag(convert(t), Nil)
    case ValRange(_, from, _, to, _) => Range(convert(from), convert(to))
  }

  def convert(implicit e: ValPrimaryPermissionContext): Expr = e match {
    case ValCurPerm(_, _, loc, _) => CurPerm(convert(loc))
    case ValPerm(_, _, loc, _, perm, _) => Perm(convert(loc), convert(perm))
    case ValValue(_, _, loc, _) => Perm(convert(loc), ReadPerm())
    case ValPointsTo(_, _, loc, _, perm, _, v, _) => PointsTo(convert(loc), convert(perm), convert(v))
    case ValHPerm(_, _, loc, _, perm, _) => HPerm(convert(loc), convert(perm))
    case ValAPerm(_, _, loc, _, perm, _) => APerm(convert(loc), convert(perm))
    case ValArrayPerm(_, _, arr, _, i, _, step, _, count, _, perm, _) => ??(e)
    case ValMatrix(_, _, m, _, dim1, _, dim2, _) => ValidMatrix(convert(m), convert(dim1), convert(dim2))
    case ValArray(_, _, arr, _, dim, _) => ValidArray(convert(arr), convert(dim))
    case ValPointer(_, _, ptr, _, n, _, perm, _) => PermPointer(convert(ptr), convert(n), convert(perm))
    case ValPointerIndex(_, _, ptr, _, idx, _, perm, _) => PermPointerIndex(convert(ptr), convert(idx), convert(perm))
  }

  def convert(implicit e: ValPrimaryBinderContext): Expr = e match {
    case ValRangeQuantifier(_, quant, t, id, _, from, _, to, _, body, _) =>
      val variable = new Variable(convert(t))(SourceNameOrigin(convert(id), origin(id)))
      val cond = SeqMember(Local(variable.ref), Range(convert(from), convert(to)))
      quant match {
        case "\\forall*" => Starall(Seq(variable), Nil, Implies(cond, convert(body)))
        case "\\forall" => Forall(Seq(variable), Nil, Implies(cond, convert(body)))
        case "\\exists" => Exists(Seq(variable), Nil, col.And(cond, convert(body)))
      }
    case ValQuantifier(_, quant, t, id, _, cond, _, body, _) =>
      val variable = new Variable(convert(t))(SourceNameOrigin(convert(id), origin(id)))
      quant match {
        case "\\forall*" => Starall(Seq(variable), Nil, Implies(convert(cond), convert(body)))
        case "\\forall" => Forall(Seq(variable), Nil, Implies(convert(cond), convert(body)))
        case "\\exists" => Exists(Seq(variable), Nil, col.And(convert(cond), convert(body)))
      }
    case ValLet(_, _, t, id, _, v, _, body, _) =>
      Let(new Variable(convert(t))(SourceNameOrigin(convert(id), origin(id))), convert(v), convert(body))
  }

  def convert(implicit e: ValPrimaryVectorContext): Expr = e match {
    case ValSum(_, _, t, id, _, cond, _, body, _) =>
      val binding = new Variable(convert(t))(SourceNameOrigin(convert(id), origin(id)))
      Sum(Seq(binding), convert(cond), convert(body))
    case ValVectorSum(_, _, rng, _, vec, _) => VectorSum(convert(rng), convert(vec))
    case ValVectorCmp(_, _, left, _, right, _) => VectorCompare(convert(left), convert(right))
    case ValVectorRep(_, _, inner, _) => VectorRepeat(convert(inner))
    case ValMatrixSum(_, _, rng, _, mat, _) => MatrixSum(convert(rng), convert(mat))
    case ValMatrixCmp(_, _, left, _, right, _) => MatrixCompare(convert(left), convert(right))
    case ValMatrixRep(_, _, inner, _) => MatrixRepeat(convert(inner))
  }

  def convert(implicit e: ValPrimaryReducibleContext): Expr = ??(e)

  def convert(implicit e: ValPrimaryThreadContext): Expr = e match {
    case ValIdle(_, _, thread, _) => IdleToken(convert(thread))
    case ValRunning(_, _, thread, _) => JoinToken(convert(thread))
  }

  def convert(implicit e: ValPrimaryContext): Expr = e match {
    case ValPrimary0(inner) => convert(inner)
    case ValPrimary1(inner) => convert(inner)
    case ValPrimary2(inner) => convert(inner)
    case ValPrimary3(inner) => convert(inner)
    case ValPrimary4(inner) => convert(inner)
    case ValPrimary5(inner) => convert(inner)
    case ValPrimary6(inner) => convert(inner)
    case ValPrimary7(inner) => convert(inner)
    case ValAny(_) => Any()
    case ValIndependent(_, e, _, name, _) => ??(e)
    case ValScale(_, perm, _, predInvocation) => Scale(convert(perm), convert(predInvocation))
    case ValInlinePattern(_, pattern, _) => InlinePattern(convert(pattern))
    case ValUnfolding(_, predExpr, _, body) => Unfolding(convert(predExpr), convert(body))
    case ValOld(_, _, expr, _) => Old(convert(expr), at = None)(blame(e))
    case ValTypeof(_, _, expr, _) => TypeOf(convert(expr))
    case ValHeld(_, _, obj, _) => Held(convert(obj))
  }

  def convert(implicit res: ValReservedContext): Expr = res match {
    case ValReserved0(name) => fail(res,
      f"This identifier is reserved, and cannot be declared or used in specifications. " +
        f"You might want to escape the identifier with backticks: `$name`")
    case ValIdEscape(id) => local(res, id.substring(1, id.length-1))
    case ValResult(_) => AmbiguousResult()
    case ValCurrentThread(_) => CurrentThreadId()
    case ValNonePerm(_) => NoPerm()
    case ValWrite(_) => WritePerm()
    case ValRead(_) => ReadPerm()
    case ValNoneOption(_) => OptNone()
    case ValEmpty(_) => EmptyProcess()
    case ValLtid(_) => ???
    case ValGtid(_) => ???
    case ValTrue(_) => true
    case ValFalse(_) => false
  }

  def convert(implicit inv: ValGenericAdtInvocationContext): Expr = inv match {
    case ValGenericAdtInvocation0(adt, _, typeArgs, _, _, func, _, args, _) =>
      ADTFunctionInvocation(Some((new UnresolvedRef[AxiomaticDataType](convert(adt)), convert(typeArgs))),
        new UnresolvedRef[ADTFunction](convert(func)), args.map(convert(_)).getOrElse(Nil))
  }
}
