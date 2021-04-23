package vct.parsers.transform

import org.antlr.v4.runtime.ParserRuleContext
import vct.antlr4.generated.JavaParser._
import vct.antlr4.generated.JavaParserPatterns._
import vct.col.ast.Constant._
import vct.col.ast._
import vct.col.{ast => col}

case class JavaToCol(override val originProvider: OriginProvider, blameProvider: BlameProvider) extends ToCol(originProvider) {
  implicit def origin(implicit node: ParserRuleContext): Origin = originProvider(node)

  def convert(implicit unit: CompilationUnitContext): Seq[GlobalDeclaration] = unit match {
    case CompilationUnit0(pkg, imports, decls, _) =>
      Seq(JavaNamespace(pkg.map(convert(_)), imports.map(convert(_)), decls.flatMap(convert(_))))
  }

  def convert(implicit pkg: PackageDeclarationContext): JavaName = pkg match {
    case PackageDeclaration0(_, _, name, _) => convert(name)
  }

  def convert(implicit imp: ImportDeclarationContext): JavaImport = imp match {
    case ImportDeclaration0(_, isStatic, name, star, _) => JavaImport(isStatic.nonEmpty, convert(name), star.nonEmpty)
  }

  def convert(implicit decl: TypeDeclarationContext): Seq[GlobalDeclaration] = decl match {
    case TypeDeclaration0(mods, ClassDeclaration0(_, name, args, ext, imp, ClassBody0(_, decls, _))) =>
      Seq(JavaClass(convert(name), mods.map(convert(_)), args.map(convert(_)).getOrElse(Nil),
        ext.map(convert(_)).getOrElse(TClass.OBJECT),
        imp.map(convert(_)).getOrElse(Nil), decls.flatMap(convert(_))))
    case TypeDeclaration1(mods, enum) => fail(enum, "Enums are not supported.")
    case TypeDeclaration2(mods, InterfaceDeclaration0(_, name, args, ext, InterfaceBody0(_, decls, _))) =>
      Seq(JavaInterface(convert(name), mods.map(convert(_)), args.map(convert(_)).getOrElse(Nil),
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
        new Variable(TType(TClass.OBJECT))(SourceNameOrigin(convert(id), origin(arg)))
      case Some(TypeParameterBound0(_, TypeBound0(ext))) =>
        new Variable(TType(convert(ext)))(SourceNameOrigin(convert(id), origin(arg)))
      case Some(TypeParameterBound0(_, TypeBound1(_, _, moreBounds))) =>
        fail(moreBounds, "Multiple upper type bounds are not supported")
    }
  }

  def convert(implicit decl: ClassBodyDeclarationContext): Seq[ClassDeclaration] = decl match {
    case ClassBodyDeclaration0(_) => Nil
    case ClassBodyDeclaration1(isStatic, body) => Seq(JavaSharedInitialization(isStatic.nonEmpty, convert(body)))
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
      Seq(JavaMethod(mods, convert(returnType), dims.map(convert(_)).getOrElse(0),
        convert(name), convert(params), Nil, signals.map(convert(_)).getOrElse(Nil),
        convert(body), c.consumeApplicableContract()))
    case MemberDeclaration1(GenericMethodDeclaration0(typeParams, MethodDeclaration0(
      returnType, name, params, dims, signals, body))) =>
      Seq(JavaMethod(mods, convert(returnType), dims.map(convert(_)).getOrElse(0),
        convert(name), convert(params), convert(typeParams), signals.map(convert(_)).getOrElse(Nil),
        convert(body), c.consumeApplicableContract()))
    case MemberDeclaration2(FieldDeclaration0(t, decls, _)) =>
      // Ignore the contract collector, so that complains about being non-empty
      Seq(JavaFields(mods, convert(t), convert(decls)))
    case MemberDeclaration3(ConstructorDeclaration0(name, params, signals, ConstructorBody0(body))) =>
      Seq(JavaConstructor(mods, convert(name), convert(params), Nil,
        signals.map(convert(_)).getOrElse(Nil), convert(body), c.consumeApplicableContract()))
    case MemberDeclaration4(GenericConstructorDeclaration0(typeParams,
      ConstructorDeclaration0(name, params, signals, ConstructorBody0(body)))) =>
      Seq(JavaConstructor(mods, convert(name), convert(params), convert(typeParams),
        signals.map(convert(_)).getOrElse(Nil), convert(body), c.consumeApplicableContract()))
    case MemberDeclaration5(interface) => fail(interface, "Inner interfaces are not supported.")
    case MemberDeclaration6(annotation) => fail(annotation, "Annotations are not supported.")
    case MemberDeclaration7(cls) => fail(cls, "Inner classes are not supported.")
    case MemberDeclaration8(enum) => fail(enum, "Enums are not supported.")
  }

  def convert(implicit decl: InterfaceMemberDeclarationContext, mods: Seq[JavaModifier], c: ContractCollector): Seq[ClassDeclaration] = decl match {
    case InterfaceMemberDeclaration0(ConstDeclaration0(t, decls, _)) =>
      // JLS SE 7 - 9.3
      Seq(JavaFields(Seq(JavaPublic(), JavaStatic(), JavaFinal()) ++ mods, convert(t), convert(decls)))
    case InterfaceMemberDeclaration1(InterfaceMethodDeclaration0(t, name, params, dims, signals, _)) =>
      // JLS SE 7 - 9.4
      Seq(JavaMethod(Seq(JavaPublic(), JavaAbstract()) ++ mods, convert(t), dims.map(convert(_)).getOrElse(0),
        convert(name), convert(params), Nil, signals.map(convert(_)).getOrElse(Nil), None, c.consumeApplicableContract()))
    case InterfaceMemberDeclaration2(GenericInterfaceMethodDeclaration0(typeParams, InterfaceMethodDeclaration0(
      t, name, params, dims, signals, _))) =>
      Seq(JavaMethod(Seq(JavaPublic(), JavaAbstract()) ++ mods, convert(t), dims.map(convert(_)).getOrElse(0),
        convert(name), convert(params), convert(typeParams), signals.map(convert(_)).getOrElse(Nil),
        None, c.consumeApplicableContract()))
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
    case FormalParameter0(_, t, name) =>
      new Variable(convert(t))(SourceNameOrigin(convert(name), origin(param)))
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
    case Block0(_, stats, _) => Block(stats.map(convert(_)))
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
      JavaLocalDeclaration(mods.map(convert(_)), convert(t), convert(decls))
  }

  def convert(implicit stat: ElseBlockContext): Statement = stat match {
    case ElseBlock0(_, stat) => convert(stat)
  }

  def convert(implicit stat: StatementContext): Statement = stat match {
    case Statement0(block) => convert(block)
    case Statement1(_, assn, _, _) => Assert(convert(assn))(blameProvider(stat))
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
            Loop(
              init.map(convert(_)).getOrElse(Block(Nil)),
              cond.map(convert(_)).getOrElse(true),
              update.map(convert(_)).getOrElse(Block(Nil)),
              Star.fold(c.consume(c.loop_invariant)),
              convert(body)
            )
        }
      })

      label match {
        case None => loop
        case Some(label) => Block(Seq(convert(label), loop))
      }
    case Statement4(contract1, label, _, cond, contract2, body) =>
      val loop = withContract(contract1, contract2, c => {
        Loop(Block(Nil), convert(cond), Block(Nil), Star.fold(c.consume(c.loop_invariant)), convert(body))
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
    case Statement13(_, label, _) => Break(label.map(convert(_)).map(new UnresolvedRef(_)))
    case Statement14(_, label, _) => Continue(label.map(convert(_)).map(new UnresolvedRef(_)))
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

  def convert(implicit ts: CatchTypeContext): JavaTUnion = ts match {
    case CatchType0(name) => JavaTUnion(Seq(convert(name)))
    case CatchType1(name, _, names) => JavaTUnion(convert(name) +: convert(names).names)
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
    case Type1(element, dims) => JavaTArray(convert(element), dims.map(convert(_)).getOrElse(0))
    case Type2(element, dims) => JavaTArray(convert(element), dims.map(convert(_)).getOrElse(0))
  }

  def convert(implicit t: ClassOrInterfaceTypeContext): JavaTClass = t match {
    case ClassOrInterfaceType0(name, args) =>
      JavaTClass(Seq((convert(name), args.map(convert(_)))))
    case ClassOrInterfaceType1(names, _, name, args) =>
      JavaTClass(convert(names).names :+ (convert(name), args.map(convert(_))))
  }

  def convert(implicit t: CreatedNameContext): Type = t match {
    case CreatedName0(t) => convert(t)
    case CreatedName1(t) => convert(t)
  }

  def convert(implicit t: ClassTypeDiamondListContext): JavaTClass = t match {
    case ClassTypeDiamondList0(name, typeArgs) =>
      JavaTClass(Seq((convert(name), typeArgs.map(convert(_)))))
    case ClassTypeDiamondList1(name, typeArgs, _, more) =>
      JavaTClass((convert(name), typeArgs.map(convert(_))) +: convert(more).names)
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
      case "float" => ??(t)
      case "double" => ??(t)
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
    case Expression0(inner) => convert(inner)
    case Expression1(inner) => convert(inner)
    case Expression2(obj, _, field) => Deref(convert(obj), new UnresolvedRef(convert(field)))
    case Expression3(innerOrOuterClass, _, _) => ??(expr)
    case Expression4(pinnedOuterClassObj, _, _, _, _) => ??(expr)
    case Expression5(_, _, _, _) => ??(expr)
    case Expression6(obj, _, ExplicitGenericInvocation0(typeArgs, invocation)) =>
      convert(invocation, Some(convert(obj)), convert(typeArgs))
    case Expression7(ar, _, idx, _) => AmbiguousSubscript(convert(ar), convert(idx))
    case Expression8(obj, _, name, args) =>
      Implies(
        Neq(convert(obj), Null()),
        JavaInvocation(Some(convert(obj)), Nil, convert(name), convert(args))(blameProvider(expr)),
      )
    case Expression9(obj, _, name, familyType, args, withThen) =>
      failIfDefined(familyType, "Predicate families not supported (for now)")
      val (given, yields) = withThen.map(convert(_)).getOrElse((Nil, Nil))
      With(Block(given), Then(
        JavaInvocation(Some(convert(obj)), Nil, convert(name), convert(args))(blameProvider(expr)),
        Block(yields)))
    case Expression10(_, creator) => convert(creator)
    case Expression11(_, t, _, inner) => Cast(convert(inner), TypeValue(convert(t)))
    case Expression12(inner, postOp) =>
      val target = convert(inner)
      postOp match {
        case "++" => PostAssignExpression(target, Plus(target, 1))
        case "--" => PostAssignExpression(target, Minus(target, 1))
      }
    case Expression13(preOp, inner) =>
      val target = convert(inner)
      preOp match {
        case "+" => target // TODO PB: not sure if this is true for IEEE floats
        case "-" => UMinus(target)
        case "++" => PreAssignExpression(target, Plus(target, 1))
        case "--" => PreAssignExpression(target, Minus(target, 1))
      }
    case Expression14(preOp, inner) => preOp match {
      case "~" => BitNot(convert(inner))
      case "!" => Not(convert(inner))
    }
    case Expression15(leftNode, mul, rightNode) =>
      val (left, right) = (convert(leftNode), convert(rightNode))
      mul match {
        case MulOp0(op) => op match {
          case "*" => Mult(left, right)
          case "/" => FloorDiv(left, right)(blameProvider(expr))
          case "%" => Mod(left, right)(blameProvider(expr))
        }
        case MulOp1(specOp) => convert(specOp, left, right)
      }
    case Expression16(left, op, right) => op match {
      case "+" => Plus(convert(left), convert(right))
      case "-" => Minus(convert(left), convert(right))
    }
    case Expression17(left, shift, right) => shift match {
      case ShiftOp0(_, _) => BitShl(convert(left), convert(right))
      case ShiftOp1(_, _, _) => BitUShr(convert(left), convert(right))
      case ShiftOp2(_, _) => BitShr(convert(left), convert(right))
    }
    case Expression18(left, comp, right) => comp match {
      case "<=" => LessEq(convert(left), convert(right))
      case ">=" => GreaterEq(convert(left), convert(right))
      case ">" => Greater(convert(left), convert(right))
      case "<" => Less(convert(left), convert(right))
    }
    case Expression19(obj, _, t) => InstanceOf(convert(obj), TypeValue(convert(t)))
    case Expression20(left, eq, right) => eq match {
      case "==" => Eq(convert(left), convert(right))
      case "!=" => Neq(convert(left), convert(right))
    }
    case Expression21(left, _, right) => BitAnd(convert(left), convert(right)) // TODO PB: should be ambiguous
    case Expression22(left, _, right) => BitXor(convert(left), convert(right))
    case Expression23(left, _, right) => BitOr(convert(left), convert(right))
    case Expression24(left, and, right) => and match {
      case AndOp0(_) => And(convert(left), convert(right))
      case AndOp1(specOp) => convert(specOp, convert(left), convert(right))
    }
    case Expression25(left, _, right) => Or(convert(left), convert(right))
    case Expression26(left, imp, right) => imp match {
      case ImpOp0(specOp) => convert(specOp, convert(left), convert(right))
    }
    case Expression27(cond, _, whenTrue, _, whenFalse) =>
      Select(convert(cond), convert(whenTrue), convert(whenFalse))
    case _: Expression28Context => ??? // Extractor should be in the new ANTLR fork update
//    case Expression28(left, op, right) =>
//      val target = convert(left)
//      val value = convert(right)
//      PreAssignExpression(target, op match {
//        case "=" => value
//        case "+=" => Plus(target, value)
//        case "-=" => Minus(target, value)
//        case "*=" => Mult(target,  value)
//        case "/=" => FloorDiv(target,  value)(blameProvider(expr))
//        case "&=" => BitAnd(target, value)
//        case "|=" => BitOr(target, value)
//        case "^=" => BitXor(target, value)
//        case ">>=" => BitShr(target, value)
//        case ">>>=" => BitUShr(target, value)
//        case "<<=" => BitShl(target, value)
//        case "%=" => Mod(target, value)(blameProvider(expr))
//      })
  }

  def convert(implicit invocation: ExplicitGenericInvocationSuffixContext,
              obj: Option[Expr], typeArgs: Seq[Type]): Expr = invocation match {
    case ExplicitGenericInvocationSuffix0(_, _) => ??(invocation)
    case ExplicitGenericInvocationSuffix1(name, familyType, arguments) =>
      // FIXME PB: should support this again somehow, maybe reuse open/close with type syntax instead of fold/unfold?
      failIfDefined(familyType, "Predicate families not supported (for now)")
      JavaInvocation(obj, typeArgs, convert(name), convert(arguments))(blameProvider(invocation))
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
    case ClassCreatorRest0(args, withThen, impl) =>
      failIfDefined(impl, "Anonymous classes are not supported")
      val (before, after) = withThen.map(convert(_)).getOrElse((Nil, Nil))
      val result = JavaNewClass(convert(args), ts, name)(blameProvider(creator))
      With(Block(before), Then(result, Block(after)))
  }

  def convert(implicit creator: ArrayCreatorRestContext, name: Type): Expr = creator match {
    case ArrayCreatorRest0(dims, init) => JavaNewLiteralArray(name, convert(dims), convert(init))
    case ArrayCreatorRest1(specDims, extraDims) => JavaNewDefaultArray(name, convert(specDims), extraDims.map(convert(_)).getOrElse(0))
  }

  def convert(implicit expr: PrimaryContext): Expr = expr match {
    case Primary0(_, expr, _) => convert(expr)
    case Primary1(_) => AmbiguousThis()
    case Primary2(_) => ??(expr)
    case Primary3(literal) => convert(literal)
    case Primary4(name) => Local(new UnresolvedRef(convert(name)))
    case Primary5(name, familyType, args, withThen) =>
      failIfDefined(familyType, "Predicate families are unsupported (for now)")
      val (before, after) = withThen.map(convert(_)).getOrElse((Nil, Nil))
      val result = JavaInvocation(None, Nil, convert(name), convert(args))(blameProvider(expr))
      With(Block(before), Then(result, Block(after)))
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

  def convert(implicit id: VariableDeclaratorIdContext): String = id match {
    case VariableDeclaratorId0(_, Some(dims)) => ??(dims)
    case VariableDeclaratorId0(name, None) => convert(name)
  }

  def convert(implicit id: JavaIdentifierContext): String = id match {
    case JavaIdentifier0(specInSpec) => specInSpec match {
      case ValReserved1(id) => id.substring(1, id.length-1)
      case other => fail(other,
        f"This identifier is reserved, and cannot be declared or used in specifications. " +
          f"You might want to escape the identifier with backticks: `${other.getText}`")
    }
    case JavaIdentifier1(id) => id
    case JavaIdentifier2(specOutOfSpec) =>
      val text = specOutOfSpec.getText
      if(text.matches("[a-zA-Z]+")) text
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
      collector.signals += ((contract, (variable, convert(exp))))
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
    case ValModifier1(_) => collector.static += mod
  }

  private def reduceWithThen(xs: Seq[(Seq[Statement], Seq[Statement])]): (Seq[Statement], Seq[Statement]) =
    (xs.flatMap(_._1), xs.flatMap(_._2))

  def convert(implicit withThen: ValEmbedWithThenContext): (Seq[Statement], Seq[Statement]) = withThen match {
    case ValEmbedWithThen0(blocks) =>
      reduceWithThen(blocks.map(convert(_)))
  }

  def convert(implicit withThen: ValEmbedWithThenBlockContext): (Seq[Statement], Seq[Statement]) = withThen match {
    case ValEmbedWithThenBlock0(_, specs, _) =>
      reduceWithThen(specs.map(convert(_)))
    case ValEmbedWithThenBlock1(specs) =>
      reduceWithThen(specs.map(convert(_)))
  }

  def convert(implicit withThen: ValWithThenContext): (Seq[Statement], Seq[Statement]) = withThen match {
    case ValWithThen0(_, stat) => (convert(stat) +: Nil, Nil)
    case ValWithThen1(_, stat) => (Nil, convert(stat) +: Nil)
  }

  def convert(implicit exprs: ValExpressionListContext): Seq[Expr] = exprs match {
    case ValExpressionList0(expr) => Seq(convert(expr))
    case ValExpressionList1(head, _, tail) => convert(head) +: convert(tail)
  }

  def convert(implicit ids: ValIdListContext): Seq[String] = ids match {
    case ValIdList0(id) => Seq(convert(id))
    case ValIdList1(id, _, ids) => convert(id) +: convert(ids)
  }

  def convert(implicit impOp: ValImpOpContext, left: Expr, right: Expr): Expr = impOp match {
    case ValImpOp0(_) => Wand(left, right)(origin(impOp))
    case ValImpOp1(_) => Implies(left, right)(origin(impOp))
  }

  def convert(implicit andOp: ValAndOpContext, left: Expr, right: Expr): Expr = andOp match {
    case ValAndOp0(_) => col.Star(left, right)(origin(andOp))
  }

  def convert(implicit mulOp: ValMulOpContext, left: Expr, right: Expr): Expr = mulOp match {
    case ValMulOp0("\\\\") => Div(left, right)(blameProvider(mulOp))
  }

  def convert(implicit block: ValEmbedStatementBlockContext): Block = block match {
    case ValEmbedStatementBlock0(_, stats, _) => Block(stats.map(convert(_)))
    case ValEmbedStatementBlock1(stats) => Block(stats.map(convert(_)))
  }

  def convert(implicit stat: ValStatementContext): Statement = stat match {
    case ValStatement0(_, block) => WandCreate(convert(block))
    case ValStatement1(_, wand, _) => WandQed(convert(wand))
    case ValStatement2(_, wand, _) => WandApply(convert(wand))
    case ValStatement3(_, wand, _) => WandUse(convert(wand))
    case ValStatement4(_, _, _) => ??(stat)
    case ValStatement5(_, target, _, process, _) => ModelCreate(convert(target), ???, convert(process))
    case ValStatement6(_, _, _, _, _) => ??(stat)
    case ValStatement7(_, model, _) => ModelDestroy(convert(model))
    case ValStatement8(_, model, _, leftPerm, _, leftProcess, _, rightPerm, _, rightProcess, _) =>
      ModelSplitInto(convert(model), convert(leftPerm), convert(leftProcess), convert(rightPerm), convert(rightProcess))
    case ValStatement9(_, model, _, leftPerm, _, leftProcess, _, rightPerm, _, rightProcess, _) =>
      ModelMergeFrom(convert(model), convert(leftPerm), convert(leftProcess), convert(rightPerm), convert(rightProcess))
    case ValStatement10(_, model, _, perm, _, chooseProcess, _, choice, _) =>
      ModelChoose(convert(model), convert(perm), convert(chooseProcess), convert(choice))
    case ValStatement11(_, predicate, _) =>
      Fold(convert(predicate))
    case ValStatement12(_, predicate, _) =>
      Unfold(convert(predicate))
    case ValStatement13(_, _, _) => ??(stat)
    case ValStatement14(_, _, _) => ??(stat)
    case ValStatement15(_, assn, _) => Assert(convert(assn))(blameProvider(stat))
    case ValStatement16(_, assn, _) => Assume(convert(assn))
    case ValStatement17(_, resource, _) => Inhale(convert(resource))
    case ValStatement18(_, resource, _) => Exhale(convert(resource))(blameProvider(stat))
    case ValStatement19(_, label, _) =>
      Label(new LabelDecl()(SourceNameOrigin(convert(label), origin(stat))))
    case ValStatement20(_, assn, _) => ??(stat)
    case ValStatement21(_, _, _) => ??(stat)
    case ValStatement22(_, stat) => convert(stat)
    case ValStatement23(_, resource, _, label, _, idx, _) => ??(stat) // FIXME PB: send/recv should be supported
    case ValStatement24(_, resource, _, label, _, idx, _) => ??(stat)
    case ValStatement25(_, _, _) => ??(stat)
    case ValStatement26(_, _, _) => ??(stat) // FIXME PB: csl_subject seems to be used
    case ValStatement27(_, _) => SpecIgnoreEnd()
    case ValStatement28(_, _) => SpecIgnoreStart()
    case ValStatement29(_, model, _, perm, _, after, _, action, modelHeapMap, _) =>
      modelHeapMap match {
        case Nil =>
        case mapping :: _ =>
          ??(mapping) // FIXME PB: disabled for now, pending investigation moving this to model creation
      }
      ModelDo(convert(model), convert(perm), convert(after), convert(action))
    case ValStatement30(_, _, invariant, _, body) =>
      ParAtomic(new UnresolvedRef(convert(invariant)), convert(body))
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
    case ValGlobalDeclaration0(_, name, _, left, _, right, _) =>
      Seq(new SimplificationRule(convert(left), convert(right))(SourceNameOrigin(convert(name), origin(decl))))
    case ValGlobalDeclaration1(modifiers, _, name, _, args, _, definition) =>
      withModifiers(modifiers, mods =>
        Seq(new Predicate(args.map(convert(_)).getOrElse(Nil), convert(definition),
          mods.consume(mods.threadLocal), mods.consume(mods.inline))
        (SourceNameOrigin(convert(name), origin(decl)))))
    case ValGlobalDeclaration2(contract, modifiers, _, t, name, _, args, _, definition) =>
      Seq(withContract(contract, c =>
        withModifiers(modifiers, m => {
          val namedOrigin = SourceNameOrigin(convert(name), origin(decl))
          new Function(convert(t), args.map(convert(_)).getOrElse(Nil), convert(definition),
            c.consumeApplicableContract(), m.consume(m.inline))(blameProvider(decl))(namedOrigin)
        })
      ))
    case ValGlobalDeclaration3(_, name, _, decls, _) =>
      Seq(new Model(decls.map(convert(_)))(SourceNameOrigin(convert(name), origin(decl))))
    case ValGlobalDeclaration4(_, inner) =>
      convert(inner)
  }

  def convert(implicit decl: ValEmbedClassDeclarationBlockContext): Seq[ClassDeclaration] = ???

  def convert(implicit decl: ValModelDeclarationContext): ModelDeclaration = decl match {
    case ValModelDeclaration0(t, name, _) =>
      new ModelField(convert(t))(SourceNameOrigin(convert(name), origin(decl)))
    case ValModelDeclaration1(contract, _, name, _, args, _, _, definition, _) =>
      withContract(contract, c => {
        new ModelProcess(args.map(convert(_)).getOrElse(Nil), convert(definition),
          col.Star.fold(c.consume(c.requires)), col.Star.fold(c.consume(c.ensures)),
          c.consume(c.modifies).map(new UnresolvedRef(_)), c.consume(c.accessible).map(new UnresolvedRef(_)))(
          SourceNameOrigin(convert(name), origin(decl)))
      })
    case ValModelDeclaration2(contract, _, name, _, args, _, _) =>
      withContract(contract, c => {
        new ModelAction(args.map(convert(_)).getOrElse(Nil),
          col.Star.fold(c.consume(c.requires)), col.Star.fold(c.consume(c.ensures)),
          c.consume(c.modifies).map(new UnresolvedRef(_)), c.consume(c.accessible).map(new UnresolvedRef(_)))(
          SourceNameOrigin(convert(name), origin(decl)))
      })
  }

  def convert(implicit definition: ValDefContext): Option[Expr] = definition match {
    case ValDef0(_) => None
    case ValDef1(_, expr, _) => Some(convert(expr))
  }

  def convert(implicit t: ValTypeContext): Type = t match {
    case ValType0(name) => name match {
      case "resource" => TResource()
      case "process" => TProcess()
      case "frac" => TFraction()
      case "zfrac" => TZFraction()
      case "rational" => TRational()
      case "bool" => TBool()
    }
    case ValType1(_, _, element, _) => TSeq(convert(element))
    case ValType2(_, _, element, _) => TSet(convert(element))
    case ValType3(_, _, element, _) => TBag(convert(element))
    case ValType4(_, _, element, _) => TPointer(convert(element))
  }

  def convert(implicit e: ValPrimaryContext): Expr = e match {
    case ValPrimary0(_, _, _, _) => ??(e)
    case ValPrimary1(_, scale, _, resource) => Scale(convert(scale), convert(resource))
    case ValPrimary2(_, xs, _) => Size(convert(xs))
    case ValPrimary3(_, predicate, _, body) => Unfolding(convert(predicate), convert(body))
    case ValPrimary4(_, expr, _, indep, _) => ???
    case ValPrimary5(_, x, _, xs, _) => AmbiguousMember(convert(x), convert(xs))
    case ValPrimary6(_, from, _, to, _) => Range(convert(from), convert(to))
    case ValPrimary7(_) => ???
    case ValPrimary8(_) => ???
    case ValPrimary9(_, binder, t, id, _, from, _, to, _, body, _) =>
      val name = convert(id)
      val variable = new Variable(convert(t))(SourceNameOrigin(name, origin(id)))
      val condition = SeqMember(Local(new DirectRef(variable)), Range(convert(from), convert(to)))
      val main = convert(body)
      binder match {
        case "\\forall*" => Starall(Seq(variable), Nil, Implies(condition, main))
        case "\\forall" => Forall(Seq(variable), Nil, Implies(condition, main))
        case "\\exists" => Exists(Seq(variable), Nil, col.And(condition, main))
      }
    case ValPrimary10(_, binder, t, id, _, cond, _, body, _) =>
      val variable = new Variable(convert(t))(SourceNameOrigin(convert(id), origin(id)))
      binder match {
        case "\\forall*" => Starall(Seq(variable), Nil, Implies(convert(cond), convert(body)))
        case "\\forall" => Forall(Seq(variable), Nil, Implies(convert(cond), convert(body)))
        case "\\exists" => Exists(Seq(variable), Nil, col.And(convert(cond), convert(body)))
      }
    case ValPrimary11(_, _, t, id, _, binding, _, body, _) =>
      Let(new Variable(convert(t))(SourceNameOrigin(convert(id), origin(id))),
        convert(binding), convert(body))
    case ValPrimary12(_, _, t, id, _, cond, _, body, _) =>
      Sum(Seq(new Variable(convert(t))(SourceNameOrigin(convert(id), origin(id)))),
        convert(cond), convert(body))
    case ValPrimary13(_, _, xs, _) => Length(convert(xs))
    case ValPrimary14(_, _, inner, _) => Old(convert(inner), at=None)(blameProvider(e))
    case ValPrimary15(_, _, inner, _) => TypeOf(convert(inner))
    case ValPrimary16(_, _, matrix, _, width, _, height, _) =>
      ValidMatrix(convert(matrix), convert(width), convert(height))
    case ValPrimary17(_, _, array, _, length, _) => ValidArray(convert(array), convert(length))
    case ValPrimary18(_, _, pointer, _, length, _, perm, _) =>
      PermPointer(convert(pointer), convert(length), convert(perm))
    case ValPrimary19(_, _, pointer, _, index, _, perm, _) =>
      PermPointerIndex(convert(pointer), convert(index), convert(perm))
    case ValPrimary20(_, _, array, _, from, _, to, _) =>
      Values(convert(array), convert(from), convert(to))
    case ValPrimary21(_, _, _, _, _, _) => ???
    case ValPrimary22(_, _, _, _, _, _) => ???
    case ValPrimary23(_, _, _, _) => ???
    case ValPrimary24(_, _, _, _, _, _) => ???
    case ValPrimary25(_, _, _, _, _, _) => ???
    case ValPrimary26(_, _, _, _) => ???
    case ValPrimary27(_, _, _) => ??? // FIXME PB: consider just dropping this syntax (used in -*)
    case ValPrimary28(_, trigger, _) => InlinePattern(convert(trigger))
    case ValPrimary29(_, _, expr, _, reducibleOp, _) => ???
    case ValPrimary30(_, _, future, _, state, _) => ???
    case ValPrimary31(_, _, _, _, _, _) => ??? // FIXME PB: unused
    case ValPrimary32(_, _, loc, _, perm, _) => ActionPerm(convert(loc), convert(perm))
    case ValPrimary33(_, _, _, _, _, _, _, _, _, _, _, _) => ??? // FIXME PB: unused
    case ValPrimary34(_, _, map, _, key, _, value, _) => MapCons(convert(map), convert(key), convert(value))
    case ValPrimary35(_, _, map, _) => MapSize(convert(map))
    case ValPrimary36(_, _, _, _, _, _) => ???
    case ValPrimary37(_, _, map1, _, map2, _) => MapDisjoint(convert(map1), convert(map2))
    case ValPrimary38(_, _, map1, _, map2, _) => MapEq(convert(map1), convert(map2))
    case ValPrimary39(_, _, future, _, perm, _, process, _) => ???
    case ValPrimary40(_, _, map, _, key, _) => MapGet(convert(map), convert(key))
    case ValPrimary41(_, _, tup, _) => TupGet(convert(tup), 0)
    case ValPrimary42(_, _, opt, _) => OptGet(convert(opt))
    case ValPrimary43(_, _, tup, _) => TupGet(convert(tup), 1)
    case ValPrimary44(_, _, xs, _) => Head(convert(xs))
    case ValPrimary45(_, _, obj, _) => Held(convert(obj))
    case ValPrimary46(_, _, model, _, perm, _, state, _) => ??? // FIXME PB: remove in favour of Future -> Model?
    case ValPrimary47(_, _, loc, _, perm, _) => ModelPerm(convert(loc), convert(perm))
    case ValPrimary48(_, _, thread, _) => IdleToken(convert(thread))
    case ValPrimary49(_, _, xs, _) => Empty(convert(xs))
    case ValPrimary50(_, _, map, _) => MapItemSet(convert(map))
    case ValPrimary51(_, _, map, _) => MapKeySet(convert(map))
    case ValPrimary52(_, _, loc, _) => CurPerm(convert(loc))
    case ValPrimary53(_, _, loc, _, perm, _) => Perm(convert(loc), convert(perm))
    case ValPrimary54(_, _, loc, _, perm, _, value, _) => PointsTo(convert(loc), convert(perm), convert(value))
    case ValPrimary55(_, _, xs, _, i, _) => RemoveAt(convert(xs), convert(i))
    case ValPrimary56(_, _, map, _, key, _) => MapRemove(convert(map), convert(key))
    case ValPrimary57(_, _, thread, _) => JoinToken(convert(thread))
    case ValPrimary58(_, _, x, _) => OptSome(convert(x))
    case ValPrimary59(_, _, xs, _) => Tail(convert(xs))
    case ValPrimary60(_, _, loc, _) => Perm(convert(loc), ReadPerm())
    case ValPrimary61(_, _, map, _) => MapValueSet(convert(map))
    case ValPrimary62(_, _, elementType, _, _, xs, _) => LiteralSeq(convert(elementType), convert(xs))
    case ValPrimary63(_, _, elementType, _, _, xs, _) => LiteralSet(convert(elementType), convert(xs))
    case ValPrimary64(_, xs, _, _, len, _, _) => Take(convert(xs), convert(len))
    case ValPrimary65(_, xs, _, start, _, maybeEnd, _, _) => maybeEnd match {
      case None => Drop(convert(xs), convert(start))
      case Some(end) => Slice(convert(xs), convert(start), convert(end))
    }
    case ValPrimary66(_, xs, _, idx, _, replacement, _, _) =>
      SeqUpdate(convert(xs), convert(idx), convert(replacement))
    case ValPrimary67(_, x, _, xs, _) => Cons(convert(x), convert(xs))
    case ValPrimary68(_, xs, _, ys, _) => Concat(convert(xs), convert(ys))
    case ValPrimary69(_, x, _, xs, _) => AmbiguousMember(convert(x), convert(xs)) // PB: duplicate?
    case ValPrimary70(_, _, opt, _, alt, _) => OptGetOrElse(convert(opt), convert(alt))
  }

  def convert(implicit res: ValReservedContext): Expr = res match {
    case ValReserved0(name) => fail(res,
      f"This identifier is reserved, and cannot be declared or used in specifications. " +
        f"You might want to escape the identifier with backticks: `$name`")
    case ValReserved1(id) => Local(new UnresolvedRef(id.substring(1, id.length-1)))
    case ValReserved2(_) => ???
    case ValReserved3(_) => ???
    case ValReserved4(_) => NoPerm()
    case ValReserved5(_) => WritePerm()
    case ValReserved6(_) => ReadPerm()
    case ValReserved7(_) => OptNone()
    case ValReserved8(_) => EmptyProcess()
    case ValReserved9(_) => ???
    case ValReserved10(_) => ???
    case ValReserved11(_) => true
    case ValReserved12(_) => false
  }
}
