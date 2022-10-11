package vct.parsers.transform

import hre.util.FuncTools
import org.antlr.v4.runtime.{ParserRuleContext, Token}
import vct.col.ast._
import vct.antlr4.generated.JavaParser._
import vct.antlr4.generated.JavaParserPatterns._
import vct.col.{ast => col}
import vct.col.origin._
import vct.antlr4.generated.{JavaParserPatterns => parse}
import vct.col.util.AstBuildHelpers._
import vct.col.ref.{Ref, UnresolvedRef}
import vct.col.resolve.lang.Java
import vct.col.util.AstBuildHelpers

import scala.annotation.nowarn
import scala.collection.mutable

@nowarn("msg=match may not be exhaustive&msg=Some\\(")
case class JavaToCol[G](override val originProvider: OriginProvider, override val blameProvider: BlameProvider, override val errors: Seq[(Token, Token, ExpectedError)])
  extends ToCol[G](originProvider, blameProvider, errors) {
  def convert(implicit unit: CompilationUnitContext): Seq[GlobalDeclaration[G]] = unit match {
    case CompilationUnit0(pkg, imports, decls, _) =>
      Seq(new JavaNamespace(pkg.map(convert(_)), imports.map(convert(_)), decls.flatMap(convert(_))))
  }

  def convert(implicit pkg: PackageDeclarationContext): JavaName[G] = pkg match {
    case PackageDeclaration0(_, _, name, _) => convert(name)
  }

  def convert(implicit imp: ImportDeclarationContext): JavaImport[G] = imp match {
    case ImportDeclaration0(_, isStatic, name, star, _) => JavaImport(isStatic.nonEmpty, convert(name), star.nonEmpty)
  }

  def convert(implicit decl: TypeDeclarationContext): Seq[GlobalDeclaration[G]] = decl match {
    case TypeDeclaration0(mods, ClassDeclaration0(contract, _, name, args, ext, imp, ClassBody0(_, decls, _))) =>
      withContract(contract, contract => {
        Seq(new JavaClass(convert(name), mods.map(convert(_)), args.map(convert(_)).getOrElse(Nil),
          AstBuildHelpers.foldStar(contract.consume(contract.lock_invariant)),
          ext.map(convert(_)).getOrElse(Java.JAVA_LANG_OBJECT),
          imp.map(convert(_)).getOrElse(Nil), decls.flatMap(convert(_))))
      })
    case TypeDeclaration1(mods, enum) => fail(enum, "Enums are not supported.")
    case TypeDeclaration2(mods, InterfaceDeclaration0(_, name, args, ext, InterfaceBody0(_, decls, _))) =>
      Seq(new JavaInterface(convert(name), mods.map(convert(_)), args.map(convert(_)).getOrElse(Nil),
        ext.map(convert(_)).getOrElse(Nil), decls.flatMap(convert(_))))
    case TypeDeclaration3(mods, AnnotationTypeDeclaration0(_, _, name, AnnotationTypeBody0(_, decls, _))) =>
      Seq(new JavaAnnotationInterface(convert(name), mods.map(convert(_)), Java.JAVA_LANG_ANNOTATION, decls.map(convert(_)).flatten))
    case TypeDeclaration4(inner) => convert(inner)
    case TypeDeclaration5(_) => Nil
  }

  def convert(implicit decl: AnnotationTypeElementDeclarationContext): Option[JavaAnnotationMethod[G]] = decl match {
    case AnnotationTypeElementDeclaration0(mods, AnnotationTypeElementRest0(returnType, AnnotationMethodOrConstantRest0(AnnotationMethodRest0(name, _, _, default)), _)) =>
      val modifiers: Set[JavaModifier[G]] = mods.map(convert(_)).toSet
      if (!modifiers.subsetOf(Set(JavaPublic()(DiagnosticOrigin), JavaAbstract()(DiagnosticOrigin)))) {
        fail(decl, "Only modifiers allowed on @interface members are public, abstract")
      }
      Some(new JavaAnnotationMethod(convert(returnType), convert(name), default.map(convert(_))))
    case AnnotationTypeElementDeclaration1(_) => None
  }

  def convert(implicit default: DefaultValueContext): Expr[G] = default match {
    case DefaultValue0(_, ElementValue0(expr)) => convert(expr)
    case DefaultValue0(_, elementValue) => ??(elementValue)
  }

  def convert(implicit modifier: ModifierContext): JavaModifier[G] = modifier match {
    case Modifier0(modifier) => convert(modifier)
    case Modifier1(name) => name match {
      case "native" => JavaNative()
      case "synchronized" => JavaSynchronized()(blame(modifier))
      case "transient" => JavaTransient()
      case "volatile" => JavaVolatile()
    }
    case Modifier2(mods) => withModifiers(mods, m => {
      if(m.consume(m.pure)) JavaPure[G]()
      else if(m.consume(m.inline)) JavaInline[G]()
      else fail(m.nodes.head, "This modifier cannot be attached to a declaration in Java")
    })
  }

  def convert(implicit modifier: ClassOrInterfaceModifierContext): JavaModifier[G] = modifier match {
    case ClassOrInterfaceModifier0(annotation) => convert(annotation)
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

  def convert(implicit annotation: AnnotationContext): JavaAnnotation[G] = annotation match {
    case Annotation0(_, name, annotationArgs) => JavaAnnotation(convert(name), annotationArgs.map(convert(_)).getOrElse(Seq()))
  }

  def convert(implicit annotationName: AnnotationNameContext): JavaNamedType[G] = annotationName match {
    case AnnotationName0(qualifiedName) => JavaNamedType(convert(qualifiedName).names.map(part => (part, None)))
  }

  def convert(implicit annotationArgs: AnnotationArgsContext): Seq[(String, Expr[G])] = annotationArgs match {
    case AnnotationArgs0(_, None, _ ) => Seq()
    case AnnotationArgs0(_, Some(AnnotationArgsElems0(pairs)), _ ) => convert(pairs)
    case AnnotationArgs0(_, Some(AnnotationArgsElems1(ElementValue0(expr))), _) => Seq(("value", convert(expr)))
    case _ => ??(annotationArgs)
  }

  def convert(implicit pairs: ElementValuePairsContext): Seq[(String, Expr[G])] = pairs match {
    case ElementValuePairs0(pair) => Seq(convert(pair))
    case ElementValuePairs1(more, _, pair) => convert(more) :+ convert(pair)
    case _ => ???
  }

  def convert(implicit pair: ElementValuePairContext): (String, Expr[G]) = pair match {
    case ElementValuePair0(name, _, ElementValue0(expr)) => (convert(name), convert(expr))
    case x => ??(x)
  }

  def convert(implicit modifier: VariableModifierContext): JavaModifier[G] = modifier match {
    case VariableModifier0(_) => JavaFinal()
    case VariableModifier1(annotation) => convert(annotation)
  }

  def convert(implicit args: TypeParametersContext): Seq[Variable[G]] = args match {
    case TypeParameters0(_, args, _) => convert(args)
  }

  def convert(implicit args: TypeParameterListContext): Seq[Variable[G]] = args match {
    case TypeParameterList0(arg) => Seq(convert(arg))
    case TypeParameterList1(arg, _, args) => convert(arg) +: convert(args)
  }

  def convert(implicit arg: TypeParameterContext): Variable[G] = arg match {
    case TypeParameter0(id, bound) => bound match {
      case None =>
        new Variable(TType(Java.JAVA_LANG_OBJECT))(SourceNameOrigin(convert(id), origin(arg)))
      case Some(TypeParameterBound0(_, TypeBound0(ext))) =>
        new Variable(TType(convert(ext)))(SourceNameOrigin(convert(id), origin(arg)))
      case Some(TypeParameterBound0(_, TypeBound1(_, _, moreBounds))) =>
        fail(moreBounds, "Multiple upper type bounds are not supported")
    }
  }

  def convert(implicit decl: ClassBodyDeclarationContext): Seq[ClassDeclaration[G]] = decl match {
    case ClassBodyDeclaration0(_) => Nil
    case ClassBodyDeclaration1(isStatic, body) => Seq(new JavaSharedInitialization(isStatic.nonEmpty, convert(body)))
    case ClassBodyDeclaration2(contract, mods, decl) =>
      withContract(contract, c => {
        convert(decl, mods.map(convert(_)), c)
      })
    case ClassBodyDeclaration3(inner) => convert(inner)
  }

  def convert(implicit decl: InterfaceBodyDeclarationContext): Seq[ClassDeclaration[G]] = decl match {
    case InterfaceBodyDeclaration0(contract, mods, decl) =>
      withContract(contract, c => {
        convert(decl, mods.map(convert(_)), c)
      })
    case InterfaceBodyDeclaration1(inner) => convert(inner)
    case InterfaceBodyDeclaration2(_) => Nil
  }

  def convert(implicit decl: MemberDeclarationContext, mods: Seq[JavaModifier[G]], c: ContractCollector[G]): Seq[ClassDeclaration[G]] = decl match {
    case MemberDeclaration0(MethodDeclaration0(returnType, name, params, dims, signals, body)) =>
      Seq(new JavaMethod(mods, convert(returnType), dims.map(convert(_)).getOrElse(0),
        convert(name), convert(params), Nil, signals.map(convert(_)).getOrElse(Nil),
        convert(body), c.consumeApplicableContract(blame(decl)))(blame(decl)))
    case MemberDeclaration1(GenericMethodDeclaration0(typeParams, MethodDeclaration0(
      returnType, name, params, dims, signals, body))) =>
      Seq(new JavaMethod(mods, convert(returnType), dims.map(convert(_)).getOrElse(0),
        convert(name), convert(params), convert(typeParams), signals.map(convert(_)).getOrElse(Nil),
        convert(body), c.consumeApplicableContract(blame(decl)))(blame(decl)))
    case MemberDeclaration2(FieldDeclaration0(t, decls, _)) =>
      // Ignore the contract collector, so that complains about being non-empty
      Seq(new JavaFields(mods, convert(t), convert(decls)))
    case MemberDeclaration3(ConstructorDeclaration0(name, params, signals, ConstructorBody0(body))) =>
      Seq(new JavaConstructor(mods, convert(name), convert(params), Nil,
        signals.map(convert(_)).getOrElse(Nil), convert(body), c.consumeApplicableContract(blame(decl)))(blame(decl)))
    case MemberDeclaration4(GenericConstructorDeclaration0(typeParams,
      ConstructorDeclaration0(name, params, signals, ConstructorBody0(body)))) =>
      Seq(new JavaConstructor(mods, convert(name), convert(params), convert(typeParams),
        signals.map(convert(_)).getOrElse(Nil), convert(body), c.consumeApplicableContract(blame(decl)))(blame(decl)))
    case MemberDeclaration5(interface) => fail(interface, "Inner interfaces are not supported.")
    case MemberDeclaration6(annotation) => fail(annotation, "Annotations are not supported.")
    case MemberDeclaration7(cls) => fail(cls, "Inner classes are not supported.")
    case MemberDeclaration8(enum) => fail(enum, "Enums are not supported.")
  }

  def convert(implicit decl: InterfaceMemberDeclarationContext, mods: Seq[JavaModifier[G]], c: ContractCollector[G]): Seq[ClassDeclaration[G]] = decl match {
    case InterfaceMemberDeclaration0(ConstDeclaration0(t, decls, _)) =>
      // JLS SE 7 - 9.3
      Seq(new JavaFields(Seq(JavaPublic[G](), JavaStatic[G](), JavaFinal[G]()) ++ mods, convert(t), convert(decls)))
    case InterfaceMemberDeclaration1(InterfaceMethodDeclaration0(t, name, params, dims, signals, _)) =>
      // JLS SE 7 - 9.4
      Seq(new JavaMethod(Seq(JavaPublic[G](), JavaAbstract[G]()) ++ mods, convert(t), dims.map(convert(_)).getOrElse(0),
        convert(name), convert(params), Nil, signals.map(convert(_)).getOrElse(Nil), None, c.consumeApplicableContract(blame(decl)))(blame(decl)))
    case InterfaceMemberDeclaration2(GenericInterfaceMethodDeclaration0(typeParams, InterfaceMethodDeclaration0(
      t, name, params, dims, signals, _))) =>
      Seq(new JavaMethod(Seq(JavaPublic[G](), JavaAbstract[G]()) ++ mods, convert(t), dims.map(convert(_)).getOrElse(0),
        convert(name), convert(params), convert(typeParams), signals.map(convert(_)).getOrElse(Nil),
        None, c.consumeApplicableContract(blame(decl)))(blame(decl)))
    case InterfaceMemberDeclaration3(interface) => fail(interface, "Inner interfaces are not supported.")
    case InterfaceMemberDeclaration4(annotation) => fail(annotation, "Annotations are not supported.")
    case InterfaceMemberDeclaration5(cls) => fail(cls, "Inner classes are not supported.")
    case InterfaceMemberDeclaration6(enum) => fail(enum, "Enums are not supported.")
  }

  def convert(implicit decls: VariableDeclaratorsContext): Seq[JavaVariableDeclaration[G]] = decls match {
    case VariableDeclarators0(decl) => Seq(convert(decl))
    case VariableDeclarators1(decl, _, decls) => convert(decl) +: convert(decls)
  }

  def convert(implicit decl: VariableDeclaratorContext): JavaVariableDeclaration[G] = decl match {
    case VariableDeclarator0(VariableDeclaratorId0(name, dims), init) =>
      JavaVariableDeclaration(convert(name), dims.map(convert(_)).getOrElse(0), init.map(convert(_)))
  }

  def convert(implicit decls: ConstantDeclaratorListContext): Seq[JavaVariableDeclaration[G]] = decls match {
    case ConstantDeclaratorList0(decl) => Seq(convert(decl))
    case ConstantDeclaratorList1(decl, _, decls) => convert(decl) +: convert(decls)
  }

  def convert(implicit decl: ConstantDeclaratorContext): JavaVariableDeclaration[G] = decl match {
    case ConstantDeclarator0(name, dims, _, init) =>
      JavaVariableDeclaration(convert(name), dims.map(convert(_)).getOrElse(0), Some(convert(init)))
  }

  def convert(implicit params: FormalParametersContext): Seq[Variable[G]] = params match {
    case FormalParameters0(_, params, _) => params.map(convert(_)).getOrElse(Nil)
  }

  def convert(implicit params: FormalParameterListContext): Seq[Variable[G]] = params match {
    case FormalParameterList0(varargs) => ??(varargs)
    case FormalParameterList1(params) => convert(params)
    case FormalParameterList2(_, _, varargs) => ??(varargs)
  }

  def convert(implicit params: InitFormalParameterListContext): Seq[Variable[G]] = params match {
    case InitFormalParameterList0(param) => Seq(convert(param))
    case InitFormalParameterList1(param, _, params) => convert(param) +: convert(params)
  }

  def convert(implicit param: FormalParameterContext): Variable[G] = param match {
    case FormalParameter0(_, tNode, nameDims) =>
      val (name, dims) = convert(nameDims)
      val t = FuncTools.repeat(TArray[G](_), dims, convert(tNode))
      new Variable(t)(SourceNameOrigin(name, origin(param)))
  }

  def convert(implicit dims: DimsContext): Int = dims match {
    case Dims0(dims) => dims.size
  }

  def convert(implicit names: QualifiedNameListContext): Seq[JavaName[G]] = names match {
    case QualifiedNameList0(name) => Seq(convert(name))
    case QualifiedNameList1(name, _, names) => convert(name) +: convert(names)
  }

  def convert(implicit name: QualifiedNameContext): JavaName[G] = name match {
    case QualifiedName0(id) => JavaName(Seq(convert(id)))
    case QualifiedName1(id, _, names) => JavaName(convert(id) +: convert(names).names)
  }

  def convert(implicit ts: IntExtContext): Seq[Type[G]] = ts match {
    case IntExt0(_, ts) => convert(ts)
  }

  def convert(implicit t: ExtContext): Type[G] = t match {
    case Ext0(_, t) => convert(t)
  }

  def convert(implicit ts: ImpContext): Seq[Type[G]] = ts match {
    case Imp0(_, ts) => convert(ts)
  }

  def convert(implicit ts: TypeListContext): Seq[Type[G]] = ts match {
    case TypeList0(t) => Seq(convert(t))
    case TypeList1(t, _, ts) => convert(t) +: convert(ts)
  }

  def convert(implicit signals: ThrowyContext): Seq[JavaNamedType[G]] = signals match {
    case Throwy0(_, ts) =>
      convert(ts).map(name => JavaNamedType(name.names.map(part => (part, None))))
  }

  def convert(implicit stat: MethodBodyOrEmptyContext): Option[Statement[G]] = stat match {
    case MethodBodyOrEmpty0(_) => None
    case MethodBodyOrEmpty1(MethodBody0(body)) => Some(convert(body))
  }

  def convert(implicit stat: BlockContext): Statement[G] = stat match {
    case Block0(_, stats, _) => Scope(Nil, Block(stats.map(convert(_))))
  }

  def convert(implicit stat: BlockStatementContext): Statement[G] = stat match {
    case BlockStatement0(decl) => convert(decl)
    case BlockStatement1(statement) => convert(statement)
    case BlockStatement2(decl) => ??(decl)
    case BlockStatement3(inner) => convert(inner)
  }

  def convert(implicit decl: LocalVariableDeclarationStatementContext): Statement[G] = decl match {
    case LocalVariableDeclarationStatement0(decl, _) => convert(decl)
  }

  def convert(implicit decl: LocalVariableDeclarationContext): Statement[G] = decl match {
    case LocalVariableDeclaration0(mods, t, decls) =>
      JavaLocalDeclarationStatement(new JavaLocalDeclaration(mods.map(convert(_)), convert(t), convert(decls)))
  }

  def convert(implicit stat: ElseBlockContext): Statement[G] = stat match {
    case ElseBlock0(_, stat) => convert(stat)
  }

  def convert(prefix: LoopAmalgamationContext, f: ContractCollector[G] => Statement[G], labels: Seq[LabelDecl[G]], collector: ContractCollector[G]): Statement[G] = prefix match {
    case LoopAmalgamation0(contract, tail) =>
      convert(contract, collector)
      convert(tail, f, labels, collector)
    case LoopAmalgamation1(label, tail) =>
      convert(tail, f, labels :+ convert(label), collector)
    case LoopAmalgamation2(_) =>
      labels.foldRight[Statement[G]](f(collector)) {
        case (label, stat) => Label(label, stat)(label.o)
      }
  }

  def convert(prefix: LoopAmalgamationContext, f: ContractCollector[G] => Statement[G]): Statement[G] =
    convert(prefix, f, Nil, new ContractCollector())


  def convert(implicit stat: StatementContext): Statement[G] = stat match {
    case Statement0(block) => convert(block)
    case Statement1(_, assn, _, _) => Assert(convert(assn))(blame(stat))
    case Statement2(_, cond, body, otherwise) =>
      Branch(Seq((convert(cond), convert(body))) ++ (otherwise match {
        case None => Nil
        case Some(otherwise) => Seq((BooleanValue(true), convert(otherwise)))
      }))
    case Statement3(prefix, _, _, control, _, contract2, body) =>
      convert(prefix, c => {
        contract2.foreach(contract => convert(contract, c))
        control match {
          case ForControl0(foreach) => ??(foreach)
          case ForControl1(init, _, cond, _, update) =>
            Scope(Nil, Loop(
              init.map(convert(_)).getOrElse(Block(Nil)),
              cond.map(convert(_)).getOrElse(tt),
              update.map(convert(_)).getOrElse(Block(Nil)),
              c.consumeLoopContract(stat),
              convert(body)
            ))
        }
      })
    case Statement4(prefix, _, cond, contract2, body) =>
      convert(prefix, c => {
        contract2.foreach(contract => convert(contract, c))
        Scope(Nil, Loop(Block(Nil), convert(cond), Block(Nil), c.consumeLoopContract(stat), convert(body)))
      })
    case Statement5(_, _, _, _, _) => ??(stat)
    case Statement6(_, attempt, grab, eventually) =>
      TryCatchFinally(convert(attempt), eventually.map(convert(_)).getOrElse(Block(Nil)), grab.map(convert(_)))
    case Statement7(_, attempt, eventually) =>
      TryCatchFinally(convert(attempt), convert(eventually), Nil)
    case Statement8(_, _, _, _, _) => ??(stat)
    case Statement9(_, expr, _, casedStatements, trailingCases, _) =>
      Switch(convert(expr), Block(casedStatements.flatMap(convert(_)) ++ trailingCases.map(convert(_))))
    case Statement10(_, obj, inner) => Synchronized(convert(obj), convert(inner))(blame(stat))
    case Statement11(_, expr, _) => Return(expr.map(convert(_)).getOrElse(Void()))
    case Statement12(_, exc, _) => Throw(convert(exc))(blame(stat))
    case Statement13(_, label, _) => Break(label.map(convert(_)).map(new UnresolvedRef[G, LabelDecl[G]](_)))
    case Statement14(_, label, _) => Continue(label.map(convert(_)).map(new UnresolvedRef[G, LabelDecl[G]](_)))
    case Statement15(_) => Block(Nil)
    case Statement16(expr, _) => Eval(convert(expr))
    case Statement17(label, _, statement) =>
      Label(
        new LabelDecl()(SourceNameOrigin(convert(label), origin(stat))),
        convert(statement),
      )
    case Statement18(inner) => convert(inner)
  }

  def convert(implicit label: LoopLabelContext): LabelDecl[G] = label match {
    case LoopLabel0(name, _) => new LabelDecl()(SourceNameOrigin(convert(name), origin(label)))
  }

  def convert(implicit switchBlock: SwitchBlockStatementGroupContext): Seq[Statement[G]] = switchBlock match {
    case SwitchBlockStatementGroup0(labels, statements) =>
      labels.map(convert(_)) ++ statements.map(convert(_))
  }

  def convert(implicit switchLabel: SwitchLabelContext): Statement[G] = switchLabel match {
    case SwitchLabel0(_, expr, _) => Case(convert(expr))
    case SwitchLabel1(_, enum, _) => ??(enum)
    case SwitchLabel2(_, _) => DefaultCase()
  }

  def convert(implicit stat: ForInitContext): Statement[G] = stat match {
    case ForInit0(locals) => convert(locals)
    case ForInit1(exprs) => Block(convert(exprs).map(Eval(_)))
  }

  def convert(implicit stat: ForUpdateContext): Statement[G] = stat match {
    case ForUpdate0(exprs) => Block(convert(exprs).map(Eval(_)))
  }

  def convert(implicit stat: FinallyBlockContext): Statement[G] = stat match {
    case FinallyBlock0(_, block) => convert(block)
  }

  def convert(implicit grab: CatchClauseContext): CatchClause[G] = grab match {
    case CatchClause0(_, _, _mods, ts, id, _, body) =>
      CatchClause(new Variable(convert(ts))(SourceNameOrigin(convert(id), origin(grab))), convert(body))
  }

  def convert(implicit ts: CatchTypeContext): TUnion[G] = ts match {
    case CatchType0(name) => TUnion(Seq(convert(name)))
    case CatchType1(name, _, names) => TUnion(convert(name) +: convert(names).types)
  }

  def convert(implicit ts: NonWildcardTypeArgumentsContext): Seq[Type[G]] = ts match {
    case NonWildcardTypeArguments0(_, ts, _) => convert(ts)
  }

  def convert(implicit t: TypeOrVoidContext): Type[G] = t match {
    case TypeOrVoid0(_) => TVoid()
    case TypeOrVoid1(t) => convert(t)
  }

  def convert(implicit t: TypeContext): Type[G] = t match {
    case Type0(inner) => convert(inner)
    case Type1(element, dims) => FuncTools.repeat(TArray[G](_), dims.map(convert(_)).getOrElse(0), convert(element))
    case Type2(element, dims) => FuncTools.repeat(TArray[G](_), dims.map(convert(_)).getOrElse(0), convert(element))
  }

  def convert(implicit t: ClassOrInterfaceTypeContext): JavaNamedType[G] = t match {
    case ClassOrInterfaceType0(name, args) =>
      JavaNamedType(Seq((convert(name), args.map(convert(_)))))
    case ClassOrInterfaceType1(names, _, name, args) =>
      JavaNamedType(convert(names).names :+ (convert(name), args.map(convert(_))))
  }

  def convert(implicit t: CreatedNameContext): Type[G] = t match {
    case CreatedName0(t) => convert(t)
    case CreatedName1(t) => convert(t)
  }

  def convert(implicit t: ClassTypeDiamondListContext): JavaNamedType[G] = t match {
    case ClassTypeDiamondList0(name, typeArgs) =>
      JavaNamedType(Seq((convert(name), typeArgs.map(convert(_)))))
    case ClassTypeDiamondList1(name, typeArgs, _, more) =>
      JavaNamedType((convert(name), typeArgs.map(convert(_))) +: convert(more).names)
  }

  def convert(implicit ts: TypeArgumentsOrDiamondContext): Seq[Type[G]] = ts match {
    case TypeArgumentsOrDiamond0(_, _) => Nil
    case TypeArgumentsOrDiamond1(ts) => convert(ts)
  }

  def convert(implicit t: PrimitiveTypeContext): Type[G] = t match {
    case PrimitiveType0(name) => name match {
      case "boolean" => TBool()
      case "char" => ??(t)
      case "byte" => TInt()
      case "short" => TInt()
      case "int" => TInt()
      case "long" => TInt()
      case "float" => Java.float
      case "double" => Java.double
    }
  }

  def convert(implicit ts: TypeArgumentsContext): Seq[Type[G]] = ts match {
    case TypeArguments0(_, ts, _) => convert(ts)
  }

  def convert(implicit ts: TypeArgumentListContext): Seq[Type[G]] = ts match {
    case TypeArgumentList0(arg) => Seq(convert(arg))
    case TypeArgumentList1(arg, _, args) => convert(arg) +: convert(args)
  }

  def convert(implicit t: TypeArgumentContext): Type[G] = t match {
    case TypeArgument0(t) => convert(t)
    case other: TypeArgument1Context => ??(other)
  }

  def convert(implicit expr: VariableDeclaratorInitContext): Expr[G] = expr match {
    case VariableDeclaratorInit0(_, init) => convert(init)
  }

  def convert(implicit expr: VariableInitializerContext): Expr[G] = expr match {
    case VariableInitializer0(arrayInit) => convert(arrayInit)
    case VariableInitializer1(expr) => convert(expr)
  }

  def convert(implicit expr: ArrayInitializerContext): Expr[G] = expr match {
    case ArrayInitializer0(_, _) => JavaLiteralArray(Nil)
    case ArrayInitializer1(_, exprs, _, _) => JavaLiteralArray(convert(exprs))
  }

  def convert(implicit expr: ParExpressionContext): Expr[G] = expr match {
    case ParExpression0(_, expr, _) => convert(expr)
  }

  def convert(implicit exprs: ExpressionListContext): Seq[Expr[G]] = exprs match {
    case ExpressionList0(expr) => Seq(convert(expr))
    case ExpressionList1(expr, _, exprs) => convert(expr) +: convert(exprs)
  }

  def convert(implicit exprs: VariableInitializerListContext): Seq[Expr[G]] = exprs match {
    case VariableInitializerList0(expr) => Seq(convert(expr))
    case VariableInitializerList1(expr, _, exprs) => convert(expr) +: convert(exprs)
  }

  def convert(implicit exprs: SpecifiedDimsContext): Seq[Expr[G]] = exprs match {
    case SpecifiedDims0(dim) => Seq(convert(dim))
    case SpecifiedDims1(dim, dims) => convert(dim) +: convert(dims)
  }

  def convert(implicit exprs: ArgumentsContext): Seq[Expr[G]] = exprs match {
    case Arguments0(_, exprs, _) => exprs.map(convert(_)).getOrElse(Nil)
  }

  def convert(implicit expr: ConstantExpressionContext): Expr[G] = expr match {
    case ConstantExpression0(expr) => convert(expr)
  }

  def convert(implicit expr: StatementExpressionContext): Expr[G] = expr match {
    case StatementExpression0(expr) => convert(expr)
  }

  def convert(implicit expr: SpecifiedDimContext): Expr[G] = expr match {
    case SpecifiedDim0(_, expr, _) => convert(expr)
  }

  def convert(implicit expr: ExpressionContext): Expr[G] = expr match {
    case Expression0(whiff, inner, den) =>
      convertEmbedWith(whiff, convertEmbedThen(den, convert(inner)))
  }

  def convert(implicit expr: ExprContext): Expr[G] = expr match {
    case JavaPrimary(inner) => convert(inner)
    case parse.JavaDeref(obj, _, field) => col.JavaDeref(convert(obj), convert(field))(blame(expr))
    case JavaPinnedThis(innerOrOuterClass, _, _) => ??(expr)
    case JavaPinnedOuterClassNew(pinnedOuterClassObj, _, _, _, _) => ??(expr)
    case JavaSuper(_, _, _, _) => ??(expr)
    case JavaGenericInvocation(obj, _, ExplicitGenericInvocation0(typeArgs, invocation)) =>
      convert(invocation, Some(convert(obj)), convert(typeArgs))
    case JavaSubscript(ar, _, idx, _) => AmbiguousSubscript(convert(ar), convert(idx))(blame(expr))
    case JavaNonNullInvocation(obj, _, name, args) =>
      Implies(
        Neq(convert(obj), Null()),
        col.JavaInvocation(Some(convert(obj)), Nil, convert(name), convert(args), Nil, Nil)(blame(expr)),
      )
    case parse.JavaInvocation(obj, _, name, familyType, args, given, yields) =>
      failIfDefined(familyType, "Predicate families not supported (for now)")
      col.JavaInvocation(
        Some(convert(obj)), Nil, convert(name), convert(args),
        convertEmbedGiven(given), convertEmbedYields(yields))(
        blame(expr))
    case JavaValPostfix(expr, PostfixOp0(valPostfix)) => convert(valPostfix, convert(expr))
    case JavaNew(_, creator, given, yields) =>
      convert(creator, convertEmbedGiven(given), convertEmbedYields(yields))
    case JavaCast(_, t, _, inner) => Cast(convert(inner), TypeValue(convert(t)))
    case JavaPostfixIncDec(inner, postOp) =>
      val target = convert(inner)
      postOp match {
        case "++" => PostAssignExpression(target, Plus(target, const(1)))(blame(expr))
        case "--" => PostAssignExpression(target, Minus(target, const(1)))(blame(expr))
      }
    case JavaPrefixOp(preOp, inner) =>
      val target = convert(inner)
      preOp match {
        case "+" => target // TODO PB: not sure if this is true for IEEE floats
        case "-" => UMinus(target)
        case "++" => PreAssignExpression(target, Plus(target, const(1)))(blame(expr))
        case "--" => PreAssignExpression(target, Minus(target, const(1)))(blame(expr))
      }
    case JavaPrefixOp2(preOp, inner) => preOp match {
      case "~" => BitNot(convert(inner))
      case "!" => Not(convert(inner))
    }
    case JavaValPrefix(PrefixOp0(op), inner) =>
      convert(op, convert(inner))
    case JavaValPrepend(left, PrependOp0(op), right) =>
      convert(op, convert(left), convert(right))
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
      case "+" => AmbiguousPlus(convert(left), convert(right))(blame(expr))
      case "-" => AmbiguousMinus(convert(left), convert(right))(blame(expr))
    }
    case JavaShift(left, shift, right) => shift match {
      case ShiftOp0(_, _) => BitShl(convert(left), convert(right))
      case ShiftOp1(_, _, _) => BitUShr(convert(left), convert(right))
      case ShiftOp2(_, _) => BitShr(convert(left), convert(right))
    }
    case JavaRel(left, comp, right) => comp match {
      case RelOp0("<=") => AmbiguousLessEq(convert(left), convert(right))
      case RelOp0(">=") => AmbiguousGreaterEq(convert(left), convert(right))
      case RelOp0(">") => AmbiguousGreater(convert(left), convert(right))
      case RelOp0("<") => AmbiguousLess(convert(left), convert(right))
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
        case "+=" => AmbiguousPlus(target, value)(blame(right))
        case "-=" => AmbiguousMinus(target, value)(blame(right))
        case "*=" => AmbiguousMult(target,  value)
        case "/=" => FloorDiv(target,  value)(blame(expr))
        case "&=" => AmbiguousComputationalAnd(target, value)
        case "|=" => BitOr(target, value)
        case "^=" => BitXor(target, value)
        case ">>=" => BitShr(target, value)
        case ">>>=" => BitUShr(target, value)
        case "<<=" => BitShl(target, value)
        case "%=" => Mod(target, value)(blame(expr))
      })(blame(expr))
  }

  def convert(implicit invocation: ExplicitGenericInvocationSuffixContext,
              obj: Option[Expr[G]], typeArgs: Seq[Type[G]]): Expr[G] = invocation match {
    case ExplicitGenericInvocationSuffix0(_, _) => ??(invocation)
    case ExplicitGenericInvocationSuffix1(name, familyType, arguments) =>
      // FIXME PB: should support this again somehow, maybe reuse open/close with type syntax instead of fold/unfold?
      failIfDefined(familyType, "Predicate families not supported (for now)")
      col.JavaInvocation(obj, typeArgs, convert(name), convert(arguments), Nil, Nil)(blame(invocation))
  }

  def convert(implicit expr: CreatorContext, givenArgs: Seq[(Ref[G, Variable[G]], Expr[G])], yields: Seq[(Ref[G, Variable[G]], Ref[G, Variable[G]])]): Expr[G] = expr match {
    case Creator0(typeArgs, name, creator) =>
      convert(creator, convert(typeArgs), convert(name), givenArgs, yields)
    case Creator1(name, creator) => creator match {
      case CreatorRest0(array) => convert(array, convert(name))
      case CreatorRest1(cls) => convert(cls, Nil, convert(name), givenArgs, yields)
    }
  }

  def convert(implicit creator: ClassCreatorRestContext, ts: Seq[Type[G]], name: Type[G], givenArgs: Seq[(Ref[G, Variable[G]], Expr[G])], yields: Seq[(Ref[G, Variable[G]], Ref[G, Variable[G]])]): Expr[G] = creator match {
    case ClassCreatorRest0(args, impl) =>
      failIfDefined(impl, "Anonymous classes are not supported")
      JavaNewClass(convert(args), ts, name, givenArgs, yields)(blame(creator))
  }

  def convert(implicit creator: ArrayCreatorRestContext, name: Type[G]): Expr[G] = creator match {
    case ArrayCreatorRest0(dims, init) => JavaNewLiteralArray(name, convert(dims), convert(init))
    case ArrayCreatorRest1(specDims, extraDims) => JavaNewDefaultArray(name, convert(specDims), extraDims.map(convert(_)).getOrElse(0))
  }

  def convert(implicit expr: AnnotatedPrimaryContext): Expr[G] = expr match {
    case AnnotatedPrimary0(pre, inner, post) =>
      convertEmbedWith(pre, convertEmbedThen(post, convert(inner)))
  }

  def convert(implicit expr: PrimaryContext): Expr[G] = expr match {
    case Primary0(_, expr, _) => convert(expr)
    case Primary1(_) => AmbiguousThis()
    case Primary2(_) => ??(expr)
    case Primary3(literal) => convert(literal)
    case Primary4(name) => local(expr, convert(name))
    case Primary5(name, familyType, args, given, yields) =>
      failIfDefined(familyType, "Predicate families are unsupported (for now)")
      col.JavaInvocation(None, Nil, convert(name), convert(args),
        convertEmbedGiven(given), convertEmbedYields(yields))(
        blame(expr))
    case Primary6(_, _, _) => ??(expr)
    case Primary7(_, _, _) => ??(expr)
    case _: Primary8Context => ??(expr)
    case Primary9(inner) => convert(inner)
  }

  def convert(implicit expr: LiteralContext): Expr[G] = expr match {
    case Literal0(i) => const(Integer.parseInt(i))
    case Literal1(n) if n.length > 1 =>
      val (num, t) = n.last match {
        case 'f' | 'F' => (n.init, Java.float[G])
        case 'd' | 'D' => (n.init, Java.double[G])
        case _ => (n, Java.double[G])
      }
      FloatValue(BigDecimal(num), t)
    case Literal2(_) => ??(expr)
    case Literal3(_) => ??(expr)
    case Literal4(value) => value match {
      case "true" => tt
      case "false" => ff
    }
    case Literal5(_) => Null()
  }

  def convert(implicit id: VariableDeclaratorIdContext): (String, Int) = id match {
    case VariableDeclaratorId0(name, Some(dims)) => (convert(name), convert(dims))
    case VariableDeclaratorId0(name, None) => (convert(name), 0)
  }

  def convert(implicit id: JavaIdentifierContext): String = id match {
    case JavaIdentifier0(text) => text
    case JavaIdentifier1(inner) => convert(inner)
  }

  def convert(implicit decl: LangGlobalDeclContext): Seq[GlobalDeclaration[G]] = decl match {
    case LangGlobalDecl0(typeDecl) => convert(typeDecl)
  }

  def convert(implicit decl: LangClassDeclContext): Seq[ClassDeclaration[G]] = decl match {
    case LangClassDecl0(classDecl) => convert(classDecl)
  }

  def convert(implicit stat: LangStatementContext): Statement[G] = stat match {
    case LangStatement0(block) => convert(block)
  }

  def convert(implicit t: LangTypeContext): Type[G] = t match {
    case LangType0(t) => convert(t)
  }

  def convert(implicit e: LangExprContext): Expr[G] = e match {
    case LangExpr0(e) => convert(e)
  }

  def convert(implicit id: LangIdContext): String = id match {
    case LangId0(id) => convert(id)
  }

  def convert(implicit n: LangConstIntContext): BigInt = n match {
    case LangConstInt0(string) => BigInt(string)
  }

  def local(ctx: ParserRuleContext, name: String): Expr[G] =
    JavaLocal(name)(blame(ctx))(origin(ctx))

  def withCollector[T](collector: ContractCollector[G], f: ContractCollector[G] => T): T = {
    val result = f(collector)
    collector.nodes.headOption match {
      case Some(node) => fail(node, "This specification clause may not occur here")
      case None => result
    }
  }

  def withContract[T](node: Option[ValEmbedContractContext], f: ContractCollector[G] => T): T = {
    val collector = new ContractCollector[G]()
    node.foreach(convert(_, collector))
    withCollector(collector, f)
  }

  def withContract[T](node1: Option[ValEmbedContractContext], node2: Option[ValEmbedContractContext], f: ContractCollector[G] => T): T = {
    val collector = new ContractCollector[G]()
    node1.foreach(convert(_, collector))
    node2.foreach(convert(_, collector))
    withCollector(collector, f)
  }

  def withContract[T](node: Seq[ValContractClauseContext], f: ContractCollector[G] => T): T = {
    val collector = new ContractCollector[G]()
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

  def convert(contract: ValEmbedContractContext, collector: ContractCollector[G]): Unit = contract match {
    case ValEmbedContract0(blocks) => blocks.foreach(convert(_, collector))
  }

  def convert(contract: ValEmbedContractBlockContext, collector: ContractCollector[G]): Unit = contract match {
    case ValEmbedContractBlock0(_, clauses, _) => clauses.foreach(convert(_, collector))
    case ValEmbedContractBlock1(clauses) => clauses.foreach(convert(_, collector))
  }

  def convert(implicit contract: ValContractClauseContext, collector: ContractCollector[G]): Unit = contract match {
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
      collector.requires += ((contract, convert(exp)))
      collector.ensures += ((contract, convert(exp)))
    case ValContractClause8(_, exp, _) => collector.loop_invariant += ((contract, convert(exp)))
    case ValContractClause9(_, exp, _) => collector.kernel_invariant += ((contract, convert(exp)))
    case ValContractClause10(_, _, t, id, _, exp, _) =>
      val variable = new Variable(convert(t))(SourceNameOrigin(convert(id), origin(contract)))
      collector.signals += ((contract, SignalsClause(variable, convert(exp))(originProvider(contract))))
    case ValContractClause11(_, invariant, _) => collector.lock_invariant += ((contract, convert(invariant)))
    case ValContractClause12(_, None, _) => collector.decreases += ((contract, DecreasesClauseNoRecursion()))
    case ValContractClause12(_, Some(clause), _) => collector.decreases += ((contract, convert(clause)))
  }

  def convert(implicit clause: ValDecreasesMeasureContext): DecreasesClause[G] = clause match {
    case ValDecreasesMeasure0(_) => DecreasesClauseAssume()
    case ValDecreasesMeasure1(exps) => DecreasesClauseTuple(convert(exps))
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

  def convertEmbedWith(implicit whiff: Option[ValEmbedWithContext], inner: Expr[G]): Expr[G] = whiff match {
    case None => inner
    case Some(ValEmbedWith0(_, whiff, _)) => convertWith(whiff, inner)
    case Some(ValEmbedWith1(whiff)) => convertWith(Some(whiff), inner)
  }

  def convertWith(implicit whiff: Option[ValWithContext], inner: Expr[G]): Expr[G] = whiff match {
    case None => inner
    case Some(whiff @ ValWith0(_, stat)) => With(convert(stat), inner)(origin(whiff))
  }

  def convertEmbedThen(implicit den: Option[ValEmbedThenContext], inner: Expr[G]): Expr[G] = den match {
    case None => inner
    case Some(ValEmbedThen0(_, den, _)) => convertThen(den, inner)
    case Some(ValEmbedThen1(den)) => convertThen(Some(den), inner)
  }

  def convertThen(implicit den: Option[ValThenContext], inner: Expr[G]): Expr[G] = den match {
    case None => inner
    case Some(den @ ValThen0(_, stat)) => Then(inner, convert(stat))(origin(den))
  }

  def convert(implicit whiff: ValEmbedWithContext): Statement[G] = whiff match {
    case ValEmbedWith0(_, Some(whiff), _) => convert(whiff)
    case ValEmbedWith0(_, None, _) => Block(Nil)
    case ValEmbedWith1(whiff) => convert(whiff)
  }

  def convert(implicit whiff: ValWithContext): Statement[G] = whiff match {
    case ValWith0(_, stat) => convert(stat)
  }

  def convert(implicit whiff: ValEmbedThenContext): Statement[G] = whiff match {
    case ValEmbedThen0(_, Some(whiff), _) => convert(whiff)
    case ValEmbedThen0(_, None, _) => Block(Nil)
    case ValEmbedThen1(whiff) => convert(whiff)
  }

  def convert(implicit whiff: ValThenContext): Statement[G] = whiff match {
    case ValThen0(_, stat) => convert(stat)
  }

  def convertEmbedGiven(implicit given: Option[ValEmbedGivenContext]): Seq[(Ref[G, Variable[G]], Expr[G])] = given match {
    case None => Nil
    case Some(ValEmbedGiven0(_, inner, _)) => convertGiven(inner)
    case Some(ValEmbedGiven1(inner)) => convertGiven(Some(inner))
  }

  def convertGiven(implicit given: Option[ValGivenContext]): Seq[(Ref[G, Variable[G]], Expr[G])] = given match {
    case None => Nil
    case Some(ValGiven0(_, _, mappings, _)) => convert(mappings)
  }

  def convert(implicit mappings: ValGivenMappingsContext): Seq[(Ref[G, Variable[G]], Expr[G])] = mappings match {
    case ValGivenMappings0(arg, _, v) => Seq((new UnresolvedRef[G, Variable[G]](convert(arg)), convert(v)))
    case ValGivenMappings1(arg, _, v, _, more) => (new UnresolvedRef[G, Variable[G]](convert(arg)), convert(v)) +: convert(more)
  }

  def convertEmbedYields(implicit given: Option[ValEmbedYieldsContext]): Seq[(Ref[G, Variable[G]], Ref[G, Variable[G]])] = given match {
    case None => Nil
    case Some(ValEmbedYields0(_, inner, _)) => convertYields(inner)
    case Some(ValEmbedYields1(inner)) => convertYields(Some(inner))
  }

  def convertYields(implicit given: Option[ValYieldsContext]): Seq[(Ref[G, Variable[G]], Ref[G, Variable[G]])] = given match {
    case None => Nil
    case Some(ValYields0(_, _, mappings, _)) => convert(mappings)
  }

  def convert(implicit mappings: ValYieldsMappingsContext): Seq[(Ref[G, Variable[G]], Ref[G, Variable[G]])] = mappings match {
    case ValYieldsMappings0(target, _, res) => Seq((new UnresolvedRef[G, Variable[G]](convert(target)), new UnresolvedRef[G, Variable[G]](convert(res))))
    case ValYieldsMappings1(target, _, res, _, more) => (new UnresolvedRef[G, Variable[G]](convert(target)), new UnresolvedRef[G, Variable[G]](convert(res))) +: convert(more)
  }

  def convert(implicit exprs: ValExpressionListContext): Seq[Expr[G]] = exprs match {
    case ValExpressionList0(expr) => Seq(convert(expr))
    case ValExpressionList1(head, _, tail) => convert(head) +: convert(tail)
  }

  def convert(implicit ids: ValIdListContext): Seq[String] = ids match {
    case ValIdList0(id) => Seq(convert(id))
    case ValIdList1(id, _, ids) => convert(id) +: convert(ids)
  }

  def convert(implicit ts: ValTypeListContext): Seq[Type[G]] = ts match {
    case ValTypeList0(t) => Seq(convert(t))
    case ValTypeList1(t, _, ts) => convert(t) +: convert(ts)
  }

  def convert(implicit impOp: ValImpOpContext, left: Expr[G], right: Expr[G]): Expr[G] = impOp match {
    case ValImpOp0(_) => Wand(left, right)(origin(impOp))
    case ValImpOp1(_) => Implies(left, right)(origin(impOp))
  }

  def convert(implicit andOp: ValAndOpContext, left: Expr[G], right: Expr[G]): Expr[G] = andOp match {
    case ValAndOp0(_) => col.Star(left, right)(origin(andOp))
  }

  def convert(implicit inOp: ValInOpContext, left: Expr[G], right: Expr[G]): Expr[G] = inOp match {
    case ValInOp0(_) => AmbiguousMember(left, right)
  }

  def convert(implicit mulOp: ValMulOpContext, left: Expr[G], right: Expr[G]): Expr[G] = mulOp match {
    case ValMulOp0(_) => col.Div(left, right)(blame(mulOp))
  }

  def convert(implicit prependOp: ValPrependOpContext, left: Expr[G], right: Expr[G]): Expr[G] = prependOp match {
    case ValPrependOp0(_) => Cons(left, right)
  }

  def convert(implicit postfixOp: ValPostfixContext, xs: Expr[G]): Expr[G] = postfixOp match {
    case ValPostfix0(_, _, to, _) => Take(xs, convert(to))
    case ValPostfix1(_, from, _, None, _) => Drop(xs, convert(from))
    case ValPostfix1(_, from, _, Some(to), _) => Slice(xs, convert(from), convert(to))
    case ValPostfix2(_, idx, _, v, _) => SeqUpdate(xs, convert(idx), convert(v))
    case ValPostfix3(_, name, _, args, _) => CoalesceInstancePredicateApply(xs, new UnresolvedRef[G, InstancePredicate[G]](convert(name)), args.map(convert(_)).getOrElse(Nil), WritePerm())
  }

  def convert(implicit prefixOp: ValPrefixContext, xs: Expr[G]): Expr[G] = prefixOp match {
    case ValScale(_, scale, _) => Scale(convert(scale), xs)(blame(prefixOp))
  }

  def convert(implicit block: ValEmbedStatementBlockContext): Block[G] = block match {
    case ValEmbedStatementBlock0(_, stats, _) => Block(stats.map(convert(_)))
    case ValEmbedStatementBlock1(stats) => Block(stats.map(convert(_)))
  }

  def convert(implicit stat: ValStatementContext): Statement[G] = stat match {
    case ValPackage(_, expr, innerStat) => WandPackage(convert(expr), convert(innerStat))(blame(stat))
    case ValApplyWand(_, wand, _) => WandApply(convert(wand))(blame(stat))
    case ValFold(_, predicate, _) =>
      Fold(convert(predicate))(blame(stat))
    case ValUnfold(_, predicate, _) =>
      Unfold(convert(predicate))(blame(stat))
    case ValOpen(_, _, _) => ??(stat)
    case ValClose(_, _, _) => ??(stat)
    case ValAssert(_, assn, _) => Assert(convert(assn))(blame(stat))
    case ValAssume(_, assn, _) => Assume(convert(assn))
    case ValInhale(_, resource, _) => Inhale(convert(resource))
    case ValExhale(_, resource, _) => Exhale(convert(resource))(blame(stat))
    case ValLabel(_, label, _) =>
      Label(new LabelDecl()(SourceNameOrigin(convert(label), origin(stat))), Block(Nil))
    case ValRefute(_, assn, _) => Refute(convert(assn))(blame(stat))
    case ValWitness(_, _, _) => ??(stat)
    case ValGhost(_, stat) => convert(stat)
    case ValSend(_, name, _, delta, _, resource, _) =>
      Send(new SendDecl()(SourceNameOrigin(convert(name), origin(stat))), convert(delta), convert(resource))(blame(stat))
    case ValRecv(_, name, _) =>
      Recv(new UnresolvedRef[G, SendDecl[G]](convert(name)))
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
      ParAtomic(Seq(new UnresolvedRef[G, ParInvariantDecl[G]](convert(invariant))), convert(body))(blame(stat))
  }

  def convert(implicit block: ValBlockContext): Seq[Statement[G]] = block match {
    case ValBlock0(_, stats, _) => stats.map(convert(_))
  }

  def convert(implicit arg: ValArgContext): Variable[G] = arg match {
    case ValArg0(t, id) => new Variable(convert(t))(SourceNameOrigin(convert(id), origin(arg)))
  }

  def convert(implicit args: ValArgListContext): Seq[Variable[G]] = args match {
    case ValArgList0(arg) => Seq(convert(arg))
    case ValArgList1(arg, _, args) => convert(arg) +: convert(args)
  }

  def convert(implicit decl: ValEmbedGlobalDeclarationBlockContext): Seq[GlobalDeclaration[G]] = decl match {
    case ValEmbedGlobalDeclarationBlock0(_, globals, _) => globals.flatMap(convert(_))
    case ValEmbedGlobalDeclarationBlock1(globals) => globals.flatMap(convert(_))
  }

  def convert(implicit decl: ValGlobalDeclarationContext): Seq[GlobalDeclaration[G]] = decl match {
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
            c.consumeApplicableContract(blame(decl)),
            m.consume(m.inline))(blame(decl))(namedOrigin)
        })
      ))
    case ValModel(_, name, _, decls, _) =>
      Seq(new Model(decls.flatMap(convert(_)))(SourceNameOrigin(convert(name), origin(decl))))
    case ValGhostDecl(_, inner) =>
      convert(inner)
    case ValAdtDecl(_, name, typeArgs, _, decls, _) =>
      Seq(new AxiomaticDataType(decls.map(convert(_)), typeArgs.map(convert(_)).getOrElse(Nil))(
        SourceNameOrigin(convert(name), origin(decl))))
  }

  def convert(implicit decl: ValEmbedClassDeclarationBlockContext): Seq[ClassDeclaration[G]] = decl match {
    case ValEmbedClassDeclarationBlock0(_, decls, _) => decls.flatMap(convert(_, x => x))
    case ValEmbedClassDeclarationBlock1(decls) => decls.flatMap(convert(_, x => x))
  }

  def convert[T](implicit decl: ValClassDeclarationContext, transform: ClassDeclaration[G] => T): Seq[T] = decl match {
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
            c.consumeApplicableContract(blame(decl)), m.consume(m.inline))(
            blame(decl))(
            SourceNameOrigin(convert(name), origin(decl))))
        })
      }))
    case ValInstanceGhostDecl(_, decl) => convert(decl).map(transform)
  }

  def convert(implicit decl: ValModelDeclarationContext): Seq[ModelDeclaration[G]] = decl match {
    case ValModelField(t, name, _) =>
      convert(name).map(name => {
        new ModelField(convert(t))(SourceNameOrigin(name, origin(decl)))
      })
    case ValModelProcess(contract, _, name, _, args, _, _, definition, _) =>
      Seq(withContract(contract, c => {
        new ModelProcess(args.map(convert(_)).getOrElse(Nil), convert(definition),
          AstBuildHelpers.foldAnd(c.consume(c.requires)), AstBuildHelpers.foldAnd(c.consume(c.ensures)),
          c.consume(c.modifies).map(new UnresolvedRef[G, ModelField[G]](_)), c.consume(c.accessible).map(new UnresolvedRef[G, ModelField[G]](_)))(
          blame(decl))(SourceNameOrigin(convert(name), origin(decl)))
      }))
    case ValModelAction(contract, _, name, _, args, _, _) =>
      Seq(withContract(contract, c => {
        new ModelAction(args.map(convert(_)).getOrElse(Nil),
          AstBuildHelpers.foldAnd(c.consume(c.requires)), AstBuildHelpers.foldAnd(c.consume(c.ensures)),
          c.consume(c.modifies).map(new UnresolvedRef[G, ModelField[G]](_)), c.consume(c.accessible).map(new UnresolvedRef[G, ModelField[G]](_)))(
          SourceNameOrigin(convert(name), origin(decl)))
      }))
  }

  def convert(implicit ts: ValTypeVarsContext): Seq[Variable[G]] = ts match {
    case ValTypeVars0(_, names, _) =>
      convert(names).map(name => new Variable(TType(TAny()))(SourceNameOrigin(name, origin(ts))))
  }

  def convert(implicit decl: ValAdtDeclarationContext): ADTDeclaration[G] = decl match {
    case ValAdtAxiom(_, ax, _) => new ADTAxiom(convert(ax))
    case ValAdtFunction(_, returnType, name, _, args, _, _) =>
      new ADTFunction(args.map(convert(_)).getOrElse(Nil), convert(returnType))(
        SourceNameOrigin(convert(name), origin(decl)))
  }

  def convert(implicit definition: ValDefContext): Option[Expr[G]] = definition match {
    case ValAbstractBody(_) => None
    case ValBody(_, expr, _) => Some(convert(expr))
  }

  def convert(implicit t: ValTypeContext): Type[G] = t match {
    case ValPrimaryType(name) => name match {
      case "resource" => TResource()
      case "process" => TProcess()
      case "frac" => TFraction()
      case "zfrac" => TZFraction()
      case "rational" => TRational()
      case "bool" => TBool()
      case "ref" => TRef()
      case "any" => TAny()
      case "nothing" => TNothing()
    }
    case ValSeqType(_, _, element, _) => TSeq(convert(element))
    case ValSetType(_, _, element, _) => TSet(convert(element))
    case ValBagType(_, _, element, _) => TBag(convert(element))
    case ValOptionType(_, _, element, _) => TOption(convert(element))
    case ValMapType(_, _, key, _, value, _) => TMap(convert(key), convert(value))
    case ValTupleType(_, _, t1, _, t2, _) => TTuple(Seq(convert(t1), convert(t2)))
    case ValPointerType(_, _, element, _) => TPointer(convert(element))
    case ValTypeType(_, _, element, _) => TType(convert(element))
    case ValEitherType(_, _, left, _, right, _) => TEither(convert(left), convert(right))
  }

  def convert(implicit e: ValPrimarySeqContext): Expr[G] = e match {
    case ValCardinality(_, xs, _) => Size(convert(xs))
    case ValArrayValues(_, _, a, _, from, _, to, _) => Values(convert(a), convert(from), convert(to))(blame(e))
  }

  def convert(implicit e: ValPrimaryOptionContext): Expr[G] = e match {
    case ValSome(_, _, v, _) => OptSome(convert(v))
  }

  def convert(implicit e: ValPrimaryEitherContext): Expr[G] = e match {
    case ValLeft(_, _, inner, _) => EitherLeft(convert(inner))
    case ValRight(_, _, inner, _) => EitherRight(convert(inner))
  }

  // valsetcompselectors
  def convert(implicit exprs: ValMapPairsContext): Seq[(Expr[G], Expr[G])] = exprs match {
    case ValMapPairs0(k, _, v) => Seq((convert(k), convert(v)))
    case ValMapPairs1(k, _, v, _, tail) => (convert(k), convert(v)) +: convert(tail)
  }

  def convert(implicit e: ValPrimaryCollectionConstructorContext): Expr[G] = e match {
    case ValTypedLiteralSeq(_, _, t, _, _, exprs, _) => LiteralSeq(convert(t), exprs.map(convert(_)).getOrElse(Nil))
    case ValTypedLiteralSet(_, _, t, _, _, exprs, _) => LiteralSet(convert(t), exprs.map(convert(_)).getOrElse(Nil))
    case ValSetComprehension(_, _, t, _, _, value, _, selectors, _, something, _) => ??(e)
    case ValTypedLiteralBag(_, _, t, _, _, exprs, _) => LiteralBag(convert(t), exprs.map(convert(_)).getOrElse(Nil))
    case ValTypedLiteralMap(_, _, key, _, value, _, _, pairs, _) => LiteralMap(convert(key), convert(value), pairs.map(convert(_)).getOrElse(Nil))
    case ValTypedTuple(_, _, t1, _, t2, _, _, v1, _, v2, _) =>
      LiteralTuple(Seq(convert(t1), convert(t2)), Seq(convert(v1), convert(v2)))
    case ValLiteralSeq(_, exprs, _) => UntypedLiteralSeq(convert(exprs))
    case ValLiteralSet(_, exprs, _) => UntypedLiteralSet(convert(exprs))
    case ValLiteralBag(_, exprs, _) => UntypedLiteralBag(convert(exprs))
    case ValEmptySeq(_, t, _) => LiteralSeq(convert(t), Nil)
    case ValEmptySet(_, t, _) => LiteralSet(convert(t), Nil)
    case ValEmptyBag(_, t, _) => LiteralBag(convert(t), Nil)
    case ValRange(_, from, _, to, _) => Range(convert(from), convert(to))
  }

  def convert(implicit e: ValPrimaryPermissionContext): Expr[G] = e match {
    case ValCurPerm(_, _, loc, _) => CurPerm(AmbiguousLocation(convert(loc))(blame(e)))
    case ValPerm(_, _, loc, _, perm, _) => Perm(AmbiguousLocation(convert(loc))(blame(e)), convert(perm))
    case ValValue(_, _, loc, _) => Value(AmbiguousLocation(convert(loc))(blame(e)))
    case ValPointsTo(_, _, loc, _, perm, _, v, _) => PointsTo(AmbiguousLocation(convert(loc))(blame(e)), convert(perm), convert(v))
    case ValHPerm(_, _, loc, _, perm, _) => ModelPerm(convert(loc), convert(perm))
    case ValAPerm(_, _, loc, _, perm, _) => ActionPerm(convert(loc), convert(perm))
    case ValArrayPerm(_, _, arr, _, i, _, step, _, count, _, perm, _) => ??(e)
    case ValMatrix(_, _, m, _, dim1, _, dim2, _) => ValidMatrix(convert(m), convert(dim1), convert(dim2))
    case ValArray(_, _, arr, _, dim, _) => ValidArray(convert(arr), convert(dim))
    case ValPointer(_, _, ptr, _, n, _, perm, _) => PermPointer(convert(ptr), convert(n), convert(perm))
    case ValPointerIndex(_, _, ptr, _, idx, _, perm, _) => PermPointerIndex(convert(ptr), convert(idx), convert(perm))
    case ValPointerBlockLength(_, _, ptr, _) => PointerBlockLength(convert(ptr))(blame(e))
    case ValPointerBlockOffset(_, _, ptr, _) => PointerBlockOffset(convert(ptr))(blame(e))
  }

  def convert(implicit e: ValPrimaryBinderContext): Expr[G] = e match {
    case ValRangeQuantifier(_, quant, t, id, _, from, _, to, _, body, _) =>
      val variable = new Variable[G](convert(t))(SourceNameOrigin(convert(id), origin(id)))
      val cond = SeqMember[G](Local(variable.ref), Range(convert(from), convert(to)))
      quant match {
        case "\\forall*" => Starall(Seq(variable), Nil, Implies(cond, convert(body)))(blame(e))
        case "\\forall" => Forall(Seq(variable), Nil, Implies(cond, convert(body)))
        case "\\exists" => Exists(Seq(variable), Nil, col.And(cond, convert(body)))
      }
    case ValQuantifier(_, quant, bindings, _, cond, _, body, _) =>
      val variables = convert(bindings)
      quant match {
        case "\\forall*" => Starall(variables, Nil, Implies(convert(cond), convert(body)))(blame(e))
        case "\\forall" => Forall(variables, Nil, Implies(convert(cond), convert(body)))
        case "\\exists" => Exists(variables, Nil, col.And(convert(cond), convert(body)))
      }
    case ValShortQuantifier(_, quant, bindings, _, body, _) =>
      val variables = convert(bindings)
      quant match {
        case "∀" => Forall(variables, Nil, convert(body))
        case "∀*" => Starall(variables, Nil, convert(body))(blame(e))
        case "∃" => Exists(variables, Nil, convert(body))
      }
    case ValLet(_, _, t, id, _, v, _, body, _) =>
      Let(new Variable(convert(t))(SourceNameOrigin(convert(id), origin(id))), convert(v), convert(body))
  }

  def convert(implicit e: ValPrimaryVectorContext): Expr[G] = e match {
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

  def convert(implicit e: ValPrimaryReducibleContext): Expr[G] = ??(e)

  def convert(implicit e: ValPrimaryThreadContext): Expr[G] = e match {
    case ValIdle(_, _, thread, _) => IdleToken(convert(thread))
    case ValRunning(_, _, thread, _) => JoinToken(convert(thread))
  }

  def convert(implicit e: ValPrimaryContextContext): Expr[G] = e match {
    case ValPrimaryContext0("\\result") => AmbiguousResult()
    case ValPrimaryContext1("\\current_thread") => CurrentThreadId()
    case ValPrimaryContext2("\\ltid") => LocalThreadId()
    case ValPrimaryContext3("\\gtid") => GlobalThreadId()
  }

  def convert(implicit e: ValPrimaryContext): Expr[G] = e match {
    case ValPrimary0(inner) => convert(inner)
    case ValPrimary1(inner) => convert(inner)
    case ValPrimary2(inner) => convert(inner)
    case ValPrimary3(inner) => convert(inner)
    case ValPrimary4(inner) => convert(inner)
    case ValPrimary5(inner) => convert(inner)
    case ValPrimary6(inner) => convert(inner)
    case ValPrimary7(inner) => convert(inner)
    case ValPrimary8(inner) => convert(inner)
    case ValPrimary9(inner) => convert(inner)
    case ValAny(_) => Any()(blame(e))
    case ValFunctionOf(_, inner, _, names, _) => FunctionOf(new UnresolvedRef[G, Variable[G]](convert(inner)), convert(names).map(new UnresolvedRef[G, Variable[G]](_)))
    case ValInlinePattern(open, pattern, _) =>
      val groupText = open.filter(_.isDigit)
      InlinePattern(convert(pattern), open.count(_ == '<'), if (groupText.isEmpty) 0 else groupText.toInt)
    case ValUnfolding(_, predExpr, _, body) => Unfolding(convert(predExpr), convert(body))(blame(e))
    case ValOld(_, _, expr, _) => Old(convert(expr), at = None)(blame(e))
    case ValOldLabeled(_, _, label, _, _, expr, _) => Old(convert(expr), at = Some(new UnresolvedRef[G, LabelDecl[G]](convert(label))))(blame(e))
    case ValTypeof(_, _, expr, _) => TypeOf(convert(expr))
    case ValTypeValue(_, _, t, _) => TypeValue(convert(t))
    case ValHeld(_, _, obj, _) => Held(convert(obj))
    case ValIdEscape(text) => local(e, text.substring(1, text.length-1))
  }

  def convert(implicit e: ValExprContext): Expr[G] = e match {
    case ValExpr0(inner) => convert(inner)
    case ValExpr1(inner) => convert(inner)
  }

  def convert(implicit id: ValIdentifierContext): String = id match {
    case ValIdentifier0(inner) => convertText(inner)
    case ValIdentifier1(ValKeywordNonExpr0(text)) => text
    case ValIdentifier2(text) => text.substring(1, text.length-1)
  }

  def convertText(implicit res: ValKeywordExprContext): String = res match {
    case ValNonePerm(_) => "none"
    case ValWrite(_) => "write"
    case ValRead(_) => "read"
    case ValNoneOption(_) => "None"
    case ValEmpty(_) => "empty"
    case ValTrue(_) => "true"
    case ValFalse(_) => "false"
  }

  def convert(implicit res: ValKeywordExprContext): Expr[G] = res match {
    case ValNonePerm(_) => NoPerm()
    case ValWrite(_) => WritePerm()
    case ValRead(_) => ReadPerm()
    case ValNoneOption(_) => OptNone()
    case ValEmpty(_) => EmptyProcess()
    case ValTrue(_) => tt
    case ValFalse(_) => ff
  }

  def convert(implicit inv: ValGenericAdtInvocationContext): Expr[G] = inv match {
    case ValGenericAdtInvocation0(adt, _, typeArgs, _, _, func, _, args, _) =>
      ADTFunctionInvocation(Some((new UnresolvedRef[G, AxiomaticDataType[G]](convert(adt)), convert(typeArgs))),
        new UnresolvedRef[G, ADTFunction[G]](convert(func)), args.map(convert(_)).getOrElse(Nil))
  }
}
