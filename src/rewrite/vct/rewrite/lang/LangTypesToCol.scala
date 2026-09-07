package vct.rewrite.lang

import com.typesafe.scalalogging.LazyLogging
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.ref.{Ref, UnresolvedRef}
import vct.col.resolve.ctx._
import vct.col.resolve.lang.{C, CPP}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilderArg, Rewritten}
import vct.col.typerules.PlatformContext
import vct.col.util.SuccessionMap
import vct.result.VerificationError.UserError

import scala.collection.mutable
import scala.reflect.ClassTag

case object LangTypesToCol extends RewriterBuilderArg[PlatformContext] {
  override def key: String = "langTypes"
  override def desc: String =
    "Translate language-specific types (such as named types) to specific internal types."

  case class IncompleteTypeArgs(t: SilverPartialTAxiomatic[_])
      extends UserError {
    override def code: String = "incompleteTypeArgs"
    override def text: String =
      t.o.messageInContext(
        "This type does not specify all generic types for the domain."
      )
  }

  case class EmptyInlineDecl(d: CLocalDeclaration[_]) extends UserError {
    override def code: String = "emptyInlineDecl"

    override def text: String =
      d.o.messageInContext(" ‘inline’ in empty declaration.")
  }

  case class ContractOnMultiInitialiser(d: CDeclaration[_]) extends UserError {
    override def code: String = "contractMultipleInit"
    override def text: String =
      d.o.messageInContext(
        "A contract cannot be placed on a declaration with more than one initialiser"
      )
  }

  case class ContractOnVariable(n: Node[_]) extends UserError {
    override def code: String = "contractOnVariable"
    override def text: String =
      n.o.messageInContext(
        "A contract cannot be placed on a variable declaration"
      )
  }
}

case class LangTypesToCol[Pre <: Generation](platformContext: PlatformContext)
    extends Rewriter[Pre] with LazyLogging {
  import LangTypesToCol._

  val structDeclMap
      : SuccessionMap[LLVMStructDeclaration[Pre], LLVMStructDeclaration[Post]] =
    SuccessionMap()
  val cStructFieldsSuccessor: SuccessionMap[(CStructMemberDeclarator[
    Pre
  ]), CStructMemberDeclarator[Post]] = SuccessionMap()

  override def porcelainRefSucc[RefDecl <: Declaration[Rewritten[Pre]]](
      ref: Ref[Pre, _]
  )(implicit tag: ClassTag[RefDecl]): Option[Ref[Rewritten[Pre], RefDecl]] =
    ref match {
      // Retain unresolved references to be resolved by LangSpecificToCol
      case unresolved: UnresolvedRef[_, _] if !unresolved.isResolved =>
        Some(new UnresolvedRef[Post, RefDecl](unresolved.name))
      case _ => None
    }

  override def porcelainRefSeqSucc[RefDecl <: Declaration[Rewritten[Pre]]](
      refs: Seq[Ref[Pre, _]]
  )(
      implicit tag: ClassTag[RefDecl]
  ): Option[Seq[Ref[Rewritten[Pre], RefDecl]]] =
    if (refs.forall(_.isInstanceOf[UnresolvedRef[_, _]]))
      Some(refs.map(porcelainRefSucc[RefDecl]).map(_.get))
    else
      None

  def specType(
      target: SpecTypeNameTarget[Pre],
      args: Seq[Type[Pre]],
  ): Type[Post] =
    target match {
      case RefAxiomaticDataType(decl) =>
        TAxiomatic[Post](succ(decl), args.map(dispatch))
      case RefModel(decl) => TModel[Post](succ(decl))
      case RefEnum(enum) => TEnum[Post](succ(enum))
      case RefProverType(typ) => TProverType[Post](succ(typ))
      case RefVariable(decl) => TVar[Post](succ(decl))
    }

  private def assumedEquivalent(
      d1: LLVMStructDeclaration[Pre],
      d2: LLVMStructDeclaration[Pre],
      assumptions: Seq[Set[LLVMStructDeclaration[Pre]]],
  ): Boolean = {
    assumptions.filter(s => s.contains(d1) && s.contains(d2)).size > 0
  }

  // Add Set {d1, d2} to the equivalences and merges sets that contain d1 or d2
  private def addAssumedEquivalence(
      d1: LLVMStructDeclaration[Pre],
      d2: LLVMStructDeclaration[Pre],
      assumptions: Seq[Set[LLVMStructDeclaration[Pre]]],
  ): Seq[Set[LLVMStructDeclaration[Pre]]] = {
    val containsD1 = assumptions.find(s => s.contains(d1)).getOrElse(Set())
    val containsD2 = assumptions.find(s => s.contains(d2)).getOrElse(Set())
    val containNeither = assumptions
      .filter(s => (!s.contains(d1)) && (!s.contains(d2)))
    containNeither :+ (containsD1 union containsD2 union Set(d1, d2))
  }

  private def structEq(
      s: LLVMStructDeclaration[Pre],
      o: LLVMStructDeclaration[Pre],
      assumptions: Seq[Set[LLVMStructDeclaration[Pre]]] = Seq(),
  ): Boolean = {
    // TODO: Might have to get rid of packed since we don't have that in the DIType
    s.isLiteral ==
      o.isLiteral && /*(s.name.isEmpty || s.name.intersect(o.name).nonEmpty) &&*/
      s.elements.size == o.elements.size && s.sizeInBits == o.sizeInBits &&
      s.elements.zip(o.elements).forall { case (a, b) =>
        a.offset == b.offset && a.size == b.size &&
        (a.t == b.t ||
          ((a.t, b.t) match {
            case (LLVMTPointer(None), LLVMTPointer(Some(_))) |
                (LLVMTPointer(Some(_)), LLVMTPointer(None)) =>
              true
            case (
                  LLVMTPointer(Some(LLVMTStruct(Ref(sa)))),
                  LLVMTPointer(Some(LLVMTStruct(Ref(sb)))),
                ) =>
              assumedEquivalent(sa, sb, assumptions) ||
              structEq(sa, sb, addAssumedEquivalence(sa, sb, assumptions))
            case (LLVMTStruct(Ref(sa)), LLVMTStruct(Ref(sb))) =>
              assumedEquivalent(sa, sb, assumptions) ||
              structEq(sa, sb, addAssumedEquivalence(sa, sb, assumptions))
            case (LLVMTInt(_), TBool()) | (TBool(), LLVMTInt(_)) => true
            case _ => false
          }))
      }
  }

  // This requires, structEq(s, 0) == true !!!
  private def structUnion(
      s: LLVMStructDeclaration[Pre],
      o: LLVMStructDeclaration[Pre],
  ): LLVMStructDeclaration[Pre] = {
    // TODO: Merge origins
    new LLVMStructDeclaration(
      s.name.toSet.union(o.name.toSet).toSeq,
      s.packed,
      s.isLiteral,
      s.elements.zip(o.elements).map {
        case (
              // This only works if ´l´ & ´r´ point to the same declaration after rewriting!
              lf @ LLVMFieldDefinition(offset, size, l: LLVMTStruct[Pre]),
              rf @ LLVMFieldDefinition(_, _, r: LLVMTStruct[Pre]),
            ) =>
          LLVMFieldDefinition(offset, size, l)(
            if (
              rf.o.getPreferredNameOrElse(Seq("t_struct")).snake == "t_struct"
            ) { lf.o }
            else { rf.o }
          )
        case (
              LLVMFieldDefinition(offset, size, LLVMTInt(_)),
              rf @ LLVMFieldDefinition(_, _, b @ TBool()),
            ) =>
          LLVMFieldDefinition(offset, size, b)(rf.o)
        case (
              lf @ LLVMFieldDefinition(offset, size, b @ TBool()),
              LLVMFieldDefinition(_, _, LLVMTInt(_)),
            ) =>
          LLVMFieldDefinition(offset, size, b)(lf.o)
        case (l, r) =>
          LLVMFieldDefinition(l.offset, l.size, l.t)(
            if (
              r.o.getPreferredNameOrElse(Seq("t_unknown")).snake
                .startsWith("t_")
            ) { l.o }
            else { r.o }
          )
      },
      s.sizeInBits,
    )(if (o.o.getPreferredNameOrElse(Seq("t_struct")).snake == "t_struct") {
      s.o
    } else { o.o })
  }

  override def dispatch(program: Program[Pre]): Program[Post] = {
    val queue = mutable.ArrayDeque[LLVMStructDeclaration[Pre]]()
    val map: mutable.LinkedHashMap[LLVMStructDeclaration[
      Pre
    ], LLVMStructDeclaration[Pre]] = mutable.LinkedHashMap()

    program.foreach {
      case s: LLVMStructDeclaration[Pre] =>
        map(s) = s
        queue += s
      case _ =>
    }

    while (queue.nonEmpty) {
      val s = queue.removeHead()
      var toBeMerged =
        map.filter { case (_, v) => structEq(s, v) }.flatMap { case (k, v) =>
          Seq(k, v)
        }.toSet
      if (toBeMerged.nonEmpty) {
        toBeMerged = toBeMerged + s
        val newType = toBeMerged.reduce(structUnion)
        toBeMerged.foreach { t => map(t) = newType }
        map(newType) = newType
        queue.removeAll(toBeMerged.contains)
      }
    }

    lazy val newDecls =
      globalDeclarations.collect {
        map.foreach { case (k, v) =>
          logger.debug(f"`$k`: `$v`")
          structDeclMap(k) = structDeclMap
            .getOrElseUpdate(v, globalDeclarations.declare(v.rewriteDefault()))
        }
      }._1

    program.rewrite(declarations =
      newDecls ++ globalDeclarations.dispatch(program.declarations)
    )
  }

  override def dispatch(t: Type[Pre]): Type[Post] = {
    implicit val o: Origin = t.o
    t match {
      case t @ JavaNamedType(_) =>
        t.ref.get match {
          case spec: SpecTypeNameTarget[Pre] => specType(spec, Nil)
          case RefJavaClass(decl) =>
            assert(
              t.names.init.map(_._2)
                .forall((x: Option[Seq[Type[Pre]]]) => x.isEmpty)
            )
            val x = JavaTClass[Post](
              succ(decl),
              t.names.last._2.getOrElse(Nil).map(dispatch),
            )
            x
        }
      case t @ PVLNamedType(_, typeArgs) =>
        t.ref.get match {
          case spec: SpecTypeNameTarget[Pre] => specType(spec, typeArgs)
          case RefClass(decl: Class[Pre]) => dispatch(decl.classType(typeArgs))
        }
      case t @ CPrimitiveType(specs) =>
        dispatch(
          C.getPrimitiveType(specs, Some(platformContext), context = Some(t))
        )
      case t @ CPPPrimitiveType(specs) =>
        dispatch(CPP.getBaseTypeFromSpecs(specs, context = Some(t)))
      case t @ CTStructUnique(inner, pointerFieldRef, unique) =>
        val fieldSucc: Ref[Post, CStructMemberDeclarator[Post]] =
          cStructFieldsSuccessor(pointerFieldRef.decl).ref
        t.rewrite(pointerFieldRef = fieldSucc)
      case t @ SilverPartialTAxiomatic(Ref(adt), partialTypeArgs) =>
        if (partialTypeArgs.map(_._1.decl).toSet != adt.typeArgs.toSet)
          throw IncompleteTypeArgs(t)
        TAxiomatic(
          succ(adt),
          adt.typeArgs.map(arg =>
            dispatch(t.partialTypeArgs.find(_._1.decl == arg).get._2)
          ),
        )
      case p: TPointer[Pre] =>
        val pointer = super.dispatch(p)
        pointer.storedBits = platformContext.pointerSize
        pointer
      case p: TNonNullPointer[Pre] =>
        val pointer = super.dispatch(p)
        pointer.storedBits = platformContext.pointerSize
        pointer
      case p: CTPointer[Pre] =>
        val pointer = super.dispatch(p)
        pointer.storedBits = platformContext.pointerSize
        pointer
      case t @ TCInt() =>
        val cint = TCInt[Post]()
        cint.storedBits = t.storedBits
        cint.signed = t.signed
        cint.rank = t.rank
        cint
      case other =>
        val newOther = super.dispatch(other)
        newOther.storedBits = other.storedBits
        newOther
    }
  }

  def normalizeCDeclaration(
      specifiers: Seq[CDeclarationSpecifier[Pre]],
      declarator: CDeclarator[Pre],
      context: Option[Node[Pre]] = None,
      hasNonTrivialContract: Boolean = false,
  )(
      implicit o: Origin
  ): (Seq[CDeclarationSpecifier[Post]], CDeclarator[Post]) = {
    val info = C.getDeclaratorInfo(declarator)
    val (specs, otherSpecifiers) = specifiers.partition({
      case _: CTypeSpecifier[Pre] => true;
      case _: CTypeQualifierDeclarationSpecifier[Pre] => true;
      case _ => false
    })
    val newOtherSpecifiers = otherSpecifiers.map(dispatch)
    val baseType = C.getPrimitiveType(specs, Some(platformContext), context)
    val newSpecifiers: Seq[CDeclarationSpecifier[LangTypesToCol.this.Post]] =
      CSpecificationType[Post](dispatch(info.typeOrReturnType(baseType))) +:
        newOtherSpecifiers
    val newDeclarator =
      info.params match {
        case Some(params) =>
          // PB TODO: varargs is discarded here.
          CTypedFunctionDeclarator[Post](
            cParams.dispatch(params),
            varargs = false,
            CName(info.name),
          )
        case None if hasNonTrivialContract =>
          throw ContractOnVariable(context.getOrElse(declarator))
        case None => CName[Post](info.name)
      }

    (newSpecifiers, newDeclarator)
  }

  def normalizeCPPDeclaration(
      specifiers: Seq[CPPDeclarationSpecifier[Pre]],
      declarator: CPPDeclarator[Pre],
      context: Option[Node[Pre]] = None,
  )(
      implicit o: Origin
  ): (Seq[CPPDeclarationSpecifier[Post]], CPPDeclarator[Post]) = {
    val info = CPP.getDeclaratorInfo(
      declarator,
      context.getOrElse(false).isInstanceOf[CPPParam[Pre]],
    )
    val baseType = CPP.getBaseTypeFromSpecs(specifiers, context)
    if (info.isReference && !baseType.isInstanceOf[SYCLTHandler[Pre]]) {
      // Only accept reference parameters for type sycl::handler, as we only need & support for a lambda method with that parameter
      throw CPP.CPPTypeNotSupported(Some(declarator))
    }
    val otherSpecifiers = specifiers
      .filter(!_.isInstanceOf[CPPTypeSpecifier[Pre]]).map(dispatch)
    val newSpecifiers =
      CPPSpecificationType[Post](dispatch(info.typeOrReturnType(baseType))) +:
        otherSpecifiers
    val newDeclarator =
      info.params match {
        case Some(params) =>
          // PB TODO: varargs is discarded here.
          CPPTypedFunctionDeclarator[Post](
            cPPParams.dispatch(params),
            varargs = false,
            CPPName(info.name),
          )
        case None => CPPName[Post](info.name)
      }

    (newSpecifiers, newDeclarator)
  }

  override def dispatch(decl: Declaration[Pre]): Unit =
    decl match {
      case param: CParam[Pre] =>
        val (specs, decl) =
          normalizeCDeclaration(
            param.specifiers,
            param.declarator,
            context = Some(param),
          )(param.o)
        cParams.declare(new CParam(specs, decl)(param.o))
      case declaration: CLocalDeclaration[Pre] =>
        declaration.decl.inits.foreach(init => {
          implicit val o: Origin = init.o
          val (specs, decl) = normalizeCDeclaration(
            declaration.decl.specs,
            init.decl,
            context = Some(declaration),
          )
          cLocalDeclarations.declare(declaration.rewrite(decl =
            declaration.decl.rewrite(
              specs = specs,
              inits = Seq(CInit(decl, init.init.map(dispatch))),
            )
          ))
        })
      case declaration: CGlobalDeclaration[Pre] =>
        declaration.decl match {
          case CDeclaration(_, Seq(_: CStructDeclaration[Pre]), Seq()) =>
            globalDeclarations
              .succeed(declaration, declaration.rewriteDefault())
          case decl @ CDeclaration(
                _,
                Seq(td: CTypedef[Pre], struct: CStructDeclaration[Pre]),
                Seq(init),
              ) =>
            val structDecl =
              new CGlobalDeclaration[Post](
                CDeclaration[Post](
                  dispatch(decl.contract),
                  Seq(dispatch(struct)),
                  Seq(),
                )(decl.o)
              )(decl.o)
            val structSpec = CStructSpecifier[Post](struct.name.get)(decl.o)
            structSpec.ref = Some(RefCStruct(structDecl))

            globalDeclarations.succeed(declaration, structDecl)
          case decl =>
            val hasNonTrivialContract = decl.contract.nonEmpty
            if (hasNonTrivialContract && decl.inits.length > 1)
              throw ContractOnMultiInitialiser(decl)
            decl.inits.foreach(init => {
              implicit val o: Origin = init.o
              val (specs, decl1) = normalizeCDeclaration(
                decl.specs,
                init.decl,
                context = Some(declaration),
                hasNonTrivialContract,
              )
              globalDeclarations.declare(declaration.rewrite(decl =
                declaration.decl.rewrite(
                  specs = specs,
                  inits = Seq(CInit(decl1, init.init.map(dispatch))),
                )
              ))
            })
        }
      case declaration: CStructMemberDeclarator[Pre] =>
        declaration.decls.foreach(decl => {
          implicit val o: Origin = decl.o
          val (specs, newDecl) = normalizeCDeclaration(
            declaration.specs,
            decl,
            context = Some(declaration),
          )
          val newMember = declaration
            .rewrite(specs = specs, decls = Seq(newDecl))
          cStructFieldsSuccessor(declaration) = newMember
          cStructMemberDeclarators.declare(newMember)
        })
      case declaration: CFunctionDefinition[Pre] =>
        implicit val o: Origin = declaration.o
        val (specs, decl) = normalizeCDeclaration(
          declaration.specs,
          declaration.declarator,
          context = Some(declaration),
        )
        globalDeclarations
          .declare(declaration.rewrite(specs = specs, declarator = decl))
      case param: CPPParam[Pre] =>
        val (specs, decl) =
          normalizeCPPDeclaration(
            param.specifiers,
            param.declarator,
            context = Some(param),
          )(param.o)
        cPPParams.declare(new CPPParam(specs, decl)(param.o))
      case declaration: CPPLocalDeclaration[Pre] =>
        declaration.decl.inits.foreach(init => {
          implicit val o: Origin = init.o
          val (specs, decl) = normalizeCPPDeclaration(
            declaration.decl.specs,
            init.decl,
            context = Some(declaration),
          )
          cPPLocalDeclarations.declare(declaration.rewrite(decl =
            declaration.decl.rewrite(
              specs = specs,
              inits = Seq(CPPInit(decl, init.init.map(dispatch))),
            )
          ))
        })
      case declaration: CPPGlobalDeclaration[Pre] =>
        declaration.decl.inits.foreach(init => {
          implicit val o: Origin = init.o
          val (specs, decl) = normalizeCPPDeclaration(
            declaration.decl.specs,
            init.decl,
            context = Some(declaration),
          )
          globalDeclarations.declare(declaration.rewrite(decl =
            declaration.decl.rewrite(
              specs = specs,
              inits = Seq(CPPInit(decl, init.init.map(dispatch))),
            )
          ))
        })
      case declaration: CPPFunctionDefinition[Pre] =>
        implicit val o: Origin = declaration.o
        val (specs, decl) = normalizeCPPDeclaration(
          declaration.specs,
          declaration.declarator,
          context = Some(declaration),
        )
        globalDeclarations
          .declare(declaration.rewrite(specs = specs, declarator = decl))
      case cls: JavaClass[Pre] => super.dispatch(cls)
      case sDecl: LLVMStructDeclaration[Pre] =>
        globalDeclarations.succeedOnly(sDecl, structDeclMap(sDecl))
      case other => super.dispatch(other)
    }

  override def dispatch(stat: Statement[Pre]): Statement[Post] =
    stat match {
      case CDeclarationStatement(local) =>
        val (locals, _) = cLocalDeclarations.collect { dispatch(local) }
        if (
          locals.isEmpty &&
          local.decl.specs.collectFirst { case CInline() => }.nonEmpty
        )
          throw EmptyInlineDecl(local)
        Block(locals.map(CDeclarationStatement(_)(stat.o)))(stat.o)
      case CPPDeclarationStatement(local) =>
        val (locals, _) = cPPLocalDeclarations.collect { dispatch(local) }
        Block(locals.map(CPPDeclarationStatement(_)(stat.o)))(stat.o)

      case other => super.dispatch(other)
    }
}
