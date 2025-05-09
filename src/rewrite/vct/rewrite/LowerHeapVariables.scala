package vct.rewrite

import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.ast.{
  Assign,
  Block,
  DecreasesClauseNoRecursion,
  Deref,
  DerefHeapVariable,
  DerefPointer,
  Expr,
  FieldLocation,
  HeapLocal,
  HeapLocalDecl,
  HeapVariable,
  HeapVariableLocation,
  InstanceField,
  Local,
  LocalHeapVariable,
  Location,
  Perm,
  PointerLocation,
  Procedure,
  Program,
  Result,
  Scope,
  Statement,
  TByValueClass,
  TNonNullPointer,
  UnitAccountedPredicate,
  Variable,
  WritePerm,
}
import vct.col.origin.{
  AbstractApplicable,
  AssignLocalOk,
  NonNullPointerNull,
  Origin,
  PanicBlame,
  TrueSatisfiable,
}
import vct.col.util.AstBuildHelpers._
import vct.col.ref.Ref
import vct.col.util.{CurrentRewriteProgramContext, SuccessionMap}
import vct.result.VerificationError
import vct.rewrite.EncodeByValueClassUsage.UnsupportedStructPerm

case object LowerHeapVariables extends RewriterBuilder {
  override def key: String = "lowerHeapVariables"

  override def desc: String =
    "Lower pointer HeapVariables to plain HeapVariables and LocalHeapVariables to Variables if their address is never taken"
}

case class LowerHeapVariables[Pre <: Generation]() extends Rewriter[Pre] {
  private val localStripped
      : SuccessionMap[LocalHeapVariable[Pre], Variable[Post]] = SuccessionMap()
  private val localLowered
      : SuccessionMap[LocalHeapVariable[Pre], Variable[Post]] = SuccessionMap()
  private val globalStripped
      : SuccessionMap[HeapVariable[Pre], HeapVariable[Post]] = SuccessionMap()
  private val globalLowered
      : SuccessionMap[HeapVariable[Pre], HeapVariable[Post]] = SuccessionMap()
  private val classCreationMethodsSucc
      : SuccessionMap[TByValueClass[Pre], Procedure[Post]] = SuccessionMap()
  private val classPointerCreationMethodsSucc
      : SuccessionMap[(TByValueClass[Pre], Option[BigInt]), Procedure[Post]] =
    SuccessionMap()

  // Duplicate from EncodeByValueClassUsage, keep in sync
  def makeClassCreationMethod(t: TByValueClass[Pre]): Procedure[Post] = {
    implicit val o: Origin = t.cls.decl.o

    globalDeclarations.declare(withResult((result: Result[Post]) =>
      procedure[Post](
        blame = AbstractApplicable,
        contractBlame = TrueSatisfiable,
        returnType = dispatch(t),
        ensures = UnitAccountedPredicate(
          unwrapClassPerm(result, Perm(_, WritePerm()), t)
        ),
        decreases = Some(DecreasesClauseNoRecursion[Post]()),
      )
    ))
  }

  def makeClassPointerCreationMethod(
      t: TByValueClass[Pre],
      unique: Option[BigInt],
  ): Procedure[Post] = {
    implicit val o: Origin = t.cls.decl.o

    globalDeclarations.declare(withResult((result: Result[Post]) =>
      procedure[Post](
        blame = AbstractApplicable,
        contractBlame = TrueSatisfiable,
        returnType = TNonNullPointer(dispatch(t), unique),
        ensures = UnitAccountedPredicate(unwrapClassPerm(
          DerefPointer(result)(NonNullPointerNull),
          Perm(_, WritePerm()),
          t,
        )),
        decreases = Some(DecreasesClauseNoRecursion[Post]()),
      )
    ))
  }

  private def unwrapClassPerm(
      obj: Expr[Post],
      perm: Location[Post] => Expr[Post],
      structType: TByValueClass[Pre],
      visited: Seq[TByValueClass[Pre]] = Seq(),
  ): Expr[Post] = {
    if (visited.contains(structType))
      throw UnsupportedStructPerm(
        obj.o
      ) // We do not allow this notation for recursive structs
    implicit val o: Origin = obj.o
    val blame = PanicBlame("Field permission is framed")
    val fields = structType.cls.decl.decls.collect {
      case f: InstanceField[Pre] => f
    }
    val newFieldPerms = fields.map(member => {
      val loc = FieldLocation[Post](obj, succ(member))
      member.t match {
        case inner: TByValueClass[Pre] =>
          unwrapClassPerm(
            Deref[Post](obj, succ(member))(blame),
            perm,
            inner,
            structType +: visited,
          )
        case _ => perm(loc)
      }
    })

    foldStar(newFieldPerms)
  }

  override def dispatch(program: Program[Pre]): Program[Post] = {
    val dereferencedHeapLocals = program.collect {
      case DerefPointer(hl @ HeapLocal(_)) => System.identityHashCode(hl)
    }
    val nakedHeapLocals = program.collect {
      case hl @ HeapLocal(Ref(v))
          if !dereferencedHeapLocals.contains(System.identityHashCode(hl)) =>
        v
    }
    val dereferencedHeapGlobals = program.collect {
      case PointerLocation(hl @ DerefHeapVariable(_)) =>
        System.identityHashCode(hl)
      case DerefPointer(hl @ DerefHeapVariable(_)) =>
        System.identityHashCode(hl)
    }
    val nakedHeapGlobals = program.collect {
      case hl @ DerefHeapVariable(Ref(v))
          // We check for TNonNullPointer here to distinguish between PVL/Java and C/C++/LLVM (where the latter group should encode all global heap variables as TNonNullPointer
          if !dereferencedHeapGlobals.contains(System.identityHashCode(hl)) &&
            v.t.isInstanceOf[TNonNullPointer[Pre]] =>
        v
    }
    VerificationError.withContext(CurrentRewriteProgramContext(program)) {
      program.rewrite(declarations = {
        program.collect {
          case HeapLocal(Ref(v)) if !nakedHeapLocals.contains(v) => v
        }.foreach(v =>
          localStripped(v) =
            new Variable[Post](dispatch(v.t.asPointer.get.element))(v.o)
        )
        program.collect {
          case DerefHeapVariable(Ref(v))
              if !nakedHeapGlobals.contains(v) &&
                v.t.isInstanceOf[TNonNullPointer[Pre]] =>
            v
        }.foreach(v =>
          globalStripped(v) =
            new HeapVariable[Post](
              dispatch(v.t.asPointer.get.element),
              v.init.map(dispatch),
            )(v.o)
        )
        globalDeclarations.collect {
          program.declarations.foreach {
            case v: HeapVariable[Pre] if globalStripped.contains(v) =>
              globalDeclarations.succeed(v, globalStripped(v))
            case v: HeapVariable[Pre] =>
              globalLowered(v) =
                new HeapVariable[Post](dispatch(v.t), v.init.map(dispatch))(v.o)
              globalDeclarations.succeed(v, globalLowered(v))
            case decl => dispatch(decl)
          }

        }._1
      })
    }
  }

  override def dispatch(node: Statement[Pre]): Statement[Post] = {
    implicit val o: Origin = node.o
    node match {
      // Same logic as CollectLocalDeclarations
      case Scope(vars, impl) =>
        val (newVars, newImpl) = variables.collect {
          vars.foreach(dispatch)
          dispatch(impl)
        }
        Scope(newVars, newImpl)
      case HeapLocalDecl(v) =>
        if (localStripped.contains(v)) { variables.declare(localStripped(v)) }
        else {
          localLowered(v) = new Variable[Post](dispatch(v.t))(v.o)
          variables.declare(localLowered(v))
        }
        if (v.t.asPointer.get.element.asByValueClass.isDefined) {
          val t = v.t.asPointer.get.element.asByValueClass.get
          if (localStripped.contains(v)) {
            Assign(
              localStripped(v).get,
              procedureInvocation[Post](
                TrueSatisfiable,
                classCreationMethodsSucc
                  .getOrElseUpdate(t, makeClassCreationMethod(t)).ref,
              ),
            )(AssignLocalOk)
          } else {
            val unique = v.t.asPointer.get.unique
            Assign(
              localLowered(v).get,
              procedureInvocation[Post](
                TrueSatisfiable,
                classPointerCreationMethodsSucc.getOrElseUpdate(
                  (t, unique),
                  makeClassPointerCreationMethod(t, unique),
                ).ref,
              ),
            )(AssignLocalOk)
          }
        } else { Block(Nil) }
      case _ => node.rewriteDefault()
    }
  }

  override def dispatch(node: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = node.o
    node match {
      case DerefPointer(HeapLocal(Ref(v))) if localStripped.contains(v) =>
        localStripped(v).get
      case DerefPointer(deref @ DerefHeapVariable(Ref(v)))
          if globalStripped.contains(v) =>
        DerefHeapVariable[Post](globalStripped(v).ref)(deref.blame)
      case HeapLocal(Ref(v)) if localLowered.contains(v) => {
        // localLowered.contains(v) should always be true since all localStripped HeapLocals would be caught by DerefPointer(HeapLocal(Ref(v)))
        Local(localLowered.ref(v))
      }
      case _ => node.rewriteDefault()
    }
  }

  override def dispatch(loc: Location[Pre]): Location[Post] = {
    implicit val o: Origin = loc.o
    loc match {
      case PointerLocation(DerefHeapVariable(Ref(v)))
          if globalStripped.contains(v) =>
        HeapVariableLocation[Post](globalStripped(v).ref)
      case _ => loc.rewriteDefault()
    }
  }
}
