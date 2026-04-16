package vct.rewrite

// This pass rewrites a PVL theory to a Isabelle theory for the purpose of proving consistency of declared ADTs.
//
// This pass rewrites ADTs declared in a PVL theory to locales in Isabelle. The output Isabelle theory can then
// be loaded from the user to define a model for the ADT and show the model satisfies the axioms of the locale.
// At the end of this, the user has proved in the Isabelle theorem prover that the input ADT is consistent.
//
// In addition to rewriting the input ADTs to locales we also generate the scaffold for the proof of consistency.
// To complete the proof of consistency the user need to 1. define a type for the model, 2. define a function for each locale parameter,
// 3. prove each axiom of the locale is satisfied by the functions, 4. finish an interpretation proof.
//
// The scaffold is implemented as part of this rewrite by adding new nodes to the output theory.

import hre.util.ScopedStack
import vct.col.ast.{
  ADTAxiom,
  ADTFunction,
  ADTFunctionInvocation,
  AxiomaticDataType,
  Declaration,
  Expr,
  Forall,
  IsarCommand,
  IsarDataConstructor,
  IsarDatatypeCommand,
  IsarDefinitionCommand,
  IsarFunctionInvocation,
  IsarInterpretationCommand,
  IsarLiftDefinitionCommand,
  IsarLocaleCommand,
  IsarPartialConstructorCommand,
  IsarTheory,
  IsarTypedefCommand,
  Program,
  TAnyValue,
  TAxiomatic,
  TBool,
  TIsarType,
  TType,
  TVar,
  Type,
  Variable,
}
import vct.col.origin.Origin
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.AstBuildHelpers.foldAnd
import vct.col.util.SuccessionMap

import scala.collection.mutable

case object ColToIsar extends RewriterBuilder {
  override def key: String = "coltoisar"
  override def desc: String = "Translates COL to Isar"
}

case class ColToIsar[Pre <: Generation]() extends Rewriter[Pre] {
  private val adtLocaleSucc
      : SuccessionMap[AxiomaticDataType[Pre], IsarLocaleCommand[Post]] =
    SuccessionMap()
  private val typeVarSucc: SuccessionMap[Variable[Pre], Variable[Post]] =
    SuccessionMap()
  private val adtTypeSucc
      : SuccessionMap[AxiomaticDataType[Pre], IsarCommand[Post]] =
    SuccessionMap()
  private val functionAdtMap
      : mutable.Map[ADTFunction[Pre], AxiomaticDataType[Pre]] = mutable
    .HashMap()
  private val functionFixesMap
      : SuccessionMap[ADTFunction[Pre], IsarCommand[Post]] = SuccessionMap()
  private val inLocale: ScopedStack[Unit] = ScopedStack()

  override def dispatch(program: Program[Pre]): Program[Post] = {
    // adt function to parent ADT
    program.declarations.collect { case adt: AxiomaticDataType[Pre] =>
      adt.decls.collect { case f: ADTFunction[Pre] => functionAdtMap(f) = adt }
    }

    program.rewrite(declarations = {
      val (commands, decls) = isarCommands.collect {
        val refdt =
          new IsarDatatypeCommand[Post]("ref", Seq(), Seq())(program.o)
        isarCommands.declare(refdt)
        globalDeclarations.dispatch(program.declarations)
      }
      Seq(
        new IsarTheory[Post](
          Seq(
            "Main",
            "HOL.Rat",
            "\"HOL-Library.FSet\"",
            "\"HOL-Library.Finite_Map\"",
          ),
          commands,
        )(program.o)
      )
    })
  }

  override def dispatch(decl: Declaration[Pre]): Unit = {

    implicit val o: Origin = decl.o

    // Rewrite ADTs
    decl match {
      case adt: AxiomaticDataType[Pre] =>
        // Rewrite setup
        val currentAdtType = TAxiomatic[Pre](
          adt.ref,
          adt.typeArgs.map(v => TVar(v.ref)),
        )

        val adtFixes = adt.decls.collect { case fixes: ADTFunction[Pre] =>
          fixes
        }
        val adtAxioms = adt.decls.collect { case assume: ADTAxiom[Pre] =>
          assume.axiom
        }
        val constructors = adtFixes.filter { f =>
          val retMatches =
            f.returnType ==
              currentAdtType // return type matches the one declared in this ADT
          val argsMatch = f.args.forall { v =>
            v.t match {
              // NOTE no free type variables allowed
              // NOTE no recursive types allowed
              case TAxiomatic(Ref(other), _) =>
                other !=
                  adt // not a constructor because type signature references same ADT
              case _ => true
            }
          }
          retMatches &&
          argsMatch // argument types match those from ADT type declaration and it's not recursive
        }
        val accessors = adtFixes.filter { f =>
          val retNotMatches =
            f.returnType !=
              currentAdtType // return type matches the one declared in this ADT
          val argsMatch = f.args.size == 1 && f.args.head.t == currentAdtType
          retNotMatches &&
          argsMatch // argument types match those from ADT type declaration and it's not recursive
        }
        val semanticAxioms = adtAxioms.collect {
          case f @ Forall(bindings, _, body)
              if bindings.size == 1 && bindings.head.t == currentAdtType &&
                body.collect {
                  // NOTE semantic axioms only include accessors from the current ADT and accessors from the referenced ADTs
                  // TODO implement the accessors from the referenced ADTSs
                  case ADTFunctionInvocation(_, Ref(f), _) =>
                    accessors.contains(f)
                }.forall(identity) =>
            f
        }

        // prefix for all names defined for the ADT
        val dataPrefix =
          (if (semanticAxioms.isEmpty) { "" }
           else { "Raw" })
        val typePostfix =
          (if (semanticAxioms.isEmpty) { "" }
           else { "_raw" })

        // Rewrite starts here

        // Define type
        //
        // We will add a type to the theory depending on the following:
        //
        // - typedecl: when we have no constructors
        // - datatype: when we have constructors
        // - typedef: when we have constructors and semantic axioms

        val (datatypeConstructors, dt) = variables.scope {

          val datatypeConstructors = constructors.map { f =>
            val dc =
              new IsarDataConstructor[Post](
                dataPrefix + f.o.getPreferredName.map(_.camel).get,
                (f.args.map(_.t) :+ f.returnType).map(dispatch),
              )
            functionFixesMap(f) = dc
            dc
          }
          val dt =
            new IsarDatatypeCommand[Post](
              adt.o.getPreferredName.map(_.camel).get + typePostfix,
              variables.dispatch(adt.typeArgs),
              datatypeConstructors,
            )
          (datatypeConstructors, dt)
        }
        adtTypeSucc(adt) = dt
        isarCommands.declare(dt)

        // Define functions
        //
        // Data constructors are already taken care of by the datatype
        // We add names for accessor/destructor. These may be lifter later.

        // NOTE instead of using inline syntax in the datatype to name accessors it's just easier to add more names
        val datatypeAccessors = accessors.map { f =>
          val dc = variables.scope {
            new IsarDefinitionCommand[Post](
              dataPrefix + f.o.getPreferredName.map(_.camel).get,
              variables.dispatch(adt.typeArgs),
              f.args.map(_.t).map {
                case TAxiomatic(Ref(adt2), args) if adt2 == adt =>
                  new Variable(TIsarType[Post](dt.ref, args.map(dispatch)))
                case other => new Variable(dispatch(other))
              },
              dispatch(f.returnType),
              None,
            )
          }
          functionFixesMap(f) = dc
          isarCommands.declare(dc)
        }

        // Does the type requires a typedef?

        if (semanticAxioms.nonEmpty) {
          // typedef required

          // start with naming the conjunction of all axioms
          val tVars = dt.typevars
            .map(_ => new Variable[Post](TType(TAnyValue())))
          val x =
            new Variable[Post](TIsarType(dt.ref, tVars.map(v => TVar(v.ref))))(
              o.where(name = "x")
            )
          val axiomBodies = semanticAxioms.map { f =>
            variables.succeedOnly(f.bindings.head, x)
            dispatch(f.body)
          }
          val axioms =
            new IsarDefinitionCommand[Post](
              adt.o.getPreferredName.map(_.camel).get + "_axioms",
              tVars,
              Seq(x),
              TBool(),
              Some(foldAnd(axiomBodies)),
            )
          isarCommands.declare(axioms)

          // then we can do a typedef
          val td = variables.scope {
            new IsarTypedefCommand[Post](
              dt.o.getPreferredName.map(_.camel).get,
              variables.dispatch(adt.typeArgs),
              dt.ref,
              axioms.ref,
            )
          }
          isarCommands.declare(td)
          adtTypeSucc(adt) = td

          // We need the constructors on the typedef and we also need to make it partial

          // definition constPointerOf :: "'a list ⇒ int ⇒ 'a ConstPointer"
          //    where
          // "constPointerOf l i ≡ if ConstPointer_axioms (RConstP l i) then Abs_ConstPointer (RConstP l i) else undefined"

          constructors.map { f =>
            variables.scope {
              val dc =
                new IsarPartialConstructorCommand[Post](
                  f.o.getPreferredName.map(_.camel).get,
                  variables.dispatch(adt.typeArgs),
                  f.args.map(a => new Variable[Post](dispatch(a.t))),
                  dispatch(f.returnType),
                  axioms.ref,
                  functionFixesMap.get(f).get.ref,
                )
              isarCommands.declare(dc)
              functionFixesMap(f) = dc
            }
          }

          // Accessors on the typedef are lifted using lift_definition

          accessors.map { f =>
            variables.scope {
              val dc =
                new IsarLiftDefinitionCommand[Post](
                  f.o.getPreferredName.map(_.camel).get,
                  variables.dispatch(adt.typeArgs),
                  (f.args.map(_.t) :+ f.returnType).map(dispatch),
                  functionFixesMap.get(f).get.ref,
                )
              functionFixesMap(f) = dc
              isarCommands.declare(dc)
            }
          }

          // NOTE type variables mapping is bound to typedef node

        } else {
          // typedef not required
          // NOTE type variable mapping is bound to datatype node
        }

        // top-level functions

        adtFixes.collect {
          case f if !constructors.contains(f) && !accessors.contains(f) =>
            variables.scope {
              val df =
                new IsarDefinitionCommand[Post](
                  f.o.getPreferredName.map(_.camel).get,
                  variables.dispatch(adt.typeArgs),
                  f.args.map(a => new Variable[Post](dispatch(a.t))),
                  dispatch(f.returnType),
                  None,
                )
              isarCommands.declare(df)
              functionFixesMap(f) = df
            }
        }

        val referencedAdts =
          adt.collect {
            // NOTE: an ADT may be referenced without ever using its declared functions.
            // This only happens when the type is present in a type signature,
            // e.g. iterate : List<K> => List<Pair(int, K)> from List ADT references Pair ADT
            // Then operations on the type are restricted to the following:
            // - equality
            // - inequality
            // Which means it is safe to ignore ADTs only referenced in a type signature
            // case TAxiomatic(Ref(other), _) => other // found ADT in type signature
            case ADTFunctionInvocation(_, Ref(f), _) =>
              functionAdtMap(f) // ADT function invocation
          }.filter(_ != adt).distinct // deduplicate

        val locale = variables.scope {
          inLocale.having(()) {
            val locale =
              new IsarLocaleCommand[Post](
                adt.o.getPreferredName.map(_.camel).get,
                referencedAdts.map(a => adtLocaleSucc.ref(a)),
                variables.dispatch(adt.typeArgs),
                adtFixes
                  .map(f => aDTDeclarations.succeedOnly(f, f.rewriteDefault())),
                adtAxioms.map(dispatch),
              )

            adtLocaleSucc(adt) = locale
            isarCommands.declare(locale)
          }
        }

        val interpretation =
          new IsarInterpretationCommand[Post](
            locale.ref,
            adtFixes.map { f => functionFixesMap.ref(f) },
          )
        isarCommands.declare(interpretation)
      case _ => super.dispatch(decl)
    }
  }

  override def dispatch(node: Type[Pre]): Type[Post] =
    node match {
      case TAxiomatic(Ref(adt), args) =>
        TIsarType(adtTypeSucc.ref(adt), args.map(dispatch))
      case _ => super.dispatch(node)
    }

  override def dispatch(e: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = e.o
    e match {
      case ADTFunctionInvocation(typeArgs, Ref(f), args) if inLocale.isEmpty =>
        IsarFunctionInvocation(
          typeArgs.map(_._2.map(dispatch)).getOrElse(Nil),
          functionFixesMap(f).ref,
          args.map(dispatch),
        )
      case _ => super.dispatch(e)
    }
  }
}
