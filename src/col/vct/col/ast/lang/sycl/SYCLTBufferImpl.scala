package vct.col.ast.lang.sycl

import vct.col.ast._
import vct.col.ast.ops.SYCLTBufferOps
import vct.col.origin.PanicBlame
import vct.col.print.{Ctx, Doc, Group, Text}
import vct.col.ref.Ref
import vct.col.resolve.ctx.{CPPInvocationTarget, RefSYCLConstructorDefinition}
import vct.col.resolve.lang.{CPP, Util}
import vct.col.util.AstBuildHelpers.{
  ExprBuildHelpers,
  c_const,
  foldStar,
  tt,
  withResult,
}
import vct.col.ast.ops.SYCLTBufferOps

trait SYCLTBufferImpl[G] extends SYCLTBufferOps[G] {
  this: SYCLTBuffer[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(
      Text("sycl::buffer") <> "<" <> typ <> ", " <> Text(dimCount.toString) <>
        ">"
    )

  override val namespacePath = "sycl::buffer"

  def findConstructor(
      genericArgs: Seq[CPPExprOrTypeSpecifier[G]],
      args: Seq[Expr[G]],
  ): Option[CPPInvocationTarget[G]] =
    genericArgs match {
      case Seq(
            CPPExprOrTypeSpecifier(None, Some(typeSpec)),
            CPPExprOrTypeSpecifier(Some(CIntegerValue(dim)), None),
          )
          if dim > 0 && dim <= 3 && args.nonEmpty &&
            args.head.t.asPointer.isDefined &&
            CPP.getBaseTypeFromSpecs[G](Seq(typeSpec))
              .superTypeOf(args.head.t.asPointer.get.element) &&
            Util.compatTypes[G](args.tail, Seq(SYCLTRange[G](dim.toInt))) =>
        Some(RefSYCLConstructorDefinition[G](
          SYCLTBuffer[G](CPP.getBaseTypeFromSpecs[G](Seq(typeSpec)), dim.toInt)
        ))
      case Seq(CPPExprOrTypeSpecifier(None, Some(typeSpec)))
          if args.nonEmpty && args.head.t.asPointer.isDefined &&
            CPP.getBaseTypeFromSpecs[G](Seq(typeSpec))
              .superTypeOf(args.head.t.asPointer.get.element) &&
            Util.compatTypes[G](args.tail, Seq(SYCLTRange[G](1))) =>
        Some(RefSYCLConstructorDefinition(
          SYCLTBuffer(CPP.getBaseTypeFromSpecs(Seq(typeSpec)), 1)
        ))
      case Seq(CPPExprOrTypeSpecifier(None, Some(typeSpec)))
          if args.nonEmpty && args.head.t.asPointer.isDefined &&
            CPP.getBaseTypeFromSpecs[G](Seq(typeSpec))
              .superTypeOf(args.head.t.asPointer.get.element) &&
            Util.compatTypes[G](args.tail, Seq(SYCLTRange[G](2))) =>
        Some(RefSYCLConstructorDefinition(
          SYCLTBuffer(CPP.getBaseTypeFromSpecs(Seq(typeSpec)), 2)
        ))
      case Seq(CPPExprOrTypeSpecifier(None, Some(typeSpec)))
          if args.nonEmpty && args.head.t.asPointer.isDefined &&
            CPP.getBaseTypeFromSpecs[G](Seq(typeSpec))
              .superTypeOf(args.head.t.asPointer.get.element) &&
            Util.compatTypes[G](args.tail, Seq(SYCLTRange[G](3))) =>
        Some(RefSYCLConstructorDefinition(
          SYCLTBuffer(CPP.getBaseTypeFromSpecs(Seq(typeSpec)), 3)
        ))
      case Nil
          if args.nonEmpty && args.head.t.asPointer.isDefined &&
            Util.compatTypes(args.tail, Seq(SYCLTRange[G](1))) =>
        Some(RefSYCLConstructorDefinition(
          SYCLTBuffer(args.head.t.asPointer.get.element, 1)
        ))
      case Nil
          if args.nonEmpty && args.head.t.asPointer.isDefined &&
            Util.compatTypes(args.tail, Seq(SYCLTRange[G](2))) =>
        Some(RefSYCLConstructorDefinition(
          SYCLTBuffer(args.head.t.asPointer.get.element, 2)
        ))
      case Nil
          if args.nonEmpty && args.head.t.asPointer.isDefined &&
            Util.compatTypes(args.tail, Seq(SYCLTRange[G](3))) =>
        Some(RefSYCLConstructorDefinition(
          SYCLTBuffer(args.head.t.asPointer.get.element, 3)
        ))
      case _ => None
    }

  // resource exclusive_hostData_access(TYPE* hostData, int range) = \pointer(hostData, range, write);
  def generateExclusiveAccessPredicate(): Predicate[G] = {
    val hostDataVar =
      new Variable[G](TPointer(this.typ))(o.where(name = "hostData"))
    val sizeVar =
      new Variable[G](TInt())(
        o.where(name = "size")
      ) // Needs to be TInt instead of TCInt as it is called with a Length expr which has type TInt

    new Predicate[G](
      args = Seq(hostDataVar, sizeVar),
      body = Some(PermPointer[G](
        Local[G](hostDataVar.ref),
        Local[G](sizeVar.ref),
        WritePerm(),
      )),
    )(o.where(name = "exclusive_hostData_access"))
  }

  // requires size >= 0;
  // context \pointer(hostData, size, write);
  // ensures \array(\result, size) ** Perm(\result[*], write);
  // ensures (\forall int i; i >= 0 && i < size; {: \result[i] :} == hostData[i]);
  // TYPE[] copy_hostdata_to_buffer(TYPE* hostData, int size);
  def generateCopyHostDataToBufferProcedure(): Procedure[G] = {
    val hostDataVar =
      new Variable[G](TPointer(this.typ))(o.where(name = "hostData"))
    val sizeVar = new Variable[G](TCInt())(o.where(name = "size"))
    val indexVar = new Variable[G](TCInt())(o.where(name = "i"))
    val copyHostdataToBufferBlame = PanicBlame(
      "The generated method 'copy_hostData_to_buffer' should not throw any errors."
    )

    withResult((result: Result[G]) => {
      new Procedure[G](
        returnType = TArray(this.typ),
        args = Seq(hostDataVar, sizeVar),
        outArgs = Nil,
        typeArgs = Nil,
        body = None,
        contract =
          ApplicableContract(
            requires = UnitAccountedPredicate(Star(
              GreaterEq(Local[G](sizeVar.ref), c_const(0)),
              PermPointer[G](
                Local[G](hostDataVar.ref),
                Local[G](sizeVar.ref),
                WritePerm(),
              ),
            )),
            ensures = UnitAccountedPredicate(foldStar(Seq(
              PermPointer[G](
                Local[G](hostDataVar.ref),
                Local[G](sizeVar.ref),
                WritePerm(),
              ),
              validArray(
                result,
                Local[G](sizeVar.ref),
                WritePerm(),
                copyHostdataToBufferBlame,
              ),
              Forall(
                bindings = Seq(indexVar),
                triggers = Seq(
                  Seq(ArraySubscript(result, Local[G](indexVar.ref))(
                    copyHostdataToBufferBlame
                  ))
                ),
                body =
                  (Local[G](indexVar.ref) >= c_const(0) &&
                    Local[G](indexVar.ref) < Local[G](sizeVar.ref)) ==> Eq(
                    ArraySubscript(result, Local[G](indexVar.ref))(
                      copyHostdataToBufferBlame
                    ),
                    PointerSubscript(
                      Local[G](hostDataVar.ref),
                      Local[G](indexVar.ref),
                    )(copyHostdataToBufferBlame),
                  ),
              ),
            ))),
            contextEverywhere = tt,
            signals = Nil,
            givenArgs = Nil,
            yieldsArgs = Nil,
            decreases = None,
          )(copyHostdataToBufferBlame),
      )(copyHostdataToBufferBlame)(o.where(name = "copy_hostData_to_buffer"))
    })
  }

  // context sycl::buffer::exclusive_hostData_access(hostData, buffer.length);
  // context \array(buffer, buffer.length) ** Perm(buffer[*], read);
  // ensures \unfolding sycl::buffer::exclusive_hostData_access(hostData, buffer.length) \in (\forall int i; i >= 0 && i < buffer.length; {: hostData[i] :} == buffer[i]);
  // void copy_buffer_to_hostdata(TYPE* hostData, TYPE[] buffer);
  def generateCopyBufferToHostDataProcedure(
      exclusiveAccessPredRef: Ref[G, Predicate[G]]
  ): Procedure[G] = {
    val bufferVar = new Variable[G](TArray(this.typ))(o.where(name = "buffer"))
    val hostDataVar =
      new Variable[G](TPointer(this.typ))(o.where(name = "hostData"))
    val indexVar = new Variable[G](TCInt())(o.where(name = "i"))
    val copyBufferToHostdataBlame = PanicBlame(
      "The generated method 'copy_buffer_to_hostData' should not throw any errors."
    )

    new Procedure[G](
      returnType = TVoid(),
      args = Seq(hostDataVar, bufferVar),
      outArgs = Nil,
      typeArgs = Nil,
      body = None,
      contract =
        ApplicableContract(
          requires = UnitAccountedPredicate(foldStar(Seq(
            validArray(
              Local(bufferVar.ref),
              Length(Local[G](bufferVar.ref))(copyBufferToHostdataBlame),
              ReadPerm(),
              copyBufferToHostdataBlame,
            ),
            predApply(
              exclusiveAccessPredRef,
              hostDataVar,
              Length(Local[G](bufferVar.ref))(copyBufferToHostdataBlame),
            ),
          ))),
          ensures = UnitAccountedPredicate(foldStar(Seq(
            validArray(
              Local(bufferVar.ref),
              Length(Local[G](bufferVar.ref))(copyBufferToHostdataBlame),
              ReadPerm(),
              copyBufferToHostdataBlame,
            ),
            predApply(
              exclusiveAccessPredRef,
              hostDataVar,
              Length(Local[G](bufferVar.ref))(copyBufferToHostdataBlame),
            ),
            Unfolding(
              predApply(
                exclusiveAccessPredRef,
                hostDataVar,
                Length(Local[G](bufferVar.ref))(copyBufferToHostdataBlame),
              ),
              Forall(
                bindings = Seq(indexVar),
                triggers = Seq(Seq(
                  PointerSubscript(
                    Local[G](hostDataVar.ref),
                    Local[G](indexVar.ref),
                  )(copyBufferToHostdataBlame)
                )),
                body =
                  (
                    Local[G](indexVar.ref) >= c_const(0) &&
                      Local[G](indexVar.ref) <
                      Length(Local[G](bufferVar.ref))(copyBufferToHostdataBlame)
                  ) ==> Eq(
                    PointerSubscript(
                      Local[G](hostDataVar.ref),
                      Local[G](indexVar.ref),
                    )(copyBufferToHostdataBlame),
                    ArraySubscript[G](
                      Local[G](bufferVar.ref),
                      Local[G](indexVar.ref),
                    )(copyBufferToHostdataBlame),
                  ),
              ),
            )(copyBufferToHostdataBlame),
          ))),
          contextEverywhere = tt,
          signals = Nil,
          givenArgs = Nil,
          yieldsArgs = Nil,
          decreases = None,
        )(copyBufferToHostdataBlame),
    )(copyBufferToHostdataBlame)(o.where(name = "copy_buffer_to_hostData"))
  }

  private def validArray(
      arr: Expr[G],
      len: Expr[G],
      perm: Expr[G],
      blame: PanicBlame,
  ): Expr[G] = {
    Star(
      ValidArray[G](arr, len),
      Perm(ArrayLocation(arr, Any()(blame))(blame), perm),
    )
  }

  private def predApply(
      exclusiveAccessPredRef: Ref[G, Predicate[G]],
      hostData: Variable[G],
      len: Expr[G],
  ): Expr[G] = {
    PredicateApply[G](
      exclusiveAccessPredRef,
      Seq(Local[G](hostData.ref), len),
      WritePerm(),
    )
  }
}
