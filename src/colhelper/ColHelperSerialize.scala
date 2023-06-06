import ColDefs.DECLARATION_KINDS

import scala.meta._

case class ColHelperSerialize(info: ColDescription, proto: ColProto) extends ColHelperMaker {
  def serType(name: String): Type = t"ser.${Type.Name(proto.Name(name).ucamel)}"

  def makeFamilyDispatch(family: String): List[Stat] = List(q"""
    def serialize(node: ${Type.Name(family)}[_]): ${serType(family)} =
      ${if(info.defs.exists(_.baseName == family)) {
        q"${Term.Name("serialize" + family)}(node)"
      } else {
        q"Serialize.${Term.Name(family + "Lut")}(node.getClass)(this, node)"
      }}
  """)

  def makeFamilyDispatchLut(family: String, decl: Boolean): List[Stat] =
    if(info.defs.exists(_.baseName == family)) Nil
    else List(
      q"""
        val ${Pat.Var(Term.Name(family + "Lut"))}: Map[java.lang.Class[_], (Serialize, ${t"${Type.Name(family)}[_]"}) => ${serType(family)}] = Map(..${
          info.defs.filter(d => info.supports(family)(d.baseName)).map { defn =>
            val vArg = q"""ser.${Term.Name(proto.Name(family).ucamel)}.V.${Term.Name(proto.Name(defn.baseName).ucamel)}(
              s.${Term.Name("serialize" + defn.baseName)}(n.asInstanceOf[${defn.typ}[_]])
            )"""
            val idArgs = if(decl) Seq(q"s.decls(n)") else Nil
            val args = (idArgs :+ vArg).toList
            q"""
              classOf[${defn.typ}[_]] -> ((s: Serialize, n: ${Type.Name(family)}[_]) =>
                ser.${Term.Name(proto.Name(family).ucamel)}(..$args)
              )
            """
          }.toList
        })
      """
    )

  def serializeTerm(term: Term, typ: proto.Typ): Term =
    proto.primitivize(typ) match {
      case proto.TName("ExpectedErrors") => q"ser.ExpectedErrors()"

      case proto.TBool => term
      case proto.TRef() => q"ser.Ref(decls($term.decl))"
      case proto.TInt => term
      case proto.TBigInt => q"ser.BigInt(com.google.protobuf.ByteString.copyFrom($term.toByteArray))"
      case proto.TBigDecimal => q"ser.BigDecimal($term.scale, ser.BigInt(com.google.protobuf.ByteString.copyFrom($term.underlying().unscaledValue().toByteArray())))"
      case proto.TString => term
      case proto.TBitString => q"ser.BitString(com.google.protobuf.ByteString.copyFrom($term.rawData), $term.skipAtLastByte)"
      case proto.TOption(t) => q"$term.map(e => ${serializeTerm(q"e", t)})"
      case proto.TSeq(t) => q"$term.map(e => ${serializeTerm(q"e", t)})"
      case proto.TSet(t) => q"$term.toSeq.map(e => ${serializeTerm(q"e", t)})"
      case typ @ proto.TName(name) if proto.boxedTypeFamily.contains(typ) => q"serialize($term)"
      case typ @ proto.TName(name) if proto.boxedTypeForward.contains(typ) => q"ser.${Term.Name(name)}(${serializeTerm(term, proto.boxedTypeForward(typ))})"
      case typ @ proto.TName(name) if proto.boxedTypeTuple.contains(typ) =>
        q"""ser.${Term.Name(name)}(..${
          val ts = proto.boxedTypeTuple(typ).ts
          ts.zipWithIndex.map {
            case (typ, i) => serializeTerm(Term.Select(term, Term.Name(s"_${i+1}")), typ)
          }.toList
        })"""
      case _ => ColHelperUtil.fail(s"Unknown type $typ")
    }

  def serializeParam(defn: ClassDef)(param: Term.Param): Term =
    serializeTerm(q"node.${Term.Name(param.name.value)}", proto.getType(param.decltpe.get))

  def makeNodeSerialize(defn: ClassDef): List[Stat] = List(q"""
    def ${Term.Name("serialize" + defn.baseName)}(node: ${defn.typ}[_]): ${serType(defn.baseName)} =
      ser.${Term.Name(proto.Name(defn.baseName).ucamel)}(..${
        val nodeParams = defn.params.map(serializeParam(defn))
        val idParams = if(DECLARATION_KINDS.contains(defn.baseName)) List(q"decls(node)") else Nil
        val originParam = List(q"new String()")
        idParams ++ originParam ++ nodeParams
      })
  """)

  def makeSerialize(): List[Stat] = q"""
    import vct.col.{serialize => ser}

    object Serialize {
      def serialize(program: Program[_]): ser.Program = {
        val decls = program.collect { case decl: Declaration[_] => decl }.zipWithIndex.toMap
        Serialize(decls).serializeProgram(program)
      }

      def serialize(verification: Verification[_]): ser.Verification = {
        val decls = verification.collect { case decl: Declaration[_] => decl }.zipWithIndex.toMap
        Serialize(decls).serializeVerification(verification)
      }

      ..${DECLARATION_KINDS.flatMap(makeFamilyDispatchLut(_, decl = true)).toList}
      ..${info.families.flatMap(makeFamilyDispatchLut(_, decl = false)).toList}
    }

    case class Serialize(decls: Map[Declaration[_], Int]) {
      ..${DECLARATION_KINDS.flatMap(makeFamilyDispatch).toList}
      ..${info.families.flatMap(makeFamilyDispatch).toList}
      ..${info.defs.flatMap(makeNodeSerialize).toList}
    }
  """.stats

  def make(): List[(String, List[Stat])] =
    List(("Serialize", makeSerialize()))
}
