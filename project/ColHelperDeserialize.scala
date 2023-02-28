import ColDefs.DECLARATION_KINDS
import scala.meta._

case class ColHelperDeserialize(info: ColDescription, proto: ColProto) extends ColHelperMaker {
  def serType(name: String): Type = t"ser.${Type.Name(proto.Name(name).ucamel)}"
  def serTypeV(name: String): Type = t"ser.${Term.Name(proto.Name(name).ucamel)}.V"
  def serTypeVKind(name: String, kind: String): Type = t"${q"ser.${Term.Name(proto.Name(name).ucamel)}.V"}.${Type.Name(proto.Name(kind).ucamel)}"

  def makeFamilyDispatch(family: String): List[Stat] = List(q"""
    def deserialize(node: ${serType(family)}): ${Type.Name(family)}[G] =
      ${if(info.defs.exists(_.baseName == family)) {
        q"${Term.Name("deserialize" + family)}(node)"
      } else {
        q"Deserialize.${Term.Name(family + "Lut")}(node.v.getClass)(this, node).asInstanceOf[${Type.Name(family)}[G]]"
      }}
  """)

  def makeFamilyDispatchLut(family: String, decl: Boolean): List[Stat] =
    if(info.defs.exists(_.baseName == family)) Nil
    else List(q"""
      val ${Pat.Var(Term.Name(family + "Lut"))}: Map[java.lang.Class[_], (Deserialize[_], ${serType(family)}) => ${t"${Type.Name(family)}[_]"}] = Map(..${
        info.defs.filter(d => info.supports(family)(d.baseName)).map { defn =>
          val t = serTypeVKind(family, defn.baseName)
          val v = q"s.${Term.Name("deserialize" + defn.baseName)}(n.v.asInstanceOf[$t].value)"
          val update = if(decl) q"s.decls.getOrElseUpdate(n.id, $v.asInstanceOf).asInstanceOf" else v
          q"classOf[$t] -> ((s: Deserialize[_], n: ${serType(family)}) => $update)"
        }.toList
      })
    """)

  def deserializeTerm(term: Term, typ: proto.Typ): Term =
    proto.primitivize(typ) match {
      case proto.TName("ExpectedErrors") => q"Nil"

      case proto.TBool => term
      case proto.TRef => q"ref($term.index)"
      case proto.TInt => term
      case proto.TBigInt => q"BigInt(new java.math.BigInteger($term.data.toByteArray()))"
      case proto.TBigDecimal => q"BigDecimal(${deserializeTerm(q"$term.unscaledValue", proto.TBigInt)}, $term.scale)"
      case proto.TString => term
      case proto.TOption(t) => q"$term.map(e => ${deserializeTerm(q"e", t)})"
      case proto.TSeq(t) => q"$term.map(e => ${deserializeTerm(q"e", t)})"
      case proto.TSet(t) => q"$term.map(e => ${deserializeTerm(q"e", t)}).toSet"
      case typ @ proto.TName(name) if proto.boxedTypeFamily.contains(typ) => q"deserialize($term)"
      case typ @ proto.TName(name) if proto.boxedTypeForward.contains(typ) => deserializeTerm(q"$term.v", proto.boxedTypeForward(typ))
      case typ @ proto.TName(name) if proto.boxedTypeTuple.contains(typ) =>
        q"""(..${
          val ts = proto.boxedTypeTuple(typ).ts
          ts.zipWithIndex.map {
            case (typ, i) => deserializeTerm(Term.Select(term, Term.Name(s"v${i+1}")), typ)
          }.toList
        })"""
      case _ => ColHelperUtil.fail(s"Unknown type $typ")
    }

  // PB: pitfall: scalapb normalizes snake_case -> snakeCase, so:
  // contextEverywhere (Node.scala) -> context_everywhere (proto) -> contextEverywhere (scalapb)
  // context_everywhere (Node.scala; poor style) -> context_everywhere (proto) -> contextEverywhere (scalapb)
  def deserializeParam(defn: ClassDef)(param: Term.Param): Term =
    deserializeTerm(q"node.${Term.Name(proto.Name(param.name.value).camel)}", proto.getType(param.decltpe.get))

  def makeNodeDeserialize(defn: ClassDef): List[Stat] = List(q"""
    def ${Term.Name("deserialize" + defn.baseName)}(node: ${serType(defn.baseName)}): ${defn.typ}[G] =
      ${defn.make(defn.params.map(deserializeParam(defn)), q"Deserialize.Origin", q"Deserialize.Origin")}
  """)

  def makeDeserialize(): List[Stat] = q"""
    import vct.col.{serialize => ser}
    import vct.col.ref.LazyRef
    import scala.collection.mutable
    import scala.reflect.ClassTag

    object Deserialize {
      case object Origin extends vct.col.origin.Origin {
        override def preferredName: String = "unknown"
        override def context: String = "At: [deserialized node]"
        override def inlineContext: String = "[Deserialized node]"
        override def shortPosition: String = "serialized"
      }

      def deserialize[G](program: ser.Program): Program[G] = {
        Deserialize[G](mutable.Map()).deserializeProgram(program)
      }

      ..${DECLARATION_KINDS.flatMap(makeFamilyDispatchLut(_, decl = true)).toList}
      ..${info.families.flatMap(makeFamilyDispatchLut(_, decl = false)).toList}
    }

    case class Deserialize[G](decls: mutable.Map[Long, Declaration[G]]) {
      def ref[T <: Declaration[G]](id: Long)(implicit tag: ClassTag[T]): Ref[G, T] =
        new LazyRef[G, T](decls(id))

      ..${DECLARATION_KINDS.flatMap(makeFamilyDispatch).toList}
      ..${info.families.flatMap(makeFamilyDispatch).toList}
      ..${info.defs.flatMap(makeNodeDeserialize).toList}
    }
  """.stats

  def make(): List[(String, List[Stat])] =
    List(("Deserialize", makeDeserialize()))
}
