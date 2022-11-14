import scala.meta._

case class ColHelperSerialize(info: ColDescription, proto: ColProto) extends ColHelperMaker {
  def makeFamilyDispatch(family: String): List[Stat] = Nil

  def serializeTerm(term: Term, typ: proto.Typ): Term =
    proto.primitivize(typ) match {
      case proto.TBool => term
      case proto.TRef => q"ser.Ref(decls($term.decl))"
      case proto.TInt => term
      case proto.TBigInt => q"ser.BigInt($term.toByteArray)"
      case proto.TBigDecimal => q"ser.BigDecimal($term.scale, ser.BigInt($term.underlying().unscaledValue().toByteArray()))"
      case proto.TString => term
      case proto.TOption(t) => q"$term.map(e => ${serializeTerm(q"e", t)})"
      case proto.TSeq(t) => q"$term.map(e => ${serializeTerm(q"e", t)})"
      case proto.TSet(t) => q"$term.map(e => ${serializeTerm(q"e", t)})"
      case proto.TName(name) => ???
      case _ => ???
    }

  def serializeParam(defn: ClassDef)(param: Term.Param): Term =
    serializeTerm(q"node.${Term.Name(param.name.value)}", proto.getType(param.decltpe.get))

  def makeNodeSerialize(defn: ClassDef): List[Stat] = List(q"""
    def ${Term.Name("serialize" + defn.baseName)}(node: ${defn.typ}[_]): ser.${Type.Name(proto.Name(defn.baseName).ucamel)} =
      ser.${Term.Name(proto.Name(defn.baseName).ucamel)}(..${defn.params.map(serializeParam(defn))})
  """)

  def makeSerialize(): List[Stat] = q"""
    import vct.col.{serialize => ser}

    object Serialize {
      def serialize(program: Program[_]): ser.Program = {
        val decls = program.collect { case decl: Declaration[_] => decl }.zipWithIndex.toMap
        Serialize(decls).serializeProgram(program)
      }
    }

    case class Serialize(decls: Map[Declaration[_], Int]) {
      ..${makeFamilyDispatch("Declaration")}
      ..${info.families.flatMap(makeFamilyDispatch).toList}
      ..${info.defs.flatMap(makeNodeSerialize).toList}
    }
  """.stats

  def make(): List[(String, List[Stat])] =
    List(("Serialize", makeSerialize()))
}
