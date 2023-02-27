import scala.meta._

case class ColHelperDeserialize(info: ColDescription, proto: ColProto) extends ColHelperMaker {
  def make(): List[(String, List[Stat])] = Nil
}
