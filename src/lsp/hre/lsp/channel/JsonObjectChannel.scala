package hre.lsp.channel

trait JsonObjectChannel {
  def read(): Option[ujson.Obj]
  def write(obj: ujson.Obj): Unit
}
