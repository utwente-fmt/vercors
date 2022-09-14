package vct.parsers

case object Language {
  def fromFilename(filename: String): Option[Language] =
    filename.split('.').last match {
      case "cl" | "c" => Some(C)
      case "cu" => Some(CUDA)
      case "i" => Some(InterpretedC)
      case "java" => Some(Java)
      case "pvl" => Some(PVL)
      case "sil" | "vpr" => Some(Silver)
      case _ => None
    }

  case object C extends Language
  case object CUDA extends Language
  case object InterpretedC extends Language
  case object Java extends Language
  case object PVL extends Language
  case object Silver extends Language
}

sealed trait Language