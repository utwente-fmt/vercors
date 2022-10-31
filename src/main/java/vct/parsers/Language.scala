package vct.parsers

case object Language {
  def fromFilename(filename: String): Option[Language] =
    filename.split('.').last match {
      case "cl" | "c" | "cu" => Some(C)
      case "cpp" => Some(CPP)
      case "i" => Some(InterpretedC)
      case "cppi" => Some(InterpretedCPP)
      case "java" => Some(Java)
      case "pvl" => Some(PVL)
      case "sil" | "vpr" => Some(Silver)
      case _ => None
    }

  case object C extends Language
  case object CPP extends Language
  case object InterpretedC extends Language
  case object InterpretedCPP extends Language
  case object Java extends Language
  case object PVL extends Language
  case object Silver extends Language
}

sealed trait Language