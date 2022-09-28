package vct.options.types

import vct.main.stages.Parsing.Language

case object ReadLanguage extends ReadEnum[Language] {
  override val options: Map[String, Language] = Map(
    "java" -> Language.Java,
    "c" -> Language.C,
    "i" -> Language.InterpretedC,
    "pvl" -> Language.PVL,
    "silver" -> Language.Silver,
  )
}
