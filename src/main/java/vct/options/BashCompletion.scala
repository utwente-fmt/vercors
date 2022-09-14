package vct.options

import scopt.OptionDef
import vct.parsers.Language

import java.nio.file.Path
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.{TermName, runtimeMirror}

case object BashCompletion {
  private val mirror = runtimeMirror(getClass.getClassLoader)
  private val tOptDef = mirror.staticClass("scopt.OptionDef")
  private val abbrSymbol = tOptDef.info.decl(TermName("_shortOpt")).asTerm

  def abbr[A, C](opt: OptionDef[A, C]): Option[String] =
    mirror.reflect(opt).reflectField(abbrSymbol).get.asInstanceOf[Option[String]]

  def main(args: Array[String]): Unit = {
    val (parser, tags) = Options.constructParser(hide = true)

    var pathOpts = Seq.empty[String]
    var pathOrStdOpts = Seq.empty[String]
    var stringOpts = Seq.empty[String]
    var backendOpts = Seq.empty[String]
    var languageOpts = Seq.empty[String]
    var unknownOpts = Seq.empty[String]

    for(option <- parser.toList if option.name.nonEmpty) {
      val opts = Seq("--" + option.name) ++ abbr(option).map("-" + _).toSeq

      // We only add types that have exactly one argument, and we can for sure know all the suggestions we should make.
      // Otherwise, we just fall back to all files and options.
      val path = classOf[Path]
      val string = classOf[String]

      val pathOrStd = classOf[PathOrStd]
      val backend = classOf[Backend]
      val language = classOf[Language]

      tags.get(option.name) match {
        case Some(ClassTag(`pathOrStd`)) => pathOrStdOpts ++= opts
        case Some(ClassTag(`string`)) => stringOpts ++= opts
        case Some(ClassTag(`path`)) => pathOpts ++= opts
        case Some(ClassTag(`backend`)) => backendOpts ++= opts
        case Some(ClassTag(`language`)) => languageOpts ++= opts
        case _ => unknownOpts ++= opts
      }
    }

    println("""
      |__vercors()
      |{
      |local cur prev pathOpts pathOrStdOpts stringOpts backendOpts languageOpts unknownOpts
      |COMPREPLY=()
      |cur="${COMP_WORDS[COMP_CWORD]}"
      |prev="${COMP_WORDS[COMP_CWORD-1]}"
      |pathOpts="%s"
      |pathOrStdOpts="%s"
      |stringOpts="%s"
      |backendOpts="%s"
      |languageOpts="%s"
      |unknownOpts="%s"
      |
      |if [[ ${prev} =~ ${pathOpts} ]]; then
      |  COMPREPLY=($(compgen -f -- ${cur}))
      |elif [[ ${prev} =~ ${pathOrStdOpts} ]]; then
      |  COMPREPLY=($(compgen -f -W "-" -- ${cur}))
      |elif [[ ${prev} =~ ${stringOpts} ]]; then
      |  COMPREPLY=($(compgen -f -- ${cur}))
      |elif [[ ${prev} =~ ${backendOpts} ]]; then
      |  COMPREPLY=($(compgen -W "%s" -- ${cur}))
      |elif [[ ${prev} =~ ${languageOpts} ]]; then
      |  COMPREPLY=($(compgen -W "%s" -- ${cur}))
      |else
      |  COMPREPLY=($(compgen -f -W "%s" -- ${cur}))
      |fi
      |
      |return 0
      |}
      |
      |complete -F __vercors vercors
      |""".stripMargin.format(
        pathOpts.mkString("|"),
        pathOrStdOpts.mkString("|"),
        stringOpts.mkString("|"),
        backendOpts.mkString("|"),
        languageOpts.mkString("|"),
        unknownOpts.mkString("|"),
        Backend.options.keys.mkString(" "),
        ReadLanguage.options.keys.mkString(" "),
        (pathOpts ++ pathOrStdOpts ++ stringOpts ++ backendOpts ++ languageOpts ++ unknownOpts).mkString(" "),
      ).strip()
    )
  }
}
