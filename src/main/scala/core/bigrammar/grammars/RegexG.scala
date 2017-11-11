package core.bigrammar.grammars

import scala.util.matching.Regex

case class RegexG(regex: Regex) extends FromStringGrammar(core.grammar.RegexG(regex))
