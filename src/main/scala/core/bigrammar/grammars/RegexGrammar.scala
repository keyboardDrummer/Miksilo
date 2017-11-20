package core.bigrammar.grammars

import scala.util.matching.Regex

case class RegexGrammar(regex: Regex, verifyWhenPrinting: Boolean = false)
  extends FromStringGrammar(core.grammar.RegexG(regex), verifyWhenPrinting)
