package core.bigrammar.grammars

import scala.util.matching.Regex

class RegexGrammar(val regex: Regex, verifyWhenPrinting: Boolean = false)
  extends FromStringGrammar(core.grammar.RegexG(regex), verifyWhenPrinting)
