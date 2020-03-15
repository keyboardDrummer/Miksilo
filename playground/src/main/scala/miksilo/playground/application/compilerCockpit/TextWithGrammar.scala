package miksilo.playground.application.compilerCockpit

import miksilo.modularLanguages.core.bigrammar.BiGrammar

case class TextWithGrammar(text: String, grammar: BiGrammar = null)
