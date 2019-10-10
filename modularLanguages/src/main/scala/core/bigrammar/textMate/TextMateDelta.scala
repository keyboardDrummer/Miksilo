package core.bigrammar.textMate

import core.bigrammar.grammars.Delimiter
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node.Node
import deltas.expression.ArrayLiteralDelta
import deltas.json.{JsonObjectLiteralDelta, JsonStringLiteralDelta}

import scala.util.matching.Regex

object TextMateDelta extends DeltaWithGrammar {
  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val objectLiteral = find(JsonObjectLiteralDelta.Shape)
    objectLiteral.find(p => p.value == Delimiter(":")).get.set(printSpace ~> Delimiter("="))
    objectLiteral.find(p => p.value == Delimiter(",")).get.set(Delimiter(";"))

    val arrayLiteral = find(ArrayLiteralDelta.Shape)
    arrayLiteral.find(p => p.value == Delimiter("[")).get.set(Delimiter("("))
    arrayLiteral.find(p => p.value == Delimiter("]")).get.set(Delimiter(")"))
  }

  override def description = "Introduces TextMate syntax"

  override def dependencies = Set(JsonObjectLiteralDelta, ArrayLiteralDelta)

  def singleMatch(name: String, regex: Regex): Node = {
    JsonObjectLiteralDelta.neww(Map(
      "name" -> JsonStringLiteralDelta.neww(name),
      "match" -> JsonStringLiteralDelta.neww(regex.toString())))
  }
}
