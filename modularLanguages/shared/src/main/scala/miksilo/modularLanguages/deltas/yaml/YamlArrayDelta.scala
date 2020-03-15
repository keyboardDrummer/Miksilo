package miksilo.modularLanguages.deltas.yaml

import miksilo.modularLanguages.core.bigrammar.BiGrammar
import miksilo.modularLanguages.core.deltas.DeltaWithGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.languageServer.core.language.Language
import miksilo.modularLanguages.core.node.{GrammarKey, NodeField}
import miksilo.modularLanguages.deltas.expression.ArrayLiteralDelta

object YamlArrayDelta extends DeltaWithGrammar {

  object Grammar extends GrammarKey

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    val _grammars = grammars
    import grammars._

    val blockValue = find(YamlCoreDelta.BlockValue)
    val blockArray: BiGrammar = {
      val element = keywordGrammar("- ") ~> CheckIndentationGrammar.greaterThan(blockValue)
      CheckIndentationGrammar.aligned(_grammars, element).as(ArrayLiteralDelta.Members).asLabelledNode(ArrayLiteralDelta.Shape)
    }
    find(YamlCoreDelta.IndentationSensitiveExpression).addAlternative(create(Grammar, blockArray))
  }

  override def description = "Adds the indentation sensitive literal array"

  override def dependencies = Set(YamlCoreDelta)
}

