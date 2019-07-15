package deltas.yaml

import core.bigrammar.BiGrammar
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node.NodeField
import deltas.expression.ArrayLiteralDelta

object YamlArrayDelta extends DeltaWithGrammar {
  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    val _grammars = grammars
    import grammars._

    val blockValue = find(YamlCoreDelta.BlockValue)
    val blockArray: BiGrammar = {
      val element = keywordGrammar("- ") ~> CheckIndentationGrammar.greaterThan(blockValue)
      CheckIndentationGrammar.aligned(_grammars, element).as(ArrayLiteralDelta.Members).asLabelledNode(ArrayLiteralDelta.Shape)
    }
    find(YamlCoreDelta.IndentationSensitiveExpression).addAlternative(blockArray)
  }

  override def description = "Adds the indentation sensitive literal array"

  override def dependencies = Set(YamlCoreDelta)
}

