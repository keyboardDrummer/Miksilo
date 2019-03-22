package deltas.yaml

import core.bigrammar.BiGrammar
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import deltas.expression.{ArrayLiteralDelta, ExpressionDelta}
import deltas.json.JsonObjectLiteralDelta

object YamlArrayDelta extends DeltaWithGrammar {
  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    val _grammars = grammars
    import grammars._

    val blockValue = find(YamlCoreDelta.BlockValueGrammar)
    val blockArray: BiGrammar = {
      val element = keyword("- ") ~> CheckIndentationGrammar.greaterThan(blockValue)
      CheckIndentationGrammar.aligned(_grammars, element).as(ArrayLiteralDelta.Members).asNode(ArrayLiteralDelta.Shape)
    }
    blockValue.addAlternative(blockArray)
  }

  override def description = "Adds the indentation sensitive literal array"

  override def dependencies = Set(YamlCoreDelta)
}

object YamlObjectDelta extends DeltaWithGrammar {
  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    val _grammars = grammars
    import grammars._

    val blockValue = find(YamlCoreDelta.BlockValueGrammar)
    val flowValue = find(ExpressionDelta.FirstPrecedenceGrammar)

    lazy val blockMap: BiGrammar = {
      val member = new WithContext(_ =>
        BlockKey, flowValue).as(JsonObjectLiteralDelta.MemberKey) ~< keyword(":") ~
        CheckIndentationGrammar.greaterThan(blockValue.as(JsonObjectLiteralDelta.MemberValue)) asNode JsonObjectLiteralDelta.MemberShape
      CheckIndentationGrammar.aligned(_grammars, member).as(JsonObjectLiteralDelta.Members).asNode(JsonObjectLiteralDelta.Shape)
    }
    blockValue.addAlternative(blockMap)
  }

  override def description = "Adds the indentation sensitive literal object"

  override def dependencies = Set(YamlCoreDelta)
}

