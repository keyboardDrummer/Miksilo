package miksilo.modularLanguages.deltas.yaml

import miksilo.modularLanguages.core.bigrammar.BiGrammar
import miksilo.modularLanguages.core.deltas.DeltaWithGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.languageServer.core.language.Language
import miksilo.modularLanguages.deltas.expression.ExpressionDelta
import miksilo.modularLanguages.deltas.json.JsonObjectLiteralDelta

object YamlObjectDelta extends DeltaWithGrammar {

  import JsonObjectLiteralDelta._
  import CheckIndentationGrammar._

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    val _grammars = grammars
    import grammars._

    val blockValue = find(YamlCoreDelta.BlockValue)
    val flowValue = find(ExpressionDelta.FirstPrecedenceGrammar)

    lazy val blockMap: BiGrammar = {
      val member = new WithContext(_ => BlockKey, flowValue).as(MemberKey) ~< ":" ~
        (greaterThan(blockValue.as(MemberValue)) |
          equal(find(YamlArrayDelta.Grammar).as(MemberValue))) asLabelledNode MemberShape

      aligned(_grammars, member).as(Members).asLabelledNode(Shape)
    }
    find(YamlCoreDelta.IndentationSensitiveExpression).addAlternative(blockMap)
  }

  override def description = "Adds the indentation sensitive literal object"

  override def dependencies = Set(YamlArrayDelta, YamlCoreDelta)
}