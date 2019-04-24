package deltas.yaml

import core.bigrammar.BiGrammar
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import deltas.expression.ExpressionDelta
import deltas.json.{JsonObjectLiteralDelta, StringLiteralDelta}

object YamlObjectDelta extends DeltaWithGrammar {

  import JsonObjectLiteralDelta._
  import CheckIndentationGrammar._

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    val _grammars = grammars
    import grammars._

    val blockValue = find(YamlCoreDelta.IndentationSensitiveExpression)
    val flowValue = find(ExpressionDelta.FirstPrecedenceGrammar)

    lazy val blockMap: BiGrammar = {
      val member = new WithContext(_ => BlockKey, flowValue).as(MemberKey) ~< ":" ~
        greaterThan(blockValue.as(MemberValue)) asNode MemberShape

      aligned(_grammars, member).as(Members).asNode(Shape)
    }
    blockValue.addAlternative(blockMap)
  }

  override def description = "Adds the indentation sensitive literal object"

  override def dependencies = Set(YamlCoreDelta)
}