package miksilo.modularLanguages.deltas.yaml

import miksilo.editorParser.parsers.core.TextPointer
import miksilo.editorParser.parsers.editorParsers.History
import miksilo.languageServer.core.language.Language
import miksilo.modularLanguages.core.bigrammar.grammars.{Delimiter, RegexGrammar}
import miksilo.modularLanguages.core.bigrammar.{BiGrammar, BiGrammarToParser}
import miksilo.modularLanguages.core.deltas.DeltaWithGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.node.{GrammarKey, NodeField, NodeShape}
import miksilo.modularLanguages.deltas.expression.{ArrayLiteralDelta, ExpressionDelta}
import miksilo.modularLanguages.deltas.json.{JsonObjectLiteralDelta, JsonStringLiteralDelta}

trait YamlContext
object FlowIn extends YamlContext{
  override def toString = "FlowIn"
}
object FlowOut extends YamlContext{
  override def toString = "FlowOut"
}
object BlockIn extends YamlContext{
  override def toString = "BlockIn"
}
object BlockOut extends YamlContext{
  override def toString = "BlockOut"
}
object BlockKey extends YamlContext{
  override def toString = "Block"
}
object FlowKey extends YamlContext{
  override def toString = "Flow"
}

object YamlCoreDelta extends DeltaWithGrammar {
  import miksilo.modularLanguages.core.bigrammar.BiGrammarToParser._

  object TaggedNode extends NodeShape
  object TagName extends NodeField
  object TagNode extends NodeField

  object ContextKey {
    override def toString = "Context"
  }

  class IfContextParser(inners: Map[YamlContext, BiGrammarToParser.Parser[Result]])
    extends ParserBuilderBase[Result] {

    override def getParser(recursive: BiGrammarToParser.GetParser): BuiltParser[Result] = {
      val innerParsers = inners.view.mapValues(p => recursive(p)).toMap

      new BuiltParser[Result] {
        override def apply(position: TextPointer, state: MyState, fixPointState: BiGrammarToParser.FixPointState): BiGrammarToParser.ParseResult[Result] = {
          val context: YamlContext = state.state.getOrElse(ContextKey, BlockOut).asInstanceOf[YamlContext]
          innerParsers(context)(position, state, fixPointState)
        }

        override def origin: Option[BiGrammarToParser.ParserBuilder[Result]] = Some(IfContextParser.this)
      }
    }

    override def leftChildren = children

    override def getMustConsume(cache: BiGrammarToParser.ConsumeCache) = inners.values.forall(i => cache(i))

    override def children = inners.values.toList
  }

  class WithContextParser[Result](update: YamlContext => YamlContext, val original: Parser[Result])
    extends ParserBuilderBase[Result] with ParserWrapper[Result] {

    override def getParser(recursive: BiGrammarToParser.GetParser) = {
      val parseOriginal = recursive(original)

      new BuiltParser[Result] {
        override def apply(position: TextPointer, state: MyState, fixPointState: BiGrammarToParser.FixPointState): BiGrammarToParser.ParseResult[Result] = {
          val context: YamlContext = state.state.getOrElse(ContextKey, BlockOut).asInstanceOf[YamlContext]
          val result = parseOriginal(position, MyState(state.state + (ContextKey -> update(context))), fixPointState)
          result.updateRemainder((p, s) => (p, MyState(s.state + (ContextKey -> context))))
        }

        override def origin: Option[BiGrammarToParser.ParserBuilder[Result]] = Some(WithContextParser.this)
      }
    }
  }

  object IndentationSensitiveExpression extends GrammarKey
  object BlockValue extends GrammarKey

  override def transformGrammars(_grammars: LanguageGrammars, language: Language): Unit = {
    val grammars = _grammars
    import _grammars._

    //Should be 	ns-uri-char - “!” - c-flow-indicator
    val tag: BiGrammar = JsonStringLiteralDelta.dropPrefix(grammars,
        RegexGrammar(s"""![^'\r\n !${PlainScalarDelta.flowIndicatorChars}]+""".r, "tag name", penaltyOption = Some(History.failPenalty),
                     defaultValue = Some("!")), TagName, "!")

    val flowValue = find(ExpressionDelta.FirstPrecedenceGrammar)
    val taggedFlowValue = tag ~ flowValue.as(TagNode) asLabelledNode TaggedNode
    flowValue.addAlternative(taggedFlowValue)
    val flowInBlock = new WithContext(_ => FlowOut, flowValue)

    val blockInBlock = create(IndentationSensitiveExpression)
    val indentationTag = tag ~ CheckIndentationGrammar.greaterThan(blockInBlock.as(TagNode)) asLabelledNode TaggedNode
    val blockValue = create(BlockValue, flowInBlock | blockInBlock | indentationTag)

    val originalBracketArray = find(ArrayLiteralDelta.Shape).inner
    find(ArrayLiteralDelta.Shape).inner = new WithContext(_ => FlowIn, originalBracketArray)
    val jsonObjectGrammar = find(JsonObjectLiteralDelta.Shape)
    jsonObjectGrammar.inner = new WithContext({
      case FlowOut => FlowIn
      case BlockKey => FlowKey
      case FlowIn => FlowIn
      case FlowKey => FlowKey
    }, jsonObjectGrammar.inner)

    grammars.bodyGrammar.inner = Delimiter("---").option ~> blockValue
  }

  override def description = "Adds the YAML language"

  override def dependencies = Set(ArrayLiteralDelta, ExpressionDelta)

}