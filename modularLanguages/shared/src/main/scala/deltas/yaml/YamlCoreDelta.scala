package deltas.yaml

import core.bigrammar.{BiGrammar, BiGrammarToParser}
import core.bigrammar.grammars.RegexGrammar
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node.{GrammarKey, NodeField, NodeShape}
import core.parsers.core.Container
import deltas.expression.{ArrayLiteralDelta, ExpressionDelta}
import deltas.json.JsonStringLiteralDelta

trait YamlContext
object FlowIn extends YamlContext
object FlowOut extends YamlContext
object BlockIn extends YamlContext
object BlockOut extends YamlContext
object BlockKey extends YamlContext
object FlowKey extends YamlContext

object YamlCoreDelta extends DeltaWithGrammar {
  import core.bigrammar.BiGrammarToParser._

  object TaggedNode extends NodeShape
  object TagName extends NodeField
  object TagNode extends NodeField

  object ContextKey
  class IfContextParser(inners: Map[YamlContext, BiGrammarToParser.Parser[Result]])
    extends ParserBuilderBase[Result] {

    override def getParser(textContainer: Container[ArrayCharSequence], recursive: BiGrammarToParser.GetParser): BuiltParser[Result] = {
      val innerParsers = inners.view.mapValues(p => recursive(p)).toMap

      def apply(input: Reader, state: ParseState) = {
        val context: YamlContext = input.state.getOrElse(ContextKey, BlockOut).asInstanceOf[YamlContext]
        innerParsers(context)(input, state)
      }

      apply
    }

    override def leftChildren = children

    override def getMustConsume(cache: BiGrammarToParser.ConsumeCache) = inners.values.forall(i => cache(i))

    override def children = inners.values.toList
  }

  class WithContextParser[Result](update: YamlContext => YamlContext, val original: Parser[Result])
    extends ParserBuilderBase[Result] with ParserWrapper[Result] {

    override def getParser(text: Container[ArrayCharSequence], recursive: BiGrammarToParser.GetParser) = {
      val parseOriginal = recursive(original)

      def apply(input: Reader, state: ParseState): ParseResult[Result] = {
        val context: YamlContext = input.state.getOrElse(ContextKey, BlockOut).asInstanceOf[YamlContext]
        val result = parseOriginal(input.withState(input.state + (ContextKey -> update(context))), state)
        result.updateRemainder(r => r.withState(r.state + (ContextKey -> context)))
      }

      apply
    }
  }

  object IndentationSensitiveExpression extends GrammarKey
  object BlockValue extends GrammarKey

  override def transformGrammars(_grammars: LanguageGrammars, language: Language): Unit = {
    val grammars = _grammars
    import _grammars._

    //Should be 	ns-uri-char - “!” - c-flow-indicator
    val tag: BiGrammar = JsonStringLiteralDelta.dropPrefix(grammars,
        RegexGrammar(s"""![^'\n !${PlainScalarDelta.flowIndicatorChars}]+""".r, "tag name",
          defaultValue = Some("!")), TagName, "!")

    val flowValue = find(ExpressionDelta.FirstPrecedenceGrammar)
    val taggedFlowValue = tag ~ flowValue.as(TagNode) asLabelledNode TaggedNode
    flowValue.addAlternative(taggedFlowValue)

    val indentationSensitive = create(IndentationSensitiveExpression)
    val indentationTag = tag ~ CheckIndentationGrammar.greaterThan(indentationSensitive.as(TagNode)) asLabelledNode TaggedNode
    val blockValue = create(BlockValue, flowValue | indentationSensitive | indentationTag)

    val originalBracketArray = find(ArrayLiteralDelta.Shape).inner
    find(ArrayLiteralDelta.Shape).inner = new WithContext(_ => FlowIn, originalBracketArray)

    grammars.bodyGrammar.inner = blockValue
  }

  override def description = "Adds the YAML language"

  override def dependencies = Set(ArrayLiteralDelta, ExpressionDelta)

}
