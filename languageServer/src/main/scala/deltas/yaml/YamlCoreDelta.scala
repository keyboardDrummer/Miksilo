package deltas.yaml

import core.bigrammar.grammars._
import core.bigrammar.{BiGrammar, BiGrammarToParser}
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node.{GrammarKey, NodeField, NodeShape}
import core.parsers.editorParsers.DefaultCache
import deltas.expression.{ArrayLiteralDelta, ExpressionDelta}
import deltas.json.StringLiteralDelta

trait YamlContext
object FlowIn extends YamlContext
object FlowOut extends YamlContext
object BlockIn extends YamlContext
object BlockOut extends YamlContext
object BlockKey extends YamlContext
object FlowKey extends YamlContext

object YamlCoreDelta extends DeltaWithGrammar {
  import BiGrammarToParser._

  object TaggedNode extends NodeShape
  object TagName extends NodeField
  object TagNode extends NodeField

  object ContextKey
  class IfContextParser(inners: Map[YamlContext, BiGrammarToParser.EditorParser[Result]])
    extends EditorParserBase[Result] {

    override def getParser(recursive: BiGrammarToParser.GetParse): Parse[Result] = {
      val innerParsers = inners.mapValues(p => recursive(p))

      def apply(input: Reader, errorAllowance: Int) = {
        val context: YamlContext = input.state.getOrElse(ContextKey, BlockOut).asInstanceOf[YamlContext]
        innerParsers(context)(input, errorAllowance)
      }

      apply
    }

    override def getDefault(cache: DefaultCache) = {
      inners.values.flatMap(inner => inner.getDefault(cache)).headOption
    }

    override def leftChildren = children

    override def getMustConsume(cache: BiGrammarToParser.ConsumeCache) = inners.values.forall(i => cache(i))

    override def children = inners.values.toList
  }

  class WithContextParser[Result](update: YamlContext => YamlContext, val original: EditorParser[Result])
    extends EditorParserBase[Result] with ParserWrapper[Result] {

    override def getParser(recursive: BiGrammarToParser.GetParse) = {
      val parseOriginal = recursive(original).asInstanceOf[Parse[Result]]

      def apply(input: Reader, errorAllowance: Int) = {
        val context: YamlContext = input.state.getOrElse(ContextKey, BlockOut).asInstanceOf[YamlContext]
        val result = parseOriginal(input.withState(input.state + (ContextKey -> update(context))), errorAllowance)
        result.updateRemainder(r => r.withState(r.state + (ContextKey -> context)))
      }

      apply
    }

    override def getDefault(cache: DefaultCache) = original.getDefault(cache)
  }

  object IndentationSensitiveExpression extends GrammarKey

  override def transformGrammars(_grammars: LanguageGrammars, language: Language): Unit = {
    val grammars = _grammars
    import _grammars._
    val tag: BiGrammar = StringLiteralDelta.dropPrefix(RegexGrammar(s"""![^'\n !${PlainScalarDelta.flowIndicatorChars}]+""".r),
      TagName, "!") //Should be 	ns-uri-char - “!” - c-flow-indicator

    val blockValue = create(IndentationSensitiveExpression)
    blockValue.addAlternative(tag ~ blockValue.as(TagNode) asNode TaggedNode)

    val flowValue = find(ExpressionDelta.FirstPrecedenceGrammar)
    val taggedFlowValue = tag ~ flowValue.as(TagNode) asNode TaggedNode
    flowValue.addAlternative(taggedFlowValue)

    val originalBracketArray = find(ArrayLiteralDelta.Shape).inner
    find(ArrayLiteralDelta.Shape).inner = new WithContext(_ => FlowIn, originalBracketArray)

    blockValue.addAlternative(flowValue)

    grammars.bodyGrammar.inner = blockValue
  }

  override def description = "Adds the YAML language"

  override def dependencies = Set(ExpressionDelta)

}
