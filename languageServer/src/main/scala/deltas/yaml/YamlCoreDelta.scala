package deltas.yaml

import core.bigrammar.grammars._
import core.bigrammar.{BiGrammar, BiGrammarToParser}
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node.{GrammarKey, NodeField, NodeShape}
import core.parsers.editorParsers.DefaultCache
import deltas.expression.{ArrayLiteralDelta, ExpressionDelta}

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
  class IfContextParser(inners: Map[YamlContext, BiGrammarToParser.EditorParser[Result]]) extends EditorParserBase[Result] {
    override def parseInternal(input: Reader) = {
      val context: YamlContext = input.state.getOrElse(ContextKey, BlockOut).asInstanceOf[YamlContext]
      inners(context).parseInternal(input)
    }

    override def getDefault(cache: DefaultCache) = {
      inners.values.flatMap(inner => inner.getDefault(cache)).headOption
    }

    override def children = inners.values.toList
  }

  class WithContextParser[Result](update: YamlContext => YamlContext, inner: EditorParser[Result]) extends EditorParserBase[Result] {
    override def parseInternal(input: Reader) = {
      val context: YamlContext = input.state.getOrElse(ContextKey, BlockOut).asInstanceOf[YamlContext]
      val result = inner.parseInternal(input.withState(input.state + (ContextKey -> update(context))))
      result.updateRemainder(r => r.withState(r.state + (ContextKey -> context)))
    }

    override def getDefault(cache: DefaultCache) = inner.getDefault(cache)

    override def children = List(inner)
  }

  object IndentationSensitiveExpression extends GrammarKey

  override def transformGrammars(_grammars: LanguageGrammars, language: Language): Unit = {
    val grammars = _grammars
    import _grammars._
    val tag: BiGrammar = "!" ~> RegexGrammar(s"""[^'\n !${PlainScalarDelta.flowIndicatorChars}]+""".r) //Should be 	ns-uri-char - “!” - c-flow-indicator

    val blockValue = create(IndentationSensitiveExpression)
    blockValue.addAlternative(tag.as(TagName) ~ blockValue.as(TagNode) asNode TaggedNode)

    val flowValue = find(ExpressionDelta.FirstPrecedenceGrammar)
    val taggedFlowValue = tag.as(TagName) ~ flowValue.as(TagNode) asNode TaggedNode
    flowValue.addAlternative(taggedFlowValue)

    val originalBracketArray = find(ArrayLiteralDelta.Shape).inner
    find(ArrayLiteralDelta.Shape).inner = new WithContext(_ => FlowIn, originalBracketArray)

    blockValue.addAlternative(flowValue)

    grammars.bodyGrammar.inner = blockValue
  }

  override def description = "Adds the YAML language"

  override def dependencies = Set(ExpressionDelta)

}
