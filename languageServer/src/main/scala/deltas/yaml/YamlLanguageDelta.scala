package deltas.yaml

import core.bigrammar.grammars._
import core.bigrammar.printer.Printer.NodePrinter
import core.bigrammar.{BiGrammar, BiGrammarToParser}
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node.{GrammarKey, NodeField, NodeShape}
import core.parsers.editorParsers.DefaultCache
import core.responsiveDocument.ResponsiveDocument
import deltas.expression.{ArrayLiteralDelta, ExpressionDelta}
import deltas.json.{DoubleQuoteStringLiteralDelta, JsonObjectLiteralDelta}

trait YamlContext
object FlowIn extends YamlContext
object FlowOut extends YamlContext
object BlockIn extends YamlContext
object BlockOut extends YamlContext
object BlockKey extends YamlContext
object FlowKey extends YamlContext

object YamlLanguageDelta extends DeltaWithGrammar {
  import BiGrammarToParser._

  val nbChars = """\n"""
  val nsChars = nbChars + " "
  def flowIndicatorChars = """,\[\]{}"""

  object TaggedNode extends NodeShape
  object TagName extends NodeField
  object TagNode extends NodeField

  object ContextKey
  class IfContextParser(inners: Map[YamlContext, BiGrammarToParser.EditorParser[Result]]) extends EditorParser[Result] {
    override def parseInternal(input: Reader, state: ParseStateLike) = {
      val context: YamlContext = input.state.getOrElse(ContextKey, BlockOut).asInstanceOf[YamlContext]
      inners(context).parseInternal(input, state)
    }

    override def getDefault(cache: DefaultCache) = {
      inners.values.flatMap(inner => inner.getDefault(cache)).headOption
    }
  }

  class WithContextParser[Result](update: YamlContext => YamlContext, inner: EditorParser[Result]) extends EditorParser[Result] {
    override def parseInternal(input: Reader, state: ParseStateLike) = {
      val context: YamlContext = input.state.getOrElse(ContextKey, BlockOut).asInstanceOf[YamlContext]
      val result = inner.parseInternal(input.withState(input.state + (ContextKey -> update(context))), state)
      result.updateRemainder(r => r.withState(r.state + (ContextKey -> context)))
    }

    override def getDefault(cache: DefaultCache) = inner.getDefault(cache)
  }

  val contexts = Seq(BlockOut, BlockIn, BlockKey, FlowKey, FlowOut, FlowIn)

  class IfContext[Result](inners: Map[YamlContext, BiGrammar], printer: BiGrammar) extends CustomGrammar with BiGrammar {
    override def print(toDocumentInner: BiGrammar => ResponsiveDocument) = toDocumentInner(printer)

    override def createPrinter(recursive: BiGrammar => NodePrinter) = recursive(printer)

    override def toParser(recursive: BiGrammar => BiGrammarToParser.EditorParser[BiGrammarToParser.Result]) =
      new IfContextParser(inners.mapValues(recursive))

    override def children = contexts.flatMap(inners.get(_).toSeq)

    override def withChildren(newChildren: Seq[BiGrammar]) =
      new IfContext(contexts.filter(c => inners.contains(c)).zip(newChildren).toMap, newChildren.last)

    override def containsParser(recursive: BiGrammar => Boolean) = true
  }

  class WithContext[Result](update: YamlContext => YamlContext, inner: BiGrammar) extends CustomGrammar with BiGrammar {
    override def print(toDocumentInner: BiGrammar => ResponsiveDocument) = toDocumentInner(inner)

    override def createPrinter(recursive: BiGrammar => NodePrinter) = recursive(inner)

    override def toParser(recursive: BiGrammar => BiGrammarToParser.EditorParser[BiGrammarToParser.Result]) =
      new WithContextParser(update, recursive(inner))

    override def children = Seq(inner)

    override def withChildren(newChildren: Seq[BiGrammar]) = new WithContext(update, newChildren.head)

    override def containsParser(recursive: BiGrammar => Boolean) = true
  }

  object BlockValueGrammar extends GrammarKey

  override def transformGrammars(_grammars: LanguageGrammars, language: Language): Unit = {
    val grammars = _grammars
    import _grammars._
    val tag: BiGrammar = "!" ~> RegexGrammar(s"""[^'\n !$flowIndicatorChars]+""".r) //Should be 	ns-uri-char - “!” - c-flow-indicator

    val blockValue = new Labelled(BlockValueGrammar)
    blockValue.addAlternative(tag.as(TagName) ~ blockValue.as(TagNode) asNode TaggedNode)

    val blockArray: BiGrammar = {
      val element = keyword("- ") ~> CheckIndentationGrammar.greaterThan(blockValue)
      CheckIndentationGrammar.aligned(grammars, element).as(ArrayLiteralDelta.Members).asNode(ArrayLiteralDelta.Shape)
    }

    val plainSafeOutChars = s"""$nbChars#'"""
    val plainSafeInChars = s"""$plainSafeOutChars$flowIndicatorChars"""
    val doubleColonPlainSafeIn =  RegexGrammar(s"""([^$plainSafeInChars:]|:[^$plainSafeInChars ])+""".r)
    val doubleColonPlainSafeOut =  RegexGrammar(s"""([^$plainSafeOutChars:]|:[^$plainSafeOutChars ])+""".r)

    val nsPlainSafe: BiGrammar = new IfContext(Map(
      FlowIn -> doubleColonPlainSafeIn,
      FlowOut -> doubleColonPlainSafeOut,
      BlockKey -> doubleColonPlainSafeOut,
      FlowKey -> doubleColonPlainSafeIn), doubleColonPlainSafeOut)

    val plainStyleSingleLineString: BiGrammar = nsPlainSafe
    val plainStyleMultiLineString: BiGrammar = new BiSequence(new BiSequence(nsPlainSafe, grammars.trivia, BiSequence.ignoreRight, false),
      CheckIndentationGrammar.greaterThan(new WithIndentationGrammar(CheckIndentationGrammar.equal(nsPlainSafe).manySeparated("\n"))),
      SequenceBijective((firstLine: Any, rest: Any) => {
        firstLine.asInstanceOf[String] + rest.asInstanceOf[List[String]].fold("")((a,b) => a + " " + b)
      }, (value: Any) => Some(value, List.empty)), false)

    val plainScalar: BiGrammar = new WithContext({
      case FlowIn => FlowIn
      case BlockKey => BlockKey
      case FlowKey => FlowKey
      case _ => FlowOut
    }, plainStyleMultiLineString | plainStyleSingleLineString).
      as(DoubleQuoteStringLiteralDelta.Value).asNode(DoubleQuoteStringLiteralDelta.Shape)

    find(ExpressionDelta.FirstPrecedenceGrammar).addAlternative(plainScalar)

    val flowValue = find(ExpressionDelta.FirstPrecedenceGrammar)
    val untaggedFlowValue = flowValue.inner
    val taggedFlowValue = tag.as(TagName) ~ flowValue.as(TagNode) asNode TaggedNode
    flowValue.addAlternative(taggedFlowValue)

    lazy val blockMap: BiGrammar = {
      val member = new WithContext(_ =>
        BlockKey, flowValue).as(JsonObjectLiteralDelta.MemberKey) ~< keyword(":") ~
          CheckIndentationGrammar.greaterThan(blockValue.as(JsonObjectLiteralDelta.MemberValue)) asNode JsonObjectLiteralDelta.MemberShape
      CheckIndentationGrammar.aligned(grammars, member).as(JsonObjectLiteralDelta.Members).asNode(JsonObjectLiteralDelta.Shape)
    }

    val originalBracketArray = find(ArrayLiteralDelta.Shape).inner
    find(ArrayLiteralDelta.Shape).inner = new WithContext(_ => FlowIn, originalBracketArray)

    blockValue.addAlternative(untaggedFlowValue)
    blockValue.addAlternative(blockArray)
    blockValue.addAlternative(blockMap)

    grammars.bodyGrammar.inner = blockValue
  }

  override def description = "Adds the YAML language"

  override def dependencies = Set.empty

}
