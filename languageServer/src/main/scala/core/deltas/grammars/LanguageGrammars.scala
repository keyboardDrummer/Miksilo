package core.deltas.grammars

import core.bigrammar.BiGrammarToParser.mergeNamedValues
import core.bigrammar._
import core.bigrammar.grammars._
import core.deltas.NodeGrammarWriter
import core.deltas.grammars.LanguageGrammars.WithMapGrammar
import core.document.Document
import core.language.node._
import core.responsiveDocument.ResponsiveDocument
import languageServer.SourceRange
import util.Utility

import scala.reflect.ClassTag

case class KeyGrammar(key: Key) extends GrammarKey
{
  override lazy val toString = key.toString
}

object TriviasGrammar extends GrammarKey
object TriviaGrammar extends GrammarKey
object BodyGrammar extends GrammarKey
object ProgramGrammar extends GrammarKey

object LanguageGrammars {
  type WithMapGrammar[Value] = BiGrammar[WithMap[Value]]
}

class LanguageGrammars extends GrammarCatalogue with NodeGrammarWriter with AbstractGrammarWriter[WithMapGrammar] {

  class NamedValuesGrammar(grammar: BiGrammar[WithMap[Unit]], grammars: LanguageGrammars) {
    def asNode(key: NodeShape) = new NodeGrammar(grammar, key)
    def asLabelledNode(key: NodeShape): Labelled[Node] = grammars.create(key, asNode(key))
  }

  implicit class AsExtension[T](grammar: BiGrammar[T]) {
    def as(field: NodeField, changePosition: SourceRange => SourceRange = null) = As(grammar, field, changePosition)
  }

  val trivia: Labelled[WithMap[Unit]] =
    create(TriviasGrammar, new ManyUnit(create(TriviaGrammar, ParseWhiteSpace), parseGreedy = false, horizontal = false))

  val bodyGrammar: BiGrammar[WithMap[_]] = create(BodyGrammar, BiFailure[WithMap[Any]]())
  create(ProgramGrammar, WithTrivia(bodyGrammar ~< trivia, trivia)) //TODO Move this, bodyGrammar and trivia to a separate Delta.

  def root: Labelled[WithMap[_]] = findMap(ProgramGrammar)

  def addTriviaIfUseful[Value](grammar: BiGrammar[WithMap[Value]], horizontal: Boolean): BiGrammar[WithMap[Value]] =
    if (grammar.containsParser() && grammar != trivia)
      WithTrivia(grammar, trivia, horizontal)
    else
      grammar

  override def map2[Value, NewValue: ClassTag](grammar: WithMapGrammar[Value],
                                               afterParsing: Value => NewValue,
                                               beforePrinting: NewValue => Option[Value]) =
  new MapGrammar[WithMap[Value], WithMap[NewValue]](grammar,
    w => Right(WithMap(afterParsing(w.value),w.namedValues)),
    w => beforePrinting(w.value).map(v => WithMap(v, w.namedValues)))

  override def choice2[Value](grammar: WithMapGrammar[Value], other: WithMapGrammar[Value], firstIsLonger: Boolean) =
    new BiChoice(grammar, other, firstIsLonger)

  override def sequence2[Value, Other, Result](grammar: WithMapGrammar[Value],
                                               other: WithMapGrammar[Other],
                                               bijective: SequenceBijective[Value, Other, Result],
                                               horizontal: Boolean) = {
    new BiSequence(grammar, addTriviaIfUseful(other, horizontal), withMapBijective(bijective), horizontal)
  }

  def withMapBijective[Left, Right, Result](original: SequenceBijective[Left, Right, Result]):
    SequenceBijective[WithMap[Left], WithMap[Right], WithMap[Result]] = {
      SequenceBijective((firstResult, secondResult) => {
        val result = original.construct(firstResult.value, secondResult.value)
        val resultMap = Utility.mergeMaps(firstResult.namedValues, secondResult.namedValues, mergeNamedValues)
        WithMap(result, resultMap)
      }, withMap =>
        original.destruct(withMap.value).map(v =>
          (WithMap(v._1, withMap.namedValues), WithMap(v._2, withMap.namedValues))))
  }


  override def many2[Value](grammar: WithMapGrammar[Value], horizontal: Boolean) = {
    val wrappedGrammar = addTriviaIfUseful(grammar, horizontal)
    val base = if (horizontal) new ManyHorizontal(wrappedGrammar) else new ManyVertical(wrappedGrammar)
    base.map(withMaps => {
      withMaps.foldRight[WithMap[List[Value]]](WithMap(List.empty[Value], Map.empty[Any, Any]))(
        (a: WithMap[Value], b: WithMap[List[Value]]) => WithMap(a.value :: b.value, a.namedValues ++ b.namedValues))
    }, withMap => withMap.value.map(singleValue => WithMap(singleValue, withMap.namedValues)))
  }

  implicit def stringToGrammar2(value: String): UnitGrammarExtension = UnitGrammarExtension(Keyword(value))
  override implicit def stringToGrammar(value: String): WithMapGrammar[Unit] = Keyword(value)

  override implicit def wrapBiGrammar[T](grammar: BiGrammar[T]): WithMapGrammar[T] =
    grammar.map(v => WithMap(v, Map.empty), v => v.value)

  implicit def grammarToNamedValuesGrammar(value: BiGrammar[WithMap[Unit]]): NamedValuesGrammar =
    new NamedValuesGrammar(value, this)

  implicit def bla[Value](grammar: BiGrammar[WithMap[Value]]): ValueGrammarExtension[Value] = ValueGrammarExtension(grammar)
  implicit def bla2[Value](grammar: BiGrammar[Unit]): UnitGrammarExtension = UnitGrammarExtension(grammar)
}


