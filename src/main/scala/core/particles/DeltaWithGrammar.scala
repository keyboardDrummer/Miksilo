package core.particles

import core.bigrammar.BiGrammarToGrammar.WithMap
import core.bigrammar.{MapGrammar, _}
import core.document.{BlankLine, WhiteSpace}
import core.grammar.~
import core.particles.grammars.GrammarCatalogue
import core.particles.node._

object NodeGrammar {

  //noinspection ComparingUnrelatedTypes
  def destruct(withMap: WithMapG[Any], key: NodeClass): Option[WithMapG[Any]] = {
    val value = withMap.value
    if (!value.isInstanceOf[NodeLike])
      return None

    val node = value.asInstanceOf[NodeLike]

    if (node.clazz == key) {
      val dataViewAsGenericMap = node.dataView.map(t => (t._1.asInstanceOf[Any], t._2))
      Some(WithMapG(UndefinedDestructuringValue, dataViewAsGenericMap)) //TODO The withMap.state ++ is inconsistent with the construct method. Consistent would be to check that withMap.state is empty.
    }
    else {
      None
    }
  }

  case class ValueNotFound(meta: NodeLike, field: Any)

  def construct(withMap: WithMap, key: NodeClass): WithMap = {
    val result = new Node(key)
    result.data ++= withMap.map.collect { case (k: NodeField,v) => (k,v) }
    WithMapG(result, Map.empty)
  }
}

class NodeGrammar(inner: BiGrammar, val key: NodeClass)
  extends MapGrammar(inner,
    input => NodeGrammar.construct(input.asInstanceOf[WithMap], key),
    obj => NodeGrammar.destruct(obj.asInstanceOf[WithMapG[Any]], key), showMap = true)
{
}

class GrammarForAst(grammar: BiGrammar) {
  def asNode(key: NodeClass) = new NodeGrammar(grammar, key)
  def as(field: NodeField) = As(grammar, field)
}

class GrammarWithTrivia(grammar: BiGrammar)(implicit grammars: GrammarCatalogue) extends NodeGrammarWriter
{
  def asLabelledNode(key: NodeClass): Labelled = grammars.create(key, new GrammarForAst(grammar).asNode(key))
  implicit def wrap(grammar: BiGrammar): GrammarWithTrivia = new GrammarWithTrivia(grammar)

  def indent(width: Int = 2) = new GrammarWithTrivia(WhiteSpace(width, 0)) ~> grammar

  def ~<(right: BiGrammar) = (this ~ right).ignoreRight

  def ~~<(right: BiGrammar) = this ~< (space ~ right)

  def manySeparated(separator: BiGrammar): BiGrammar = someSeparated(separator) | ValueGrammar(Seq.empty[Any])

  def ~~(right: BiGrammar): BiGrammar = {
    (this ~< space) ~ right
  }

  def someSeparatedVertical(separator: BiGrammar): BiGrammar =
    someMap(this % (separator %> grammar).manyVertical)

  def manyVertical = new ManyVertical(WithTrivia(grammar, grammars.trivia))

  def manySeparatedVertical(separator: BiGrammar): BiGrammar = someSeparatedVertical(separator) | ValueGrammar(Seq.empty[Node])

  def some: BiGrammar = someMap(grammar ~ (grammar*))
  def someSeparated(separator: BiGrammar): BiGrammar = someMap(this ~ ((separator ~> grammar) *))

  private def someMap(grammar: BiGrammar): BiGrammar = {
    grammar ^^
      ( {
        case first ~ rest => Seq(first) ++ rest.asInstanceOf[Seq[Any]]
      }, {
        case seq: Seq[Any] => if (seq.nonEmpty) Some(core.grammar.~(seq.head, seq.tail)) else None
      })
  }
  def inParenthesis = ("(": BiGrammar) ~> grammar ~< ")"

  def ~(other: BiGrammar) = new Sequence(grammar, WithTrivia(other, grammars.trivia))

  def ~>(right: BiGrammar): BiGrammar = (this ~ right).ignoreLeft

  def ~~>(right: BiGrammar) = (this ~ space) ~> right

  def * = new ManyHorizontal(WithTrivia(grammar, grammars.trivia))
  def many = this*

  def %(bottom: BiGrammar) = new TopBottom(grammar, WithTrivia(bottom, grammars.trivia))

  def %%(bottom: BiGrammar): BiGrammar = {
    (this %< BlankLine) % bottom
  }

  def %>(bottom: BiGrammar) = (this % bottom).ignoreLeft

  def %<(bottom: BiGrammar) = (this % bottom).ignoreRight
}

trait NodeGrammarWriter extends BiGrammarWriter {

  implicit def grammarAsRoot(grammar: BiGrammar): RootGrammar = new RootGrammar(grammar)
  implicit val postfixOps = language.postfixOps
  implicit def toAstGrammar(grammar: BiGrammar): GrammarForAst = new GrammarForAst(grammar)
}

trait DeltaWithGrammar extends Delta with NodeGrammarWriter {

  def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit

  override def inject(state: Language): Unit = {
    super.inject(state)
    transformGrammars(state.grammarCatalogue, state)
  }

}
