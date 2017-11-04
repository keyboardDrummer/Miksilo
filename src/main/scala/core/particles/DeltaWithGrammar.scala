package core.particles

import core.bigrammar.BiGrammarToGrammar.WithMap
import core.bigrammar.{MapGrammar, _}
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

class GrammarWithTrivia(val grammar: BiGrammar)(implicit grammars: GrammarCatalogue) extends NodeGrammarWriter
  with BiGrammarSequenceMethodsExtension
{
  def asLabelledNode(key: NodeClass): Labelled = grammars.create(key, new GrammarForAst(grammar).asNode(key))

  def manyVertical = new ManyVertical(new WithTrivia(grammar, grammars.trivia))

  def ~(other: BiGrammar) = new Sequence(grammar, new WithTrivia(other, grammars.trivia))

  def many = new ManyHorizontal(new WithTrivia(grammar, grammars.trivia))

  def %(bottom: BiGrammar) = new TopBottom(grammar, new WithTrivia(bottom, grammars.trivia))

  override implicit def addSequenceMethods(grammar: BiGrammar): GrammarWithTrivia = new GrammarWithTrivia(grammar)
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
