package transformations.bytecode.types

import core.bigrammar.{Keyword, BiGrammar}
import core.particles.CompilationState
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node}

object FloatTypeC extends TypeInstance
{
  object FloatTypeKey extends Key
  override val key: Key = FloatTypeKey
  val floatType = new Node(FloatTypeKey)

  override def getSuperTypes(_type: Node, state: CompilationState): Seq[Node] = ???

  override def getJavaGrammar(grammars: GrammarCatalogue): BiGrammar = "float" ~> produce(floatType)

  override def getByteCodeGrammar(grammars: GrammarCatalogue): BiGrammar = new Keyword("F",false) ~> produce(floatType)

  override def description: String = "Adds the float type."
}
