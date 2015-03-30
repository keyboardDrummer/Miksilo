package transformations.types

import core.bigrammar.BiGrammar
import core.particles.grammars.GrammarCatalogue
import core.particles.CompilationState
import core.particles.node.{Key, Node}

object BooleanTypeC extends TypeInstance
  with StackType //TODO remove this and change VariablePool accordingly.
{
  override val key = BooleanTypeKey

  override def getSuperTypes(_type: Node, state: CompilationState): Seq[Node] = Seq.empty

  override def getByteCodeGrammar(grammars: GrammarCatalogue): BiGrammar = "Z" ~> produce(booleanType)

  override def getStackType(_type: Node, state: CompilationState): Node = IntTypeC.intType

  override def getJavaGrammar(grammars: GrammarCatalogue) = {
    "boolean" ~> produce(booleanType)
  }

  def booleanType = new Node(BooleanTypeKey)


  object BooleanTypeKey extends Key

  override def description: String = "Defines the boolean type."

  override def getStackSize: Int = IntTypeC.getStackSize
}
