package transformations.javac.types

import core.bigrammar.BiGrammar
import core.particles.CompilationState
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node}
import transformations.bytecode.types.{IntTypeC, StackType, TypeInstance}

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
