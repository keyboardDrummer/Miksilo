package transformations.bytecode.constants

import core.bigrammar.BiGrammar
import core.particles.grammars.GrammarCatalogue
import core.particles.CompilationState
import core.particles.node.Node
import transformations.bytecode.PrintByteCode
import transformations.bytecode.types.TypeSkeleton

object FieldDescriptorConstant extends ConstantEntry {
  object Key
  object Type

  def constructor(_type: Node) = new Node(Key, Type -> _type)

  override def key: Any = Key

  override def getByteCode(constant: Node, state: CompilationState): Seq[Byte] = {
    val _type: Node = constant(Type).asInstanceOf[Node]
    val typeString = TypeSkeleton.getByteCodeString(state)(_type)
    PrintByteCode.toUTF8ConstantEntry(typeString)
  }

  override def getConstantEntryGrammar(grammars: GrammarCatalogue): BiGrammar = {
    val typeGrammar = grammars.find(TypeSkeleton.JavaTypeGrammar)
    "field:" ~> typeGrammar ^^ parseMap(Key, Type)
  }

  override def description: String = "Adds the field descriptor constant. It contains the type of a field."
}
