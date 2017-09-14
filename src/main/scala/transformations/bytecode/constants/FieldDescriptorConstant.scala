package transformations.bytecode.constants

import core.bigrammar.BiGrammar
import core.particles.grammars.GrammarCatalogue
import core.particles.CompilationState
import core.particles.node.{Key, Node, NodeClass, NodeField}
import transformations.bytecode.PrintByteCode
import transformations.bytecode.types.TypeSkeleton

object FieldDescriptorConstant extends ConstantEntry {
  object MyKey extends NodeClass
  object Type extends NodeField

  def constructor(_type: Node) = new Node(MyKey, Type -> _type)

  override def key = MyKey

  override def getByteCode(constant: Node, state: CompilationState): Seq[Byte] = {
    val _type: Node = constant(Type).asInstanceOf[Node]
    val typeString = TypeSkeleton.getByteCodeString(state)(_type)
    PrintByteCode.toUTF8ConstantEntry(typeString)
  }

  override def getConstantEntryGrammar(grammars: GrammarCatalogue): BiGrammar = {
    val typeGrammar = grammars.find(TypeSkeleton.JavaTypeGrammar)
    "field:" ~> typeGrammar asNode(MyKey, Type)
  }

  override def description: String = "Adds the field descriptor constant. It contains the type of a field."
}
