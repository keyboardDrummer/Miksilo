package transformations.bytecode.constants

import core.bigrammar.BiGrammar
import core.particles.CompilationState
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node}
import transformations.bytecode.PrintByteCode._

object MethodTypeConstant extends ConstantEntry {

  object MethodTypeConstantKey extends Key
  object MethodTypeDescriptorIndex extends Key
  override def key = MethodTypeConstantKey

  def construct(descriptorIndex: Int) = new Node(MethodTypeConstantKey, MethodTypeDescriptorIndex -> descriptorIndex)

  override def getByteCode(constant: Node, state: CompilationState): Seq[Byte] = {
    byteToBytes(16) ++ shortToBytes(constant(MethodTypeDescriptorIndex).asInstanceOf[Int])
  }

  override def getConstantEntryGrammar(grammars: GrammarCatalogue): BiGrammar = ("method type constant:" ~~> integer).
    asNode(MethodTypeConstantKey, MethodTypeDescriptorIndex)

  override def description: String = "Add the method type constant"
}
