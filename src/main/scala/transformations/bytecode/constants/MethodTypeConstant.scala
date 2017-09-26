package transformations.bytecode.constants

import core.bigrammar.BiGrammar
import core.particles.Language
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeField}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.coreInstructions.ConstantPoolIndexGrammar

object MethodTypeConstant extends ConstantEntry {

  object MethodTypeDescriptorIndex extends NodeField

  def construct(descriptorIndex: Int) = new Node(key, MethodTypeDescriptorIndex -> descriptorIndex)

  override def getByteCode(constant: Node, state: Language): Seq[Byte] = {
    byteToBytes(16) ++ shortToBytes(constant(MethodTypeDescriptorIndex).asInstanceOf[Int])
  }

  override def inject(state: Language): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getState(state).constantReferences.put(key, Map(MethodTypeDescriptorIndex -> Utf8Constant.key))
  }

  override def getConstantEntryGrammar(grammars: GrammarCatalogue): BiGrammar =
    grammars.find(ConstantPoolIndexGrammar).as(MethodTypeDescriptorIndex).asNode(key)

  override def description: String = "Add the method type constant"

  override def getName = "MethodType"
}
