package transformations.bytecode.constants

import core.bigrammar.BiGrammar
import core.particles.CompilationState
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeClass, NodeField}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.coreInstructions.ConstantPoolIndexGrammar

object MethodTypeConstant extends ConstantEntry {

  object MethodTypeConstantKey extends NodeClass
  object MethodTypeDescriptorIndex extends NodeField
  override def key = MethodTypeConstantKey

  def construct(descriptorIndex: Int) = new Node(MethodTypeConstantKey, MethodTypeDescriptorIndex -> descriptorIndex)

  override def getByteCode(constant: Node, state: CompilationState): Seq[Byte] = {
    byteToBytes(16) ++ shortToBytes(constant(MethodTypeDescriptorIndex).asInstanceOf[Int])
  }

  override def inject(state: CompilationState): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getState(state).constantReferences.put(key, Map(MethodTypeDescriptorIndex -> Utf8Constant.key))
  }

  override def getConstantEntryGrammar(grammars: GrammarCatalogue): BiGrammar =
    grammars.find(ConstantPoolIndexGrammar).as(MethodTypeDescriptorIndex).asNode(MethodTypeConstantKey)

  override def description: String = "Add the method type constant"

  override def getName = "MethodType"
}
