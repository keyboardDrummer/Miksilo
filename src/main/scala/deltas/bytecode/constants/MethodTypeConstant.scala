package deltas.bytecode.constants

import core.bigrammar.BiGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeField, NodeShape}
import core.language.Language
import deltas.bytecode.ByteCodeSkeleton
import deltas.bytecode.PrintByteCode._
import deltas.bytecode.coreInstructions.ConstantPoolIndexGrammar

object MethodTypeConstant extends ConstantEntry {

  object MethodTypeConstantKey extends NodeShape
  object MethodTypeDescriptorIndex extends NodeField
  override def key = MethodTypeConstantKey

  def construct(descriptorIndex: Int) = new Node(MethodTypeConstantKey, MethodTypeDescriptorIndex -> descriptorIndex)

  override def getByteCode(constant: Node, state: Language): Seq[Byte] = {
    byteToBytes(16) ++ shortToBytes(constant(MethodTypeDescriptorIndex).asInstanceOf[Int])
  }

  override def inject(state: Language): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getRegistry(state).constantReferences.put(key, Map(MethodTypeDescriptorIndex -> Utf8ConstantDelta.key))
  }

  override def getConstantEntryGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    find(ConstantPoolIndexGrammar).as(MethodTypeDescriptorIndex)
  }

  override def description: String = "Add the method type constant"

  override def getName = "MethodType"
}
