package deltas.bytecode.constants

import core.deltas.grammars.LanguageGrammars
import core.language.node.{Node, NodeField, NodeShape}
import core.language.{Compilation, Language}
import deltas.bytecode.ByteCodeSkeleton
import deltas.bytecode.PrintByteCode._
import deltas.bytecode.coreInstructions.ConstantPoolIndexGrammar

object MethodTypeConstant extends ConstantPoolEntry {

  object MethodTypeConstantKey extends NodeShape
  object MethodTypeDescriptorIndex extends NodeField
  override def shape = MethodTypeConstantKey

  def construct(descriptorIndex: Int) = new Node(MethodTypeConstantKey, MethodTypeDescriptorIndex -> descriptorIndex)

  override def getBytes(compilation: Compilation, constant: Node): Seq[Byte] = {
    byteToBytes(16) ++ shortToBytes(constant(MethodTypeDescriptorIndex).asInstanceOf[Int])
  }

  override def inject(language: Language): Unit = {
    super.inject(language)
    ByteCodeSkeleton.constantReferences.add(language, shape, Map(MethodTypeDescriptorIndex -> Utf8ConstantDelta.shape))
  }

  override def getConstantEntryGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    find(ConstantPoolIndexGrammar).as(MethodTypeDescriptorIndex)
  }

  override def description: String = "Add the method type constant"

  override val getName = "MethodType"
}
