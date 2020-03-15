package miksilo.modularLanguages.deltas.bytecode.constants

import miksilo.modularLanguages.core.bigrammar.BiGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.node.{Node, NodeField, NodeShape}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.deltas.bytecode.ByteCodeSkeleton
import miksilo.modularLanguages.deltas.bytecode.PrintByteCode._
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.ConstantPoolIndexGrammar

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
