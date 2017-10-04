package transformations.bytecode.constants

import core.bigrammar.BiGrammar
import core.particles.Language
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeClass, NodeField}
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.constants.MethodHandleConstant.MethodHandleKey

object InvokeDynamicConstant extends ConstantEntry {

  object InvokeDynamicKey extends NodeClass
  object InvokeDynamicBootstrapMethodIndex extends NodeField
  object InvokeDynamicNameAndTypeIndex extends NodeField

  def construct(kind: Int, index: Int) = new Node(InvokeDynamicKey, InvokeDynamicBootstrapMethodIndex -> kind, InvokeDynamicNameAndTypeIndex -> index)

  override def key = InvokeDynamicKey

  override def getByteCode(constant: Node, state: Language): Seq[Byte] = {
    byteToBytes(18) ++ byteToBytes(constant(InvokeDynamicBootstrapMethodIndex).asInstanceOf[Int]) ++ shortToBytes(constant(InvokeDynamicNameAndTypeIndex).asInstanceOf[Int])
  }

  override def getConstantEntryGrammar(grammars: GrammarCatalogue): BiGrammar = (("bootstrap index:" ~~> integer ~< ", nameAndTypeIndex:") ~~ integer).
    asNode(MethodHandleKey, InvokeDynamicBootstrapMethodIndex, InvokeDynamicNameAndTypeIndex)

  override def description: String = "Adds the invoke dynamic constant"

  override def getName = "InvokeDynamic"
}
