package deltas.bytecode.constants

import core.bigrammar.BiGrammar
import core.deltas.Language
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeClass, NodeField}
import deltas.bytecode.PrintByteCode._

object InvokeDynamicConstant extends ConstantEntry {

  object InvokeDynamicKey extends NodeClass
  object InvokeDynamicBootstrapMethodIndex extends NodeField
  object InvokeDynamicNameAndTypeIndex extends NodeField

  def construct(kind: Int, index: Int) = new Node(InvokeDynamicKey, InvokeDynamicBootstrapMethodIndex -> kind, InvokeDynamicNameAndTypeIndex -> index)

  override def key = InvokeDynamicKey

  override def getByteCode(constant: Node, state: Language): Seq[Byte] = {
    byteToBytes(18) ++ byteToBytes(constant(InvokeDynamicBootstrapMethodIndex).asInstanceOf[Int]) ++ shortToBytes(constant(InvokeDynamicNameAndTypeIndex).asInstanceOf[Int])
  }

  override def getConstantEntryGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    "bootstrap index:" ~~> integer.as(InvokeDynamicBootstrapMethodIndex) ~<
      ", nameAndTypeIndex:" ~~
      integer.as(InvokeDynamicNameAndTypeIndex)
  }

  override def description: String = "Adds the invoke dynamic constant"

  override def getName = "InvokeDynamic"
}
