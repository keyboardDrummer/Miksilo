package transformations.bytecode.constants

import core.bigrammar.BiGrammar
import core.particles.Language
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node}
import transformations.bytecode.PrintByteCode._

object InvokeDynamicConstant extends ConstantEntry {

  object InvokeDynamicBootstrapMethodIndex extends Key
  object InvokeDynamicNameAndTypeIndex extends Key

  def construct(kind: Int, index: Int) = new Node(key, InvokeDynamicBootstrapMethodIndex -> kind, InvokeDynamicNameAndTypeIndex -> index)

  override def getByteCode(constant: Node, state: Language): Seq[Byte] = {
    byteToBytes(18) ++ byteToBytes(constant(InvokeDynamicBootstrapMethodIndex).asInstanceOf[Int]) ++ shortToBytes(constant(InvokeDynamicNameAndTypeIndex).asInstanceOf[Int])
  }

  override def getConstantEntryGrammar(grammars: GrammarCatalogue): BiGrammar = (("bootstrap index:" ~~> integer <~ ", nameAndTypeIndex:") ~~ integer).
    asNode(key, InvokeDynamicBootstrapMethodIndex, InvokeDynamicNameAndTypeIndex)

  override def description: String = "Adds the invoke dynamic constant"

  override def getName = "InvokeDynamic"
}
