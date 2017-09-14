package transformations.bytecode.constants

import core.bigrammar.BiGrammar
import core.particles.CompilationState
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node}
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.constants.MethodHandleConstant.MethodHandleKey

object InvokeDynamicConstant extends ConstantEntry {

  object InvokeDynamicKey extends Key
  object InvokeDynamicBootstrapMethodIndex extends Key
  object InvokeDynamicNameAndTypeIndex extends Key

  def construct(kind: Int, index: Int) = new Node(InvokeDynamicKey, InvokeDynamicBootstrapMethodIndex -> kind, InvokeDynamicNameAndTypeIndex -> index)

  override def key = InvokeDynamicKey

  override def getByteCode(constant: Node, state: CompilationState): Seq[Byte] = {
    byteToBytes(18) ++ byteToBytes(constant(InvokeDynamicBootstrapMethodIndex).asInstanceOf[Int]) ++ shortToBytes(constant(InvokeDynamicNameAndTypeIndex).asInstanceOf[Int])
  }

  override def getConstantEntryGrammar(grammars: GrammarCatalogue): BiGrammar = (("invoke dynamic, bootstrap index:" ~~> integer <~ ", nameAndTypeIndex:") ~~ integer).
    asNode(MethodHandleKey, InvokeDynamicBootstrapMethodIndex, InvokeDynamicNameAndTypeIndex)

  override def description: String = "Adds the invoke dynamic constant"
}
