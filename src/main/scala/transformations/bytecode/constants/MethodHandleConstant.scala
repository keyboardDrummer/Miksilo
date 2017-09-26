package transformations.bytecode.constants

import core.bigrammar.BiGrammar
import core.particles.Language
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeField}
import transformations.bytecode.PrintByteCode._

object MethodHandleConstant extends ConstantEntry {

  object MethodHandleReference extends NodeField
  object MethodHandleIndex extends NodeField

  def construct(kind: Int, index: Int) = new Node(key, MethodHandleReference -> kind, MethodHandleIndex -> index)

  override def getByteCode(constant: Node, state: Language): Seq[Byte] = {
    byteToBytes(15) ++ byteToBytes(constant(MethodHandleReference).asInstanceOf[Int]) ++ shortToBytes(constant(MethodHandleIndex).asInstanceOf[Int])
  }

  override def getConstantEntryGrammar(grammars: GrammarCatalogue): BiGrammar = ((integer.as(MethodHandleReference) <~ ":") ~~ integer.as(MethodHandleIndex)).
    asNode(key)

  override def description: String = "Adds the method handle constant"

  override def getName = "MethodHandle"
}
