package transformations.bytecode.constants

import core.bigrammar.BiGrammar
import core.particles.Language
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeField}
import transformations.bytecode.PrintByteCode

object DoubleInfoConstant extends ConstantEntry {

  implicit class LongConstantEntry(node: Node) {
    def value: Long = node(DoubleEntryValue).asInstanceOf[Long]
  }

  def construct(value: Double) = new Node(key, DoubleEntryValue -> value)

  object DoubleEntryValue extends NodeField

  override def getByteCode(constant: Node, state: Language): Seq[Byte] = PrintByteCode.byteToBytes(6) ++ PrintByteCode.longToBytes(constant.value)

  override def getConstantEntryGrammar(grammars: GrammarCatalogue): BiGrammar =
    number.as(DoubleEntryValue).asNode(key)

  override def description: String = "Add the double constant entry."

  override def getName = "Double"
}
