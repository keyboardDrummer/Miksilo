package transformations.bytecode.constants

import core.bigrammar.BiGrammar
import core.particles.Language
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeField}
import transformations.bytecode.PrintByteCode

object LongInfoConstant extends ConstantEntry {

  implicit class LongConstantEntry(node: Node) {
    def value: Long = node(LongEntryValue).asInstanceOf[Long]
  }

  def construct(value: Long) = new Node(key, LongEntryValue -> value)

  object LongEntryValue extends NodeField

  override def getByteCode(constant: Node, state: Language): Seq[Byte] = PrintByteCode.byteToBytes(5) ++ PrintByteCode.longToBytes(constant.value)

  override def getConstantEntryGrammar(grammars: GrammarCatalogue): BiGrammar = number.as(LongEntryValue).asNode(key)

  override def description: String = "Add the long constant entry."

  override def getName = "Long"
}
