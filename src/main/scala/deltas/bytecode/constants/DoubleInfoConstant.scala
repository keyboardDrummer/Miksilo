package deltas.bytecode.constants

import core.bigrammar.BiGrammar
import core.deltas.Language
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeShape, NodeField}
import deltas.bytecode.PrintByteCode

object DoubleInfoConstant extends ConstantEntry {

  implicit class LongConstantEntry(node: Node) {
    def value: Long = node(DoubleEntryValue).asInstanceOf[Long]
  }

  def construct(value: Double) = new Node(DoubleEntryKey, DoubleEntryValue -> value)

  object DoubleEntryKey extends NodeShape
  object DoubleEntryValue extends NodeField
  override def key = DoubleEntryKey

  override def getByteCode(constant: Node, state: Language): Seq[Byte] = PrintByteCode.byteToBytes(6) ++ PrintByteCode.longToBytes(constant.value)

  override def getConstantEntryGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    number.as(DoubleEntryValue)
  }

  override def description: String = "Add the double constant entry."

  override def getName = "Double"
}
