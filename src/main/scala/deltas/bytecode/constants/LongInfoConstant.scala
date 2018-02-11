package deltas.bytecode.constants

import core.bigrammar.BiGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeField, NodeShape}
import core.language.Language
import deltas.bytecode.PrintByteCode

object LongInfoConstant extends ConstantEntry {

  implicit class LongConstantEntry(node: Node) {
    def value: Long = node(LongEntryValue).asInstanceOf[Long]
  }

  def construct(value: Long) = new Node(LongEntryKey, LongEntryValue -> value)

  object LongEntryKey extends NodeShape
  object LongEntryValue extends NodeField
  override def key = LongEntryKey

  override def getByteCode(constant: Node, state: Language): Seq[Byte] = PrintByteCode.byteToBytes(5) ++ PrintByteCode.longToBytes(constant.value)

  override def getConstantEntryGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    number.as(LongEntryValue)
  }

  override def description: String = "Add the long constant entry."

  override def getName = "Long"
}
