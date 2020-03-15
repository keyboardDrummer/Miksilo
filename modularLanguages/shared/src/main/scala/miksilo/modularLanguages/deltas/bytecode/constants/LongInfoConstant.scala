package miksilo.modularLanguages.deltas.bytecode.constants

import miksilo.modularLanguages.core.bigrammar.BiGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node.{Node, NodeField, NodeShape}
import miksilo.modularLanguages.deltas.bytecode.PrintByteCode

object LongInfoConstant extends ConstantPoolEntry {

  implicit class LongConstantEntry(node: Node) {
    def value: Long = node(LongEntryValue).asInstanceOf[Long]
  }

  def construct(value: Long) = new Node(LongEntryKey, LongEntryValue -> value)

  object LongEntryKey extends NodeShape
  object LongEntryValue extends NodeField
  override def shape = LongEntryKey

  override def getBytes(compilation: Compilation, constant: Node): Seq[Byte] = PrintByteCode.byteToBytes(5) ++ PrintByteCode.longToBytes(constant.value)

  override def getConstantEntryGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    number.as(LongEntryValue)
  }

  override def description: String = "Add the long constant entry."

  override val getName = "Long"
}
