package transformations.bytecode.constants

import core.bigrammar.BiGrammar
import core.particles.CompilationState
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node}
import transformations.bytecode.PrintByteCode

object LongInfoConstant extends ConstantEntry {

  implicit class LongConstantEntry(node: Node) {
    def value: Long = node(LongEntryValue).asInstanceOf[Long]
  }

  def construct(value: Long) = new Node(LongEntryKey, LongEntryValue -> value)

  object LongEntryKey extends Key
  object LongEntryValue extends Key
  override def key = LongEntryKey

  override def getByteCode(constant: Node, state: CompilationState): Seq[Byte] = PrintByteCode.byteToBytes(5) ++ PrintByteCode.longToBytes(constant.value)

  override def getConstantEntryGrammar(grammars: GrammarCatalogue): BiGrammar = "long:" ~~> number asNode(LongEntryKey, LongEntryValue)

  override def description: String = "Add the long constant entry."
}
