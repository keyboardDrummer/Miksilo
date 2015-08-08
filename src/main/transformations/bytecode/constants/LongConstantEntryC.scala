package transformations.bytecode.constants

import core.bigrammar.BiGrammar
import core.particles.CompilationState
import core.particles.grammars.GrammarCatalogue
import core.particles.node.Node
import transformations.bytecode.PrintByteCode

object LongConstantEntryC extends ConstantEntry {

  implicit class LongConstantEntry(node: Node) {
    def value: Long = node(LongEntryValue).asInstanceOf[Long]
  }

  def construct(value: Long) = new Node(LongEntryKey, LongEntryValue -> value)

  object LongEntryKey
  object LongEntryValue
  override def key: Any = LongEntryKey

  override def getByteCode(constant: Node, state: CompilationState): Seq[Byte] = PrintByteCode.longToBytes(constant.value)

  override def getConstantEntryGrammar(grammars: GrammarCatalogue): BiGrammar = "long:" ~~> number ^^
    parseMap(LongEntryKey, LongEntryValue)

  override def description: String = "Add the long constant entry."
}
