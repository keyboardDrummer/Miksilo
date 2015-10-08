package transformations.bytecode.constants

import core.bigrammar.BiGrammar
import core.particles.CompilationState
import core.particles.grammars.GrammarCatalogue
import core.particles.node.Node
import transformations.bytecode.PrintByteCode
import transformations.bytecode.constants.LongConstantEntryC.LongEntryValue

object DoubleConstantEntryC extends ConstantEntry {

  implicit class LongConstantEntry(node: Node) {
    def value: Long = node(DoubleEntryValue).asInstanceOf[Long]
  }

  def construct(value: Double) = new Node(DoubleEntryKey, DoubleEntryValue -> value)

  object DoubleEntryKey
  object DoubleEntryValue
  override def key: Any = DoubleEntryKey

  override def getByteCode(constant: Node, state: CompilationState): Seq[Byte] = PrintByteCode.longToBytes(constant.value)

  override def getConstantEntryGrammar(grammars: GrammarCatalogue): BiGrammar = "double:" ~~> number ^^
    parseMap(DoubleEntryKey, DoubleEntryValue)

  override def description: String = "Add the double constant entry."
}
