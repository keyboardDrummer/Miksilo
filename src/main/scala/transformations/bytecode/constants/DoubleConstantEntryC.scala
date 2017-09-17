package transformations.bytecode.constants

import core.bigrammar.BiGrammar
import core.particles.CompilationState
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeClass, NodeField}
import transformations.bytecode.PrintByteCode

object DoubleConstantEntryC extends ConstantEntry {

  implicit class LongConstantEntry(node: Node) {
    def value: Long = node(DoubleEntryValue).asInstanceOf[Long]
  }

  def construct(value: Double) = new Node(DoubleEntryKey, DoubleEntryValue -> value)

  object DoubleEntryKey extends NodeClass
  object DoubleEntryValue extends NodeField
  override def key = DoubleEntryKey

  override def getByteCode(constant: Node, state: CompilationState): Seq[Byte] = PrintByteCode.longToBytes(constant.value)

  override def getConstantEntryGrammar(grammars: GrammarCatalogue): BiGrammar =
    ("double:" ~~> number).asNode(DoubleEntryKey, DoubleEntryValue)

  override def description: String = "Add the double constant entry."
}
