package transformations.bytecode.constants

import core.bigrammar.BiGrammar
import core.particles.CompilationState
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node}
import transformations.bytecode.PrintByteCode

object IntegerConstant extends ConstantEntry {

  object IntegerKey extends Key
  object IntegerValue extends Key

  def construct(index: Int) = new Node(IntegerKey, IntegerValue -> index)

  override def key = IntegerKey

  override def getByteCode(constant: Node, state: CompilationState): Seq[Byte] = PrintByteCode.byteToBytes(3) ++
    PrintByteCode.intToBytes(constant(IntegerValue).asInstanceOf[Int])

  override def getConstantEntryGrammar(grammars: GrammarCatalogue): BiGrammar = "integer:" ~~> integer asNode(IntegerKey, IntegerValue)

  override def description: String = "Adds the integer constant entry."
}
