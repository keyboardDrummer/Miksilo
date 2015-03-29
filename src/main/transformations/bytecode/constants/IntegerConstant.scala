package transformations.bytecode.constants

import core.bigrammar.BiGrammar
import core.particles.CompilationState
import core.particles.grammars.GrammarCatalogue
import core.particles.node.Node
import transformations.bytecode.PrintByteCode

object IntegerConstant extends ConstantEntry {

  object IntegerKey
  object IntegerValue

  def construct(index: Int) = new Node(IntegerKey, IntegerValue -> index)

  override def key: Any = IntegerKey

  override def getByteCode(constant: Node, state: CompilationState): Seq[Byte] = PrintByteCode.byteToBytes(3) ++
    PrintByteCode.intToBytes(constant(IntegerValue).asInstanceOf[Int])

  override def getGrammar(grammars: GrammarCatalogue): BiGrammar = integer ^^ parseMap(IntegerKey, IntegerValue)

  override def description: String = "Adds the integer constant entry."
}
