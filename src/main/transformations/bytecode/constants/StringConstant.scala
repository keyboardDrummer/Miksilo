package transformations.bytecode.constants

import core.bigrammar.BiGrammar
import core.particles.CompilationState
import core.particles.grammars.GrammarCatalogue
import core.particles.node.Node
import transformations.bytecode.PrintByteCode

object StringConstant extends ConstantEntry {

  object StringKey
  object StringIndex

  def construct(index: Int) = new Node(StringKey, StringIndex -> index)

  override def key: Any = StringKey

  override def getByteCode(constant: Node, state: CompilationState): Seq[Byte] = PrintByteCode.byteToBytes(8) ++
    PrintByteCode.shortToBytes(constant(StringIndex).asInstanceOf[Int])

  override def getGrammar(grammars: GrammarCatalogue): BiGrammar = "string at index:" ~~> integer ^^ parseMap(StringKey, StringIndex)

  override def description: String = "Adds the string constant entry."
}
