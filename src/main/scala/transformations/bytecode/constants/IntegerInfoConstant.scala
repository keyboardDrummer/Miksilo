package transformations.bytecode.constants

import core.bigrammar.BiGrammar
import core.particles.Language
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeField}
import transformations.bytecode.PrintByteCode

object IntegerInfoConstant extends ConstantEntry {

  object IntegerValue extends NodeField

  def construct(index: Int) = new Node(key, IntegerValue -> index)

  override def getByteCode(constant: Node, state: Language): Seq[Byte] = PrintByteCode.byteToBytes(3) ++
    PrintByteCode.intToBytes(constant(IntegerValue).asInstanceOf[Int])

  override def getConstantEntryGrammar(grammars: GrammarCatalogue): BiGrammar = integer.as(IntegerValue).asNode(key)

  override def description: String = "Adds the integer constant entry."

  override def getName = "Integer"
}
