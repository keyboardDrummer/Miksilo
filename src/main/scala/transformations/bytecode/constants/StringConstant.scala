package transformations.bytecode.constants

import core.bigrammar.{BiGrammar, Keyword}
import core.particles.Language
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeField}
import transformations.bytecode.PrintByteCode
import transformations.bytecode.coreInstructions.ConstantPoolIndexGrammar

object StringConstant extends ConstantEntry {

  object StringIndex extends NodeField

  def construct(index: Int) = new Node(key, StringIndex -> index)

  override def getByteCode(constant: Node, state: Language): Seq[Byte] = PrintByteCode.byteToBytes(8) ++
    PrintByteCode.shortToBytes(constant(StringIndex).asInstanceOf[Int])

  override def getConstantEntryGrammar(grammars: GrammarCatalogue): BiGrammar =
    grammars.find(ConstantPoolIndexGrammar).as(StringIndex).asNode(key)

  override def description: String = "Adds the string constant entry."

  override def getName = Keyword("String", reserved = false) //TODO all die getNames moeten geen reserved keywords zijn. Misschien de default van keyword switchen.
}
