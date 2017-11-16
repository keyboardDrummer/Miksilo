package deltas.bytecode.constants

import core.bigrammar.BiGrammar
import core.bigrammar.grammars.Keyword
import core.deltas.Language
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeClass, NodeField}
import deltas.bytecode.PrintByteCode
import deltas.bytecode.coreInstructions.ConstantPoolIndexGrammar

object StringConstant extends ConstantEntry {

  object StringKey extends NodeClass
  object StringIndex extends NodeField

  def construct(index: Int) = new Node(StringKey, StringIndex -> index)

  override def key = StringKey

  override def getByteCode(constant: Node, state: Language): Seq[Byte] = PrintByteCode.byteToBytes(8) ++
    PrintByteCode.shortToBytes(constant(StringIndex).asInstanceOf[Int])

  override def getConstantEntryGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    find(ConstantPoolIndexGrammar).as(StringIndex)
  }

  override def description: String = "Adds the string constant entry."

  override def getName = Keyword("String", reserved = false) //TODO all die getNames moeten geen reserved keywords zijn. Misschien de default van keyword switchen.
}
