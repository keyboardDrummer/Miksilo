package deltas.bytecode.constants

import core.deltas.grammars.LanguageGrammars
import core.language.Compilation
import core.language.node.{Node, NodeField, NodeShape}
import deltas.bytecode.PrintByteCode
import deltas.bytecode.coreInstructions.ConstantPoolIndexGrammar

object StringConstant extends ConstantPoolEntry {

  object StringKey extends NodeShape
  object StringIndex extends NodeField

  def construct(index: Int) = new Node(StringKey, StringIndex -> index)

  override def shape = StringKey

  override def getBytes(compilation: Compilation, constant: Node): Seq[Byte] = PrintByteCode.byteToBytes(8) ++
    PrintByteCode.shortToBytes(constant(StringIndex).asInstanceOf[Int])

  override def getConstantEntryGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    find(ConstantPoolIndexGrammar).as(StringIndex)
  }

  override def description: String = "Adds the string constant entry."

  override val getName = "String"
}
