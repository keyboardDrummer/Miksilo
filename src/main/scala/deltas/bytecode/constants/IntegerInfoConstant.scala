package deltas.bytecode.constants

import core.bigrammar.BiGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.node.{Node, NodeField, NodeShape}
import core.language.Language
import deltas.bytecode.PrintByteCode

object IntegerInfoConstant extends ConstantEntry {

  object IntegerKey extends NodeShape
  object IntegerValue extends NodeField

  def construct(index: Int) = new Node(IntegerKey, IntegerValue -> index)

  override def key = IntegerKey

  override def getByteCode(constant: Node, state: Language): Seq[Byte] = PrintByteCode.byteToBytes(3) ++
    PrintByteCode.intToBytes(constant(IntegerValue).asInstanceOf[Int])

  override def getConstantEntryGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    integer.as(IntegerValue)
  }

  override def description: String = "Adds the integer constant entry."

  override def getName = "Integer"
}
