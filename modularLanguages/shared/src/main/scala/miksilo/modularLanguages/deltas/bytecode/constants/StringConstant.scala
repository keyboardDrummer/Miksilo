package miksilo.modularLanguages.deltas.bytecode.constants

import miksilo.modularLanguages.core.bigrammar.BiGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node.{Node, NodeField, NodeShape}
import miksilo.modularLanguages.deltas.bytecode.PrintByteCode
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.ConstantPoolIndexGrammar

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
