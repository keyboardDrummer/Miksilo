package miksilo.modularLanguages.deltas.bytecode.constants

import miksilo.modularLanguages.core.bigrammar.BiGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node.{Node, NodeField, NodeShape}
import miksilo.modularLanguages.deltas.bytecode.PrintByteCode

object IntegerInfoConstant extends ConstantPoolEntry {

  object IntegerKey extends NodeShape
  object IntegerValue extends NodeField

  def construct(index: Int) = new Node(IntegerKey, IntegerValue -> index)

  override def shape = IntegerKey

  override def getBytes(compilation: Compilation, constant: Node): Seq[Byte] = PrintByteCode.byteToBytes(3) ++
    PrintByteCode.intToBytes(constant(IntegerValue).asInstanceOf[Int])

  override def getConstantEntryGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    integer.as(IntegerValue)
  }

  override def description: String = "Adds the integer constant entry."

  override val getName = "Integer"
}
