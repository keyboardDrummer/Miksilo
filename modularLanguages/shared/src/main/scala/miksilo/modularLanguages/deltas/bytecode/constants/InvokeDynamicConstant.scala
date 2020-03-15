package miksilo.modularLanguages.deltas.bytecode.constants

import miksilo.modularLanguages.core.bigrammar.BiGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.node.{Node, NodeField, NodeShape}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.deltas.bytecode.PrintByteCode._

object InvokeDynamicConstant extends ConstantPoolEntry {

  object InvokeDynamicKey extends NodeShape
  object InvokeDynamicBootstrapMethodIndex extends NodeField
  object InvokeDynamicNameAndTypeIndex extends NodeField

  def construct(kind: Int, index: Int) = new Node(InvokeDynamicKey, InvokeDynamicBootstrapMethodIndex -> kind, InvokeDynamicNameAndTypeIndex -> index)

  override def shape = InvokeDynamicKey

  override def getBytes(compilation: Compilation, constant: Node): Seq[Byte] = {
    byteToBytes(18) ++ byteToBytes(constant(InvokeDynamicBootstrapMethodIndex).asInstanceOf[Int]) ++ shortToBytes(constant(InvokeDynamicNameAndTypeIndex).asInstanceOf[Int])
  }

  override def getConstantEntryGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    "bootstrapIndex" ~ ":" ~~> integer.as(InvokeDynamicBootstrapMethodIndex) ~<
      "," ~~ "nameAndTypeIndex" ~ ":" ~~
      integer.as(InvokeDynamicNameAndTypeIndex)
  }

  override def description: String = "Adds the invoke dynamic constant"

  override val getName = "InvokeDynamic"
}
