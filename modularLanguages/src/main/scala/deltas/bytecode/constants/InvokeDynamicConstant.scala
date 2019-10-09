package deltas.bytecode.constants

import core.bigrammar.BiGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.node.{Node, NodeField, NodeShape}
import core.language.{Compilation, Language}
import deltas.bytecode.PrintByteCode._

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
