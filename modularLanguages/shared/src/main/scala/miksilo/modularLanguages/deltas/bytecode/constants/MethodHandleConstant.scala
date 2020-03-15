package miksilo.modularLanguages.deltas.bytecode.constants

import miksilo.modularLanguages.core.bigrammar.BiGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node.{Node, NodeField, NodeShape}
import miksilo.modularLanguages.deltas.bytecode.PrintByteCode._

object MethodHandleConstant extends ConstantPoolEntry {

  object MethodHandleKey extends NodeShape
  object MethodHandleReference extends NodeField
  object MethodHandleIndex extends NodeField

  def construct(kind: Int, index: Int) = new Node(MethodHandleKey, MethodHandleReference -> kind, MethodHandleIndex -> index)

  override def shape = MethodHandleKey

  override def getBytes(compilation: Compilation, constant: Node): Seq[Byte] = {
    byteToBytes(15) ++ byteToBytes(constant(MethodHandleReference).asInstanceOf[Int]) ++ shortToBytes(constant(MethodHandleIndex).asInstanceOf[Int])
  }

  override def getConstantEntryGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    (integer.as(MethodHandleReference) ~< ":") ~~ integer.as(MethodHandleIndex)
  }

  override def description: String = "Adds the method handle constant"

  override val getName = "MethodHandle"
}
