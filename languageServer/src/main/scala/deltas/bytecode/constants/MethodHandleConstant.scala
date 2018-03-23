package deltas.bytecode.constants

import core.bigrammar.BiGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.node.{Node, NodeField, NodeShape}
import core.language.{Compilation, Language}
import deltas.bytecode.PrintByteCode._

object MethodHandleConstant extends ConstantEntry {

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

  override def getName = "MethodHandle"
}
