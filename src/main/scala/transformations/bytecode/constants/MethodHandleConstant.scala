package transformations.bytecode.constants

import core.bigrammar.BiGrammar
import core.particles.CompilationState
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node}
import transformations.bytecode.PrintByteCode._

object MethodHandleConstant extends ConstantEntry {

  object MethodHandleKey extends Key
  object MethodHandleReference extends Key
  object MethodHandleIndex extends Key

  def construct(kind: Int, index: Int) = new Node(MethodHandleKey, MethodHandleReference -> kind, MethodHandleIndex -> index)

  override def key: Any = MethodHandleKey

  override def getByteCode(constant: Node, state: CompilationState): Seq[Byte] = {
    byteToBytes(15) ++ byteToBytes(constant(MethodHandleReference).asInstanceOf[Int]) ++ shortToBytes(constant(MethodHandleIndex).asInstanceOf[Int])
  }

  override def getConstantEntryGrammar(grammars: GrammarCatalogue): BiGrammar = (("method handle" ~~> integer <~ ":") ~~ integer).
    asNode(MethodHandleKey, MethodHandleReference, MethodHandleIndex)

  override def description: String = "Adds the method handle constant"
}
