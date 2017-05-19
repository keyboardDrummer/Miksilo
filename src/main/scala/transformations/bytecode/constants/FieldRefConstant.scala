package transformations.bytecode.constants

import core.bigrammar.BiGrammar
import core.particles.grammars.GrammarCatalogue
import core.particles.CompilationState
import core.particles.node.{Key, Node}
import transformations.bytecode.PrintByteCode._

object FieldRefConstant extends ConstantEntry {

  object FieldRef extends Key

  object FieldRefClassIndex extends Key

  object FieldRefNameAndTypeIndex extends Key

  def fieldRef(classIndex: Int, nameAndTypeIndex: Int) = new Node(FieldRef,
    FieldRefClassIndex -> classIndex,
    FieldRefNameAndTypeIndex -> nameAndTypeIndex)

  override def getByteCode(constant: Node, state: CompilationState): Seq[Byte] = {
    byteToBytes(9) ++
      shortToBytes(getFieldRefClassIndex(constant)) ++
      shortToBytes(getNameAndTypeIndex(constant))
  }

  override def key: Any = FieldRef

  def getFieldRefClassIndex(fieldRef: Node): Int = fieldRef(FieldRefClassIndex).asInstanceOf[Int]

  def getNameAndTypeIndex(fieldRef: Node): Int = fieldRef(FieldRefNameAndTypeIndex).asInstanceOf[Int]

  override def getConstantEntryGrammar(grammars: GrammarCatalogue): BiGrammar =
    ("field reference:" ~~> (integer <~ ".") ~ integer).asNode(FieldRef, FieldRefClassIndex, FieldRefNameAndTypeIndex)

  override def description: String = "Defines the field reference constant, which reference to a field by class name, field name and type."
}
