package transformations.bytecode.constants

import core.biGrammar.BiGrammar
import core.particles.grammars.GrammarCatalogue
import core.particles.CompilationState
import core.particles.node.MetaObject
import transformations.bytecode.PrintByteCode._

object FieldRefConstant extends ConstantEntry {

  object FieldRef

  object FieldRefClassIndex

  object FieldRefNameAndTypeIndex

  def fieldRef(classIndex: Int, nameAndTypeIndex: Int) = new MetaObject(FieldRef,
    FieldRefClassIndex -> classIndex,
    FieldRefNameAndTypeIndex -> nameAndTypeIndex)

  override def getByteCode(constant: MetaObject, state: CompilationState): Seq[Byte] = {
    byteToBytes(9) ++
      shortToBytes(getFieldRefClassIndex(constant)) ++
      shortToBytes(getNameAndTypeIndex(constant))
  }

  override def key: Any = FieldRef

  def getFieldRefClassIndex(fieldRef: MetaObject) = fieldRef(FieldRefClassIndex).asInstanceOf[Int]

  def getNameAndTypeIndex(fieldRef: MetaObject) = fieldRef(FieldRefNameAndTypeIndex).asInstanceOf[Int]

  override def getGrammar(grammars: GrammarCatalogue): BiGrammar = "field reference:" ~~> (integer <~ ".") ~ integer ^^ parseMap(FieldRef, FieldRefClassIndex, FieldRefNameAndTypeIndex)

  override def description: String = "Defines the field reference constant, which reference to a field by class name, field name and type."
}
