package transformations.bytecode.constants

import core.bigrammar.BiGrammar
import core.particles.Language
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeField}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.coreInstructions.ConstantPoolIndexGrammar

object FieldRefConstant extends ConstantEntry {

  object FieldRefClassIndex extends NodeField

  object FieldRefNameAndTypeIndex extends NodeField

  def fieldRef(classConstant: Node, nameAndType: Node) = new Node(key,
    FieldRefClassIndex -> classConstant,
    FieldRefNameAndTypeIndex -> nameAndType)

  def fieldRef(classIndex: Int, nameAndTypeIndex: Int) = new Node(key,
    FieldRefClassIndex -> classIndex,
    FieldRefNameAndTypeIndex -> nameAndTypeIndex)

  override def getByteCode(constant: Node, state: Language): Seq[Byte] = {
    byteToBytes(9) ++
      shortToBytes(getFieldRefClassIndex(constant)) ++
      shortToBytes(getNameAndTypeIndex(constant))
  }

  override def inject(state: Language): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getState(state).constantReferences.put(key, Map(
      FieldRefClassIndex -> ClassInfoConstant.key,
      FieldRefNameAndTypeIndex -> NameAndTypeConstant.key))
  }
  def getFieldRefClassIndex(fieldRef: Node): Int = fieldRef(FieldRefClassIndex).asInstanceOf[Int]

  def getNameAndTypeIndex(fieldRef: Node): Int = fieldRef(FieldRefNameAndTypeIndex).asInstanceOf[Int]

  override def getConstantEntryGrammar(grammars: GrammarCatalogue): BiGrammar =
    ("field reference:" ~~> (grammars.find(ConstantPoolIndexGrammar).as(FieldRefClassIndex) <~ ".") ~
      grammars.find(ConstantPoolIndexGrammar).as(FieldRefNameAndTypeIndex)).
      asNode(key)

  override def description: String = "Defines the field reference constant, which reference to a field by class name, field name and type."

  override def getName = "Fieldref"
}
