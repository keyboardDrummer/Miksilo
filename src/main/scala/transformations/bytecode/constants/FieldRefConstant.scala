package transformations.bytecode.constants

import core.bigrammar.BiGrammar
import core.particles.CompilationState
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeClass, NodeField}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.coreInstructions.ConstantPoolIndexGrammar

object FieldRefConstant extends ConstantEntry {

  object FieldRef extends NodeClass

  object FieldRefClassIndex extends NodeField

  object FieldRefNameAndTypeIndex extends NodeField

  def fieldRef(classConstant: Node, nameAndType: Node) = new Node(FieldRef,
    FieldRefClassIndex -> classConstant,
    FieldRefNameAndTypeIndex -> nameAndType)

  def fieldRef(classIndex: Int, nameAndTypeIndex: Int) = new Node(FieldRef,
    FieldRefClassIndex -> classIndex,
    FieldRefNameAndTypeIndex -> nameAndTypeIndex)

  override def getByteCode(constant: Node, state: CompilationState): Seq[Byte] = {
    byteToBytes(9) ++
      shortToBytes(getFieldRefClassIndex(constant)) ++
      shortToBytes(getNameAndTypeIndex(constant))
  }

  override def inject(state: CompilationState): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getState(state).constantReferences.put(key, Map(
      FieldRefClassIndex -> ClassInfoConstant.key,
      FieldRefNameAndTypeIndex -> NameAndTypeConstant.key))
  }

  override def key = FieldRef

  def getFieldRefClassIndex(fieldRef: Node): Int = fieldRef(FieldRefClassIndex).asInstanceOf[Int]

  def getNameAndTypeIndex(fieldRef: Node): Int = fieldRef(FieldRefNameAndTypeIndex).asInstanceOf[Int]

  override def getConstantEntryGrammar(grammars: GrammarCatalogue): BiGrammar =
    ("field reference:" ~~> (grammars.find(ConstantPoolIndexGrammar).as(FieldRefClassIndex) <~ ".") ~
      grammars.find(ConstantPoolIndexGrammar).as(FieldRefNameAndTypeIndex)).
      asNode(FieldRef)

  override def description: String = "Defines the field reference constant, which reference to a field by class name, field name and type."

  override def getName = "Fieldref"
}
