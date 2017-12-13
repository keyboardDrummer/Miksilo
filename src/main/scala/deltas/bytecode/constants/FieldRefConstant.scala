package deltas.bytecode.constants

import core.bigrammar.BiGrammar
import core.deltas.Language
import core.deltas.grammars.LanguageGrammars
import core.deltas.node._
import deltas.bytecode.ByteCodeSkeleton
import deltas.bytecode.PrintByteCode._
import deltas.bytecode.constants.NameAndTypeConstant.NameAndTypeConstantWrapper
import deltas.bytecode.coreInstructions.ConstantPoolIndexGrammar

object FieldRefConstant extends ConstantEntry {

  object FieldRef extends NodeShape

  object ClassInfo extends NodeField

  object NameAndType extends NodeField

  implicit class FieldRefWrapper[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def nameAndType: NameAndTypeConstantWrapper[T] = node(NameAndType).asInstanceOf[T]
    def nameAndType_=(value: NameAndTypeConstantWrapper[T]): Unit = node(NameAndType) = value

    def nameAndTypeIndex: Int = node(NameAndType).asInstanceOf[Int]
    def nameAndTypeIndex_=(value: Int): Unit = node(NameAndType) = value

    def classIndex: Int = node(ClassInfo).asInstanceOf[Int]
    def classIndex_=(value: Int): Unit = node(ClassInfo) = value
  }

  def fieldRef(classConstant: Node, nameAndType: Node) = new Node(FieldRef,
    ClassInfo -> classConstant,
    NameAndType -> nameAndType)

  def fieldRef(classIndex: Int, nameAndTypeIndex: Int) = new Node(FieldRef,
    ClassInfo -> classIndex,
    NameAndType -> nameAndTypeIndex)

  override def getByteCode(constant: Node, state: Language): Seq[Byte] = {
    val fieldRef: FieldRefWrapper[Node] = constant
    byteToBytes(9) ++
      shortToBytes(fieldRef.classIndex) ++
      shortToBytes(fieldRef.nameAndTypeIndex)
  }

  override def inject(state: Language): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getRegistry(state).constantReferences.put(key, Map(
      ClassInfo -> ClassInfoConstant.key,
      NameAndType -> NameAndTypeConstant.key))
  }

  override def key = FieldRef

  override def getConstantEntryGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    find(ConstantPoolIndexGrammar).as(ClassInfo) ~< "." ~
      find(ConstantPoolIndexGrammar).as(NameAndType)
  }

  override def description: String = "Defines the field reference constant, which reference to a field by class name, field name and type."

  override def getName = "Fieldref"
}
