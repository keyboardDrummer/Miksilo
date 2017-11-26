package deltas.bytecode.constants

import core.bigrammar.BiGrammar
import core.deltas.Language
import core.deltas.grammars.LanguageGrammars
import core.deltas.node._
import deltas.bytecode.ByteCodeSkeleton
import deltas.bytecode.PrintByteCode._
import deltas.bytecode.constants.Utf8ConstantDelta.Utf8Constant
import deltas.bytecode.coreInstructions.ConstantPoolIndexGrammar
import deltas.bytecode.extraConstants.TypeConstant.TypeConstantWrapper

object NameAndTypeConstant extends ConstantEntry {

  object Clazz extends NodeClass

  object Name extends NodeField

  object Type extends NodeField

  def nameAndType(nameIndex: Node, typeIndex: Node): Node = new Node(Clazz,
    Name -> nameIndex,
    Type -> typeIndex)

  def nameAndType(nameIndex: Int, typeIndex: Int): Node = new Node(Clazz,
    Name -> nameIndex,
    Type -> typeIndex)

  def getName(nameAndType: Node): Int = nameAndType(Name).asInstanceOf[Int]

  def getTypeIndex(nameAndType: Node): Int = nameAndType(Type).asInstanceOf[Int]

  override def key = Clazz

  implicit class NameAndTypeConstantWrapper[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def _type: TypeConstantWrapper[T] = node(Type).asInstanceOf[T]
    def _type_=(value: TypeConstantWrapper[T]): Unit = node(Type) = value

    def name: Utf8Constant[T] = node(Name).asInstanceOf[T]
    def name_=(value: Utf8Constant[T]): Unit = node(Name) = value
  }

  override def getByteCode(constant: Node, state: Language): Seq[Byte] = {
    byteToBytes(12) ++ shortToBytes(getName(constant)) ++
      shortToBytes(getTypeIndex(constant))
  }

  override def inject(state: Language): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getRegistry(state).constantReferences.put(key, Map(
      Name -> Utf8ConstantDelta.key,
      Type -> Utf8ConstantDelta.key))
  }

  override def getConstantEntryGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    find(ConstantPoolIndexGrammar).as(Name) ~~ find(ConstantPoolIndexGrammar).as(Type)
  }

  override def description: String = "Defines the name and type constant, which contains a name and a field or method descriptor."

  override def getName = "NameAndType"
}
