package deltas.bytecode.constants

import core.bigrammar.BiGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.node._
import core.language.{Compilation, Language}
import deltas.bytecode.ByteCodeSkeleton
import deltas.bytecode.PrintByteCode._
import deltas.bytecode.constants.Utf8ConstantDelta.Utf8Constant
import deltas.bytecode.coreInstructions.ConstantPoolIndexGrammar
import deltas.bytecode.extraConstants.TypeConstant
import deltas.bytecode.extraConstants.TypeConstant.TypeConstantWrapper

object NameAndTypeConstant extends ConstantPoolEntry {

  object Shape extends NodeShape

  object Name extends NodeField

  object Type extends NodeField

  def fromNameAndType(name: String, _type: Node): Node = {
    val nameIndex = Utf8ConstantDelta.create(name)
    NameAndTypeConstant.nameAndType(nameIndex, TypeConstant.constructor(_type))
  }

  def nameAndType(nameIndex: Node, typeIndex: Node): Node = new Node(Shape,
    Name -> nameIndex,
    Type -> typeIndex)

  def nameAndType(nameIndex: Int, typeIndex: Int): Node = new Node(Shape,
    Name -> nameIndex,
    Type -> typeIndex)

  def getName(nameAndType: Node): Int = nameAndType(Name).asInstanceOf[Int]

  def getTypeIndex(nameAndType: Node): Int = nameAndType(Type).asInstanceOf[Int]

  override def shape = Shape

  implicit class NameAndTypeConstantWrapper[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def _type: TypeConstantWrapper[T] = node(Type).asInstanceOf[T]
    def _type_=(value: TypeConstantWrapper[T]): Unit = node(Type) = value

    def name: Utf8Constant[T] = node(Name).asInstanceOf[T]
    def name_=(value: Utf8Constant[T]): Unit = node(Name) = value
  }

  override def getBytes(compilation: Compilation, constant: Node): Seq[Byte] = {
    byteToBytes(12) ++ shortToBytes(getName(constant)) ++
      shortToBytes(getTypeIndex(constant))
  }

  override def inject(language: Language): Unit = {
    super.inject(language)
    ByteCodeSkeleton.constantReferences.add(language, shape, Map(
      Name -> Utf8ConstantDelta.shape,
      Type -> Utf8ConstantDelta.shape))
  }

  override def getConstantEntryGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    find(ConstantPoolIndexGrammar).as(Name) ~~ find(ConstantPoolIndexGrammar).as(Type)
  }

  override def description: String = "Defines the name and type constant, which contains a name and a field or method descriptor."

  override val getName = "NameAndType"
}
