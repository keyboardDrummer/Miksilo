package transformations.bytecode.constants

import core.bigrammar.BiGrammar
import core.particles.Language
import core.particles.grammars.LanguageGrammars
import core.particles.node._
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.constants.NameAndTypeConstant.NameAndTypeConstantWrapper
import transformations.bytecode.coreInstructions.ConstantPoolIndexGrammar

object MethodRefConstant extends ConstantEntry {

  object MethodRefKey extends NodeClass

  object ClassRef extends NodeField

  object NameAndType extends NodeField

  override def getByteCode(constant: Node, state: Language): Seq[Byte] = {
    byteToBytes(10) ++
      shortToBytes(getMethodRefClassRefIndex(constant)) ++
      shortToBytes(getNameAndTypeIndex(constant))
  }

  implicit class MethodRefWrapper[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def nameAndType: NameAndTypeConstantWrapper[T] = node(NameAndType).asInstanceOf[T]
    def nameAndType_=(value: NameAndTypeConstantWrapper[T]): Unit = node(NameAndType) = value
  }

  override def key = MethodRefKey

  def methodRef(classNameIndex: Node, methodNameAndTypeIndex: Node) = new Node(MethodRefKey,
    ClassRef -> classNameIndex,
    NameAndType -> methodNameAndTypeIndex)

  def methodRef(classNameIndex: Int, methodNameAndTypeIndex: Int) = new Node(MethodRefKey,
    ClassRef -> classNameIndex,
    NameAndType -> methodNameAndTypeIndex)

  def getMethodRefClassRefIndex(methodRef: Node): Int = methodRef(ClassRef).asInstanceOf[Int]

  def getNameAndTypeIndex(methodRef: Node): Int = methodRef(NameAndType).asInstanceOf[Int]

  def getConstantEntryGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    find(ConstantPoolIndexGrammar).as(ClassRef) ~< "." ~
      find(ConstantPoolIndexGrammar).as(NameAndType)
  }

  override def description: String = "Defines the method reference constant, which refers to a method by class name, method name and signature."

  override def inject(state: Language): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getRegistry(state).constantReferences.put(key,
      Map(ClassRef -> ClassInfoConstant.key, NameAndType -> NameAndTypeConstant.key))
  }

  override def getName = "Methodref"
}




//En dan twee transformaties, 1 is RemoveConstantPool die de constantEntry grammars niet veranderd.
//En de tweede is een JasminConstantPoolSyntax
