package miksilo.modularLanguages.deltas.bytecode.constants

import miksilo.modularLanguages.core.bigrammar.BiGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.node._
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.deltas.bytecode.ByteCodeSkeleton
import miksilo.modularLanguages.deltas.bytecode.PrintByteCode._
import miksilo.modularLanguages.deltas.bytecode.constants.NameAndTypeConstant.NameAndTypeConstantWrapper
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.ConstantPoolIndexGrammar

object MethodRefConstant extends ConstantPoolEntry {

  object MethodRefKey extends NodeShape

  object ClassRef extends NodeField

  object NameAndType extends NodeField

  override def getBytes(compilation: Compilation, constant: Node): Seq[Byte] = {
    byteToBytes(10) ++
      shortToBytes(getMethodRefClassRefIndex(constant)) ++
      shortToBytes(getNameAndTypeIndex(constant))
  }

  implicit class MethodRefWrapper[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def nameAndType: NameAndTypeConstantWrapper[T] = node(NameAndType).asInstanceOf[T]
    def nameAndType_=(value: NameAndTypeConstantWrapper[T]): Unit = node(NameAndType) = value
  }

  override def shape = MethodRefKey

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

  override def inject(language: Language): Unit = {
    super.inject(language)
    ByteCodeSkeleton.constantReferences.add(language, shape,
      Map(ClassRef -> ClassInfoConstant.shape, NameAndType -> NameAndTypeConstant.shape))
  }

  override val getName = "Methodref"
}
