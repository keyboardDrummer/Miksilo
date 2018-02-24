package deltas.bytecode.constants

import core.bigrammar.BiGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.node._
import core.language.{Compilation, Language}
import deltas.bytecode.ByteCodeSkeleton
import deltas.bytecode.PrintByteCode._
import deltas.bytecode.coreInstructions.ConstantPoolIndexGrammar
import deltas.bytecode.extraConstants.QualifiedClassNameConstantDelta
import deltas.bytecode.extraConstants.QualifiedClassNameConstantDelta.QualifiedClassNameConstant
import deltas.javac.classes.skeleton.QualifiedClassName

object ClassInfoConstant extends ConstantEntry {

  object Shape extends NodeShape

  object Name extends NodeField

  def classRef(name: QualifiedClassName): Node = new Node(Shape, Name -> QualifiedClassNameConstantDelta.create(name))
  def classRef(classRefNameIndex: Int): Node = new Node(Shape, Name -> classRefNameIndex)

  implicit class ClassInfoConstantWrapper[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def nameIndex: Int = node(Name).asInstanceOf[Int]
    def nameIndex_=(value: Int): Unit = node(Name) = value

    def name: QualifiedClassNameConstant[T] = node(Name).asInstanceOf[T]
    def name_=(value: QualifiedClassNameConstant[T]): Unit = node(Name) = value
  }

  override def shape = Shape

  override def getBytes(compilation: Compilation, constant: Node): Seq[Byte] = {
    byteToBytes(7) ++ shortToBytes(new ClassInfoConstantWrapper(constant).nameIndex)
  }

  override def inject(language: Language): Unit = {
    super.inject(language)
    ByteCodeSkeleton.constantReferences.add(language, shape, Map(Name -> QualifiedClassNameConstantDelta.shape))
  }

  override def getConstantEntryGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    find(ConstantPoolIndexGrammar).as(Name)
  }

  override def description: String = "Adds a new type of constant named the class reference. " +
    "It only contains an index pointing to a string constant that contains the name of the class."

  override def getName = "Class"
}
