package deltas.bytecode.constants

import core.deltas.grammars.LanguageGrammars
import core.language.node._
import core.language.{Compilation, Language}
import deltas.bytecode.ByteCodeSkeleton
import deltas.bytecode.PrintByteCode._
import deltas.bytecode.constants.Utf8ConstantDelta.Utf8Constant
import deltas.bytecode.coreInstructions.ConstantPoolIndexGrammar
import deltas.javac.classes.skeleton.QualifiedClassName

object ClassInfoConstant extends ConstantPoolEntry {

  object Shape extends NodeShape

  object Name extends NodeField

  def classRef(name: QualifiedClassName): Node = new Node(Shape, Name -> Utf8ConstantDelta.fromQualifiedClassName(name))
  def classRef(classRefNameIndex: Int): Node = new Node(Shape, Name -> classRefNameIndex)

  implicit class ClassInfoConstantWrapper[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def nameIndex: Int = node(Name).asInstanceOf[Int]
    def nameIndex_=(value: Int): Unit = node(Name) = value

    def name: Utf8Constant[T] = node(Name).asInstanceOf[T]
    def name_=(value: Utf8Constant[T]): Unit = node(Name) = value
  }

  override def shape = Shape

  override def getBytes(compilation: Compilation, constant: Node): Seq[Byte] = {
    byteToBytes(7) ++ shortToBytes(new ClassInfoConstantWrapper(constant).nameIndex)
  }

  override def inject(language: Language): Unit = {
    super.inject(language)
    ByteCodeSkeleton.constantReferences.add(language, shape, Map(Name -> Utf8ConstantDelta.shape))
  }

  override def getConstantEntryGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    find(ConstantPoolIndexGrammar).as(Name)
  }

  override def description: String = "Adds a new type of constant named the class reference. " +
    "It only contains an index pointing to a string constant that contains the name of the class."

  override val getName = "Class"
}
