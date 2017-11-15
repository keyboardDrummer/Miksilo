package deltas.bytecode.constants

import core.bigrammar.BiGrammar
import core.particles.Language
import core.particles.grammars.LanguageGrammars
import core.particles.node._
import deltas.bytecode.ByteCodeSkeleton
import deltas.bytecode.PrintByteCode._
import deltas.bytecode.coreInstructions.ConstantPoolIndexGrammar
import deltas.bytecode.extraConstants.QualifiedClassNameConstantDelta
import deltas.bytecode.extraConstants.QualifiedClassNameConstantDelta.QualifiedClassNameConstant
import deltas.javac.classes.skeleton.QualifiedClassName

object ClassInfoConstant extends ConstantEntry {

  object Clazz extends NodeClass

  object Name extends NodeField

  def classRef(name: QualifiedClassName): Node = new Node(Clazz, Name -> QualifiedClassNameConstantDelta.create(name))
  def classRef(classRefNameIndex: Int): Node = new Node(Clazz, Name -> classRefNameIndex)

  implicit class ClassInfoConstantWrapper[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def nameIndex: Int = node(Name).asInstanceOf[Int]
    def nameIndex_=(value: Int): Unit = node(Name) = value

    def name: QualifiedClassNameConstant[T] = node(Name).asInstanceOf[T]
    def name_=(value: QualifiedClassNameConstant[T]): Unit = node(Name) = value
  }

  override def key = Clazz

  override def getByteCode(constant: Node, state: Language): Seq[Byte] = {
    byteToBytes(7) ++ shortToBytes(new ClassInfoConstantWrapper(constant).nameIndex)
  }

  override def inject(state: Language): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getRegistry(state).constantReferences.put(key, Map(Name -> QualifiedClassNameConstantDelta.key))
  }

  override def getConstantEntryGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    find(ConstantPoolIndexGrammar).as(Name)
  }

  override def description: String = "Adds a new type of constant named the class reference. " +
    "It only contains an index pointing to a string constant that contains the name of the class."

  override def getName = "Class"
}
