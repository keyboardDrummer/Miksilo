package transformations.bytecode.constants

import core.bigrammar.BiGrammar
import core.particles.Language
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeClass, NodeField}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.coreInstructions.ConstantPoolIndexGrammar
import transformations.bytecode.extraConstants.QualifiedClassNameConstant
import transformations.javac.classes.skeleton.QualifiedClassName

object ClassInfoConstant extends ConstantEntry {

  object ClassRefKey extends NodeClass

  object ClassRefName extends NodeField

  def classRef(name: QualifiedClassName): Node = new Node(ClassRefKey, ClassRefName -> QualifiedClassNameConstant.create(name))
  def classRef(classRefNameIndex: Int): Node = new Node(ClassRefKey, ClassRefName -> classRefNameIndex)

  def getNameIndex(classRef: Node): Int = classRef(ClassRefName).asInstanceOf[Int]

  override def key = ClassRefKey

  override def getByteCode(constant: Node, state: Language): Seq[Byte] = {
    byteToBytes(7) ++ shortToBytes(getNameIndex(constant))
  }

  override def inject(state: Language): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getState(state).constantReferences.put(key, Map(ClassRefName -> QualifiedClassNameConstant.key))
  }

  override def getConstantEntryGrammar(grammars: GrammarCatalogue): BiGrammar =
    grammars.find(ConstantPoolIndexGrammar).as(ClassRefName) asNode ClassRefKey

  override def description: String = "Adds a new type of constant named the class reference. " +
    "It only contains an index pointing to a string constant that contains the name of the class."

  override def getName = "Class"
}
