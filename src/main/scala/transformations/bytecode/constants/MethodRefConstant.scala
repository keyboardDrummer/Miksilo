package transformations.bytecode.constants

import core.bigrammar.BiGrammar
import core.particles.grammars.GrammarCatalogue
import core.particles.CompilationState
import core.particles.node.{Key, Node}
import transformations.bytecode.PrintByteCode._

object MethodRefConstant extends ConstantEntry {

  object MethodRefKey extends Key

  object MethodRefClassName extends Key

  object MethodRefMethodName extends Key

  override def getByteCode(constant: Node, state: CompilationState): Seq[Byte] = {
    byteToBytes(10) ++
      shortToBytes(getMethodRefClassRefIndex(constant)) ++
      shortToBytes(getNameAndTypeIndex(constant))
  }

  override def key: Any = MethodRefKey

  def methodRef(classNameIndex: Int, methodNameAndTypeIndex: Int) = new Node(MethodRefKey,
    MethodRefClassName -> classNameIndex,
    MethodRefMethodName -> methodNameAndTypeIndex)

  def getMethodRefClassRefIndex(methodRef: Node): Int = methodRef(MethodRefClassName).asInstanceOf[Int]

  def getNameAndTypeIndex(methodRef: Node): Int = methodRef(MethodRefMethodName).asInstanceOf[Int]

  def getConstantEntryGrammar(grammars: GrammarCatalogue): BiGrammar = ("method reference:" ~~> (integer <~ ".") ~ integer).
    asNode(MethodRefKey, MethodRefClassName, MethodRefMethodName)

  override def description: String = "Defines the method reference constant, which refers to a method by class name, method name and signature."
}
