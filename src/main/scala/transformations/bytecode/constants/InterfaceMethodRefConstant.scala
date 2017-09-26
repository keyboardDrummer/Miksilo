package transformations.bytecode.constants

import core.particles.Language
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeField}
import transformations.bytecode.PrintByteCode._

object InterfaceMethodRefConstant extends ConstantEntry {

  object MethodRefClassName extends NodeField

  object MethodRefMethodName extends NodeField

  override def getByteCode(constant: Node, state: Language): Seq[Byte] = {
    byteToBytes(11) ++
      shortToBytes(getClassRefIndex(constant)) ++
      shortToBytes(getNameIndex(constant))
  }

  def methodRef(classNameIndex: Int, methodNameAndTypeIndex: Int) = new Node(key,
    MethodRefClassName -> classNameIndex,
    MethodRefMethodName -> methodNameAndTypeIndex)

  def getClassRefIndex(methodRef: Node) = methodRef(MethodRefClassName).asInstanceOf[Int]

  def getNameIndex(methodRef: Node) = methodRef(MethodRefMethodName).asInstanceOf[Int]

  def getConstantEntryGrammar(grammars: GrammarCatalogue) = ((integer.as(MethodRefClassName) <~ ".") ~ integer.as(MethodRefMethodName)).
    asNode(key)

  override def description: String = "Defines the interface method reference constant, " +
    "which refers to a method by class name, method name and signature."

  override def getName = "InterfaceMethodref"
}
