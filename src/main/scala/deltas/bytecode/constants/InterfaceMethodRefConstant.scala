package deltas.bytecode.constants

import core.deltas.grammars.LanguageGrammars
import core.language.node.{Node, NodeField, NodeShape}
import core.language.Language
import deltas.bytecode.PrintByteCode._

object InterfaceMethodRefConstant extends ConstantEntry {

  object InterfaceMethodRefConstantKey extends NodeShape

  object MethodRefClassName extends NodeField

  object MethodRefMethodName extends NodeField

  override def getByteCode(constant: Node, state: Language): Seq[Byte] = {
    byteToBytes(11) ++
      shortToBytes(getClassRefIndex(constant)) ++
      shortToBytes(getNameIndex(constant))
  }

  override def key = InterfaceMethodRefConstantKey

  def methodRef(classNameIndex: Int, methodNameAndTypeIndex: Int) = new Node(InterfaceMethodRefConstantKey,
    MethodRefClassName -> classNameIndex,
    MethodRefMethodName -> methodNameAndTypeIndex)

  def getClassRefIndex(methodRef: Node) = methodRef(MethodRefClassName).asInstanceOf[Int]

  def getNameIndex(methodRef: Node) = methodRef(MethodRefMethodName).asInstanceOf[Int]

  def getConstantEntryGrammar(grammars: LanguageGrammars) = {
    import grammars._
    (integer.as(MethodRefClassName) ~< ".") ~ integer.as(MethodRefMethodName)
  }


  override def description: String = "Defines the interface method reference constant, " +
    "which refers to a method by class name, method name and signature."

  override def getName = "InterfaceMethodref"
}
