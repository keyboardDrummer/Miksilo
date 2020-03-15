package miksilo.modularLanguages.deltas.bytecode.constants

import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node.{Node, NodeField, NodeShape}
import miksilo.modularLanguages.deltas.bytecode.PrintByteCode._

object InterfaceMethodRefConstant extends ConstantPoolEntry {

  object InterfaceMethodRefConstantKey extends NodeShape

  object MethodRefClassName extends NodeField

  object MethodRefMethodName extends NodeField

  override def getBytes(compilation: Compilation, constant: Node): Seq[Byte] = {
    byteToBytes(11) ++
      shortToBytes(getClassRefIndex(constant)) ++
      shortToBytes(getNameIndex(constant))
  }

  override def shape = InterfaceMethodRefConstantKey

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

  override val getName = "InterfaceMethodref"
}
