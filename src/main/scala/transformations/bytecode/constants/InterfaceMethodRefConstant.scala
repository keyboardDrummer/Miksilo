package transformations.bytecode.constants

import core.particles.CompilationState
import core.particles.grammars.GrammarCatalogue
import core.particles.node.Node
import transformations.bytecode.PrintByteCode
import PrintByteCode._

object InterfaceMethodRefConstant extends ConstantEntry {

  object InterfaceMethodRefConstantKey

  object MethodRefClassName

  object MethodRefMethodName

  override def getByteCode(constant: Node, state: CompilationState): Seq[Byte] = {
    byteToBytes(11) ++
      shortToBytes(getClassRefIndex(constant)) ++
      shortToBytes(getNameIndex(constant))
  }

  override def key: Any = InterfaceMethodRefConstantKey

  def methodRef(classNameIndex: Int, methodNameAndTypeIndex: Int) = new Node(InterfaceMethodRefConstantKey,
    MethodRefClassName -> classNameIndex,
    MethodRefMethodName -> methodNameAndTypeIndex)

  def getClassRefIndex(methodRef: Node) = methodRef(MethodRefClassName).asInstanceOf[Int]

  def getNameIndex(methodRef: Node) = methodRef(MethodRefMethodName).asInstanceOf[Int]

  def getConstantEntryGrammar(grammars: GrammarCatalogue) = "interface method reference:" ~~> (integer <~ ".") ~ integer ^^
    parseMap(InterfaceMethodRefConstantKey, MethodRefClassName, MethodRefMethodName)

  override def description: String = "Defines the interface method reference constant, " +
    "which refers to a method by class name, method name and signature."
}
