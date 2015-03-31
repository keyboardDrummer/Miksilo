package transformations.bytecode.constants

import core.particles.grammars.GrammarCatalogue
import core.particles.CompilationState
import core.particles.node.Node
import transformations.bytecode.PrintByteCode._


object MethodRefConstant extends ConstantEntry {

  object MethodRefKey

  object MethodRefClassName

  object MethodRefMethodName

  override def getByteCode(constant: Node, state: CompilationState): Seq[Byte] = {
    byteToBytes(10) ++
      shortToBytes(getMethodRefClassRefIndex(constant)) ++
      shortToBytes(getMethodRefMethodNameIndex(constant))
  }

  override def key: Any = MethodRefKey

  def methodRef(classNameIndex: Int, methodNameAndTypeIndex: Int) = new Node(MethodRefKey,
    MethodRefClassName -> classNameIndex,
    MethodRefMethodName -> methodNameAndTypeIndex)

  def getMethodRefClassRefIndex(methodRef: Node) = methodRef(MethodRefClassName).asInstanceOf[Int]

  def getMethodRefMethodNameIndex(methodRef: Node) = methodRef(MethodRefMethodName).asInstanceOf[Int]

  def getGrammar(grammars: GrammarCatalogue) = "method reference:" ~~> (integer <~ ".") ~ integer ^^ parseMap(MethodRefKey, MethodRefClassName, MethodRefMethodName)

  override def description: String = "Defines the method reference constant, which refers to a method by class name, method name and signature."
}
