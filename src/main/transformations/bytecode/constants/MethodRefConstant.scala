package transformations.bytecode.constants

import core.transformation.grammars.GrammarCatalogue
import core.transformation.{CompilationState, MetaObject}
import transformations.bytecode.PrintByteCode._

object MethodRefConstant extends ConstantEntry {

  object MethodRefKey

  object MethodRefClassName

  object MethodRefMethodName

  override def getByteCode(constant: MetaObject, state: CompilationState): Seq[Byte] = {
    byteToBytes(10) ++
      shortToBytes(getMethodRefClassRefIndex(constant)) ++
      shortToBytes(getMethodRefMethodNameIndex(constant))
  }

  override def key: Any = MethodRefKey

  def methodRef(classNameIndex: Int, methodNameAndTypeIndex: Int) = new MetaObject(MethodRefKey) {
    data.put(MethodRefClassName, classNameIndex)
    data.put(MethodRefMethodName, methodNameAndTypeIndex)
  }

  def getMethodRefClassRefIndex(methodRef: MetaObject) = methodRef(MethodRefClassName).asInstanceOf[Int]

  def getMethodRefMethodNameIndex(methodRef: MetaObject) = methodRef(MethodRefMethodName).asInstanceOf[Int]

  def getGrammar(grammars: GrammarCatalogue) = "method reference:" ~~> (integer <~ ".") ~ integer ^^ parseMap(MethodRefKey, MethodRefClassName, MethodRefMethodName)

  override def description: String = "Defines the method reference constant, which refers to a method by class name, method name and signature."
}
