package transformations.bytecode.constants

import core.transformation.MetaObject
import core.transformation.sillyCodePieces.GrammarTransformation

trait MethodRefConstant extends GrammarTransformation {

  object MethodRefKey

  object MethodRefClassName

  object MethodRefMethodName

  val methodRefGrammar = "method reference:" ~~> (number <~ ".") ~ number ^^ parseMap(MethodRefKey, MethodRefClassName, MethodRefMethodName)

  def methodRef(classNameIndex: Int, methodNameAndTypeIndex: Int) = new MetaObject(MethodRefKey) {
    data.put(MethodRefClassName, classNameIndex)
    data.put(MethodRefMethodName, methodNameAndTypeIndex)
  }

  def getMethodRefClassRefIndex(methodRef: MetaObject) = methodRef(MethodRefClassName).asInstanceOf[Int]

  def getMethodRefMethodNameIndex(methodRef: MetaObject) = methodRef(MethodRefMethodName).asInstanceOf[Int]
}
