package transformations.bytecode.constants

import core.transformation.MetaObject
import transformations.bytecode.ByteCodeSkeleton._

trait ClassRefConstant {

  object ClassRefKey

  object ClassRefName

  def classRef(classRefNameIndex: Int): MetaObject = new MetaObject(ClassRefKey) {
    data.put(ClassRefName, classRefNameIndex)
  }

  def getClassRefName(classRef: MetaObject) = classRef(ClassRefName).asInstanceOf[Int]

  val classRefGrammar = "class reference:" ~~> integer ^^ parseMap(ClassRefKey, ClassRefName)
}
