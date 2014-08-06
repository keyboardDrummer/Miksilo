package transformations.javac.types

import core.exceptions.BadInputException
import core.transformation._
import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.PrintByteCode._
import transformations.javac.base.ConstantPool
import transformations.javac.types.BooleanTypeC.BooleanTypeKey

import scala.collection.mutable

class TypeMismatchException(to: MetaObject, from: MetaObject) extends BadInputException {
  override def toString = s"cannot assign: $to = $from"
}

class NoCommonSuperTypeException(first: MetaObject, second: MetaObject) extends BadInputException

class AmbiguousCommonSuperTypeException(first: MetaObject, second: MetaObject) extends BadInputException

object TypeC extends GrammarTransformation {
  def getVerificationInfoBytes(clazz: MetaObject, _type: MetaObject, state: TransformationState): Seq[Byte] = {

    val constantPool = new ConstantPool(ByteCodeSkeleton.getConstantPool(clazz))
    val stackType: MetaObject = toStackType(_type)
    stackType.clazz match {
      case IntTypeC.IntTypeKey => hexToBytes("01")
      case LongTypeC.LongTypeKey => hexToBytes("02")
      case ObjectTypeC.ObjectTypeKey => hexToBytes("07") ++ shortToBytes(constantPool.getClassRef(ObjectTypeC.getObjectTypeName(stackType).right.get))
    }
  }

  def toStackType(_type: MetaObject) = _type.clazz match {
    case BooleanTypeKey => IntTypeC.intType
    case _ => _type
  }

  def getTypeSize(_type: MetaObject, state: TransformationState): Int = getState(state).stackSize(_type.clazz)


  def getByteCodeString(state: TransformationState): MetaObject => String =
    _type => getState(state).toByteCodeString(_type.clazz)(_type)

  override def dependencies: Set[Contract] = Set.empty

  def checkAssignableTo(state: TransformationState)(to: MetaObject, from: MetaObject) = {
    if (!isAssignableTo(state)(to, from))
      throw new TypeMismatchException(to, from)
  }

  def union(state: TransformationState)(first: MetaObject, second: MetaObject): MetaObject = {
    val filteredDepths = getAllSuperTypes(state)(first).map(depthTypes => depthTypes.filter(_type => isAssignableTo(state)(_type, second)))
    val resultDepth = filteredDepths.find(depth => depth.nonEmpty).getOrElse(throw new NoCommonSuperTypeException(first, second))
    if (resultDepth.size > 1)
      throw new AmbiguousCommonSuperTypeException(first, second)

    resultDepth.head
  }

  def isAssignableTo(state: TransformationState)(to: MetaObject, from: MetaObject): Boolean = {
    val fromSuperTypes = getSuperTypes(state)(from)
    if (to.equals(from))
      return true

    fromSuperTypes.exists(_type => _type.equals(to))
  }

  def getAllSuperTypes(state: TransformationState)(_type: MetaObject): Stream[Set[MetaObject]] = {
    var returnedTypes = Set.empty[MetaObject]
    Stream.iterate(Set(_type))(previousDepthTypes => {
      val result = previousDepthTypes.flatMap(_type => getSuperTypes(state)(_type)).filter(_type => !returnedTypes.contains(_type))
      returnedTypes ++= result
      result
    })
  }

  def getSuperTypes(state: TransformationState)(_type: MetaObject) = getSuperTypesRegistry(state)(_type.clazz)(_type)

  def getSuperTypesRegistry(state: TransformationState) = {
    getState(state).superTypes
  }

  def getState(state: TransformationState) = state.data.getOrElseUpdate(this, new State()).asInstanceOf[State]

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    grammars.create(TypeGrammar)
  }

  class State {
    val superTypes = new mutable.HashMap[Any, MetaObject => Seq[MetaObject]]()
    val toByteCodeString = new mutable.HashMap[Any, MetaObject => String]()
    val stackSize = new mutable.HashMap[Any, Int]()
  }

  object TypeGrammar

}
