package transformations.types

import core.exceptions.BadInputException
import core.transformation._
import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation
import transformations.bytecode.{PrintByteCode, ByteCodeSkeleton}
import PrintByteCode._
import transformations.javac.classes.ConstantPool
import transformations.types.BooleanTypeC.BooleanTypeKey
import transformations.types.ObjectTypeC.ObjectTypeName

import scala.collection.mutable

class TypeMismatchException(to: MetaObject, from: MetaObject) extends BadInputException {
  override def toString = s"cannot assign: $to = $from"
}

class NoCommonSuperTypeException(first: MetaObject, second: MetaObject) extends BadInputException

class AmbiguousCommonSuperTypeException(first: MetaObject, second: MetaObject) extends BadInputException

object TypeC extends GrammarTransformation { //TODO move some specific type code to the respective type.
  def getVerificationInfoBytes(_type: MetaObject, state: TransformationState): Seq[Byte] = {
    _type.clazz match {
      case IntTypeC.IntTypeKey => hexToBytes("01")
      case LongTypeC.LongTypeKey => hexToBytes("04")
      case ObjectTypeC.ObjectTypeKey => hexToBytes("07") ++ shortToBytes(_type(ObjectTypeName).asInstanceOf[Int])
    }
  }

  def toStackType(_type: MetaObject, state: TransformationState) : MetaObject = {
    toStackType(ByteCodeSkeleton.getState(state).constantPool, _type)
  }

  def toStackType(constantPool: ConstantPool, _type: MetaObject) : MetaObject  = {
    _type.clazz match {
      case BooleanTypeKey => IntTypeC.intType
      case ObjectTypeC.ObjectTypeKey => ObjectTypeC.stackObjectType(constantPool.getClassRef(ObjectTypeC.getObjectTypeName(_type).right.get))
      case _ => _type
    }
  }

  def getTypeSize(_type: MetaObject, state: TransformationState): Int = getState(state).stackSize(_type.clazz)

  def getByteCodeString(state: TransformationState): MetaObject => String =
    _type => getState(state).toByteCodeString(_type.clazz)(_type)

  override def dependencies: Set[Contract] = Set.empty

  def checkAssignableTo(state: TransformationState)(to: MetaObject, from: MetaObject) = {
    if (!isAssignableTo(state)(to, from))
      throw new TypeMismatchException(to, from)
  }

  def isAssignableTo(state: TransformationState)(to: MetaObject, from: MetaObject): Boolean = {
    val fromSuperTypes = getSuperTypes(state)(from)
    if (to.equals(from))
      return true

    fromSuperTypes.exists(_type => _type.equals(to))
  }

  def getSuperTypes(state: TransformationState)(_type: MetaObject) = getSuperTypesRegistry(state)(_type.clazz)(_type)

  def getSuperTypesRegistry(state: TransformationState) = {
    getState(state).superTypes
  }

  def union(state: TransformationState)(first: MetaObject, second: MetaObject): MetaObject = {
    val filteredDepths = getAllSuperTypes(state)(first).map(depthTypes => depthTypes.filter(_type => isAssignableTo(state)(_type, second)))
    val resultDepth = filteredDepths.find(depth => depth.nonEmpty).getOrElse(throw new NoCommonSuperTypeException(first, second))
    if (resultDepth.size > 1)
      throw new AmbiguousCommonSuperTypeException(first, second)

    resultDepth.head
  }

  def getAllSuperTypes(state: TransformationState)(_type: MetaObject): Stream[Set[MetaObject]] = {
    var returnedTypes = Set.empty[MetaObject]
    Stream.iterate(Set(_type))(previousDepthTypes => {
      val result = previousDepthTypes.flatMap(_type => getSuperTypes(state)(_type)).filter(_type => !returnedTypes.contains(_type))
      returnedTypes ++= result
      result
    })
  }

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    grammars.create(TypeGrammar)
  }

  def getState(state: TransformationState) = state.data.getOrElseUpdate(this, new State()).asInstanceOf[State]
  class State {
    val superTypes = new mutable.HashMap[Any, MetaObject => Seq[MetaObject]]()
    val toByteCodeString = new mutable.HashMap[Any, MetaObject => String]()
    val stackSize = new mutable.HashMap[Any, Int]()
  }

  object TypeGrammar

}
