package transformations.types

import core.exceptions.BadInputException
import core.particles._
import core.particles.grammars.GrammarCatalogue
import core.particles.node.MetaObject
import transformations.bytecode.ByteCodeSkeleton

class TypeMismatchException(to: MetaObject, from: MetaObject) extends BadInputException {
  override def toString = s"cannot assign: $to = $from"
}

class NoCommonSuperTypeException(first: MetaObject, second: MetaObject) extends BadInputException

class AmbiguousCommonSuperTypeException(first: MetaObject, second: MetaObject) extends BadInputException

object TypeSkeleton extends ParticleWithGrammar with WithState {

  def toStackType(_type: MetaObject, state: CompilationState) : MetaObject = {
    getState(state).instances(_type.clazz).getStackType(_type, state)
  }

  def getTypeSize(_type: MetaObject, state: CompilationState): Int = getState(state).stackSize(_type.clazz)

  def getByteCodeString(state: CompilationState): MetaObject => String =
    _type => getState(state).toByteCodeString(_type.clazz)(_type)

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton)

  def checkAssignableTo(state: CompilationState)(to: MetaObject, from: MetaObject) = {
    if (!isAssignableTo(state)(to, from))
      throw new TypeMismatchException(to, from)
  }

  def isAssignableTo(state: CompilationState)(to: MetaObject, from: MetaObject): Boolean = {
    val fromSuperTypes = getSuperTypes(state)(from)
    if (to.equals(from))
      return true

    fromSuperTypes.exists(_type => _type.equals(to))
  }

  def getSuperTypes(state: CompilationState)(_type: MetaObject) = getSuperTypesRegistry(state)(_type.clazz)(_type)

  def getSuperTypesRegistry(state: CompilationState) = {
    getState(state).superTypes
  }

  def union(state: CompilationState)(first: MetaObject, second: MetaObject): MetaObject = {
    val filteredDepths = getAllSuperTypes(state)(first).map(depthTypes => depthTypes.filter(_type => isAssignableTo(state)(_type, second)))
    val resultDepth = filteredDepths.find(depth => depth.nonEmpty).getOrElse(throw new NoCommonSuperTypeException(first, second))
    if (resultDepth.size > 1)
      throw new AmbiguousCommonSuperTypeException(first, second)

    resultDepth.head
  }

  def getAllSuperTypes(state: CompilationState)(_type: MetaObject): Stream[Set[MetaObject]] = {
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

  def createState = new State
  
  class State {
    val superTypes = new ClassRegistry[MetaObject => Seq[MetaObject]]()
    val toByteCodeString = new ClassRegistry[MetaObject => String]()
    val stackSize = new ClassRegistry[Int]()
    val instances = new ClassRegistry[TypeInstance]
  }

  object TypeGrammar

  override def description: String = "Defines the concept of a type."
}
