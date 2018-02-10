package deltas.bytecode.types

import core.bigrammar.printer.BiGrammarToPrinter
import core.deltas._
import core.deltas.exceptions.BadInputException
import core.deltas.grammars.LanguageGrammars
import core.deltas.node._
import core.language.Language
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.bytecode.ByteCodeSkeleton

class TypeMismatchException(to: Node, from: Node) extends BadInputException {
  override def toString = s"cannot assign: $to = $from"
}

class NoCommonSuperTypeException(first: Node, second: Node) extends BadInputException

class AmbiguousCommonSuperTypeException(first: Node, second: Node) extends BadInputException

object TypeSkeleton extends DeltaWithGrammar with WithLanguageRegistry {
  def getType(compilation: Compilation, builder: ConstraintBuilder, _type: NodeLike, parentScope: Scope): Type = {
    hasTypes.get(compilation, _type.shape).getType(compilation, builder, _type, parentScope)
  }

  def toStackType(_type: Node, language: Language) : Node = {
    getInstance(_type, language).getStackType(_type, language)
  }

  private def getInstance(_type: NodeLike, language: Language) = {
    getRegistry(language).instances(_type.shape)
  }

  def getTypeSize(_type: Node, language: Language): Int = getRegistry(language).stackSize(_type.shape)

  def getByteCodeString(state: Language)(_type: Node): String = {
      val grammar = state.grammars.find(TypeSkeleton.ByteCodeTypeGrammar)
      val rendered = BiGrammarToPrinter.toDocument(_type, grammar).renderString()
      rendered
  }

  val hasTypes = new ShapeProperty[HasType]
  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton)

  def checkAssignableTo(language: Language)(to: Node, from: Node): Unit = {
    if (!isAssignableTo(language)(to, from))
      throw new TypeMismatchException(to, from)
  }

  def isAssignableTo(language: Language)(to: Node, from: Node): Boolean = {
    val fromSuperTypes = getSuperTypes(language)(from)
    if (to.equals(from))
      return true

    fromSuperTypes.exists(_type => _type.equals(to))
  }

  def getSuperTypes(state: Language)(_type: Node) = getSuperTypesRegistry(state)(_type.shape)(_type)

  def getSuperTypesRegistry(state: Language) = {
    getRegistry(state).superTypes
  }

  def union(state: Language)(first: Node, second: Node): Node = {
    val filteredDepths = getAllSuperTypes(state)(first).map(depthTypes => depthTypes.filter(_type => isAssignableTo(state)(_type, second)))
    val resultDepth = filteredDepths.find(depth => depth.nonEmpty).getOrElse(throw new NoCommonSuperTypeException(first, second))
    if (resultDepth.size > 1)
      throw new AmbiguousCommonSuperTypeException(first, second)

    resultDepth.head
  }

  def getAllSuperTypes(state: Language)(_type: Node): Stream[Set[Node]] = {
    var returnedTypes = Set.empty[Node]
    Stream.iterate(Set(_type))(previousDepthTypes => {
      val result = previousDepthTypes.flatMap(_type => getSuperTypes(state)(_type)).diff(returnedTypes)
      returnedTypes ++= result
      result
    })
  }

  object ByteCodeTypeGrammar extends GrammarKey
  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    grammars.create(JavaTypeGrammar)
    grammars.create(ByteCodeTypeGrammar)
  }

  def createRegistry = new Registry
  
  class Registry {
    val superTypes = new ShapeRegistry[Node => Seq[Node]]()
    val stackSize = new ShapeRegistry[Int]()
    val instances = new ShapeRegistry[TypeInstance]
  }

  object JavaTypeGrammar extends GrammarKey

  override def description: String = "Defines the concept of a type."
}
