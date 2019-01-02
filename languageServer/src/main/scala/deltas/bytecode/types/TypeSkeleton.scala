package deltas.bytecode.types

import core.bigrammar.printer.BiGrammarToPrinter
import core.deltas._
import core.language.exceptions.BadInputException
import core.deltas.grammars.LanguageGrammars
import core.language.node._
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type

object TypeSkeleton extends DeltaWithGrammar {
  def getType(compilation: Compilation, builder: ConstraintBuilder, _type: NodeLike, parentScope: Scope): Type = {
    hasTypes(compilation, _type.shape).getType(compilation, builder, _type, parentScope)
  }

  def toStackType(_type: Node, language: Language) : Node = {
    byteCodeInstances(language, _type.shape).getStackType(_type, language)
  }

  def getTypeSize(_type: Node, language: Language): Int = hasStackSize(language, _type.shape)

  def getByteCodeString(state: Language)(_type: Node): String = {
      val grammar = state.grammars.find(TypeSkeleton.ByteCodeTypeGrammar)
      BiGrammarToPrinter.toDocument(_type, grammar).renderString()
  }

  val hasTypes = new ShapeProperty[HasTypeDelta]
  override def dependencies: Set[Contract] = Set.empty

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

  def getSuperTypes(language: Language)(_type: Node): Seq[Node] = hasSuperTypes(language, _type.shape).getSuperTypes(_type)

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

  val byteCodeInstances = new ShapeProperty[ByteCodeTypeInstance]

  trait HasSuperTypes {
    def getSuperTypes(node: Node): Seq[Node]
  }

  val hasSuperTypes = new ShapeProperty[HasSuperTypes]
  val hasStackSize = new ShapeProperty[Int]
  val typeInstances = new ShapeProperty[TypeInstance]

  object JavaTypeGrammar extends GrammarKey

  override def description: String = "Defines the concept of a type."
}

class TypeMismatchException(to: Node, from: Node) extends BadInputException {
  override def toString = s"cannot assign: $to = $from"
}

class NoCommonSuperTypeException(first: Node, second: Node) extends BadInputException

class AmbiguousCommonSuperTypeException(first: Node, second: Node) extends BadInputException