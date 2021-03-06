package miksilo.modularLanguages.deltas.bytecode.types

import miksilo.modularLanguages.core.bigrammar.printer.BiGrammarToPrinter
import miksilo.modularLanguages.core.deltas._
import miksilo.languageServer.core.language.exceptions.BadInputException
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.node._
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.{PrimitiveType, Type, TypeApplication, TypeFromDeclaration}

import scala.collection.mutable

object TypeSkeleton extends DeltaWithGrammar {

  val typeKind = PrimitiveType("type")
  val maps = new mutable.HashMap[Any, Type => Node]
  def fromConstraintType(_type: Type): Node = {
    def getName(_type: Type): Any = _type match {
      case PrimitiveType(primitiveType) => primitiveType
      case TypeApplication(constructor, _, _) => getName(constructor)
      case TypeFromDeclaration(_) => "DECLARATION"
    }

    val name = getName(_type)
    maps(name)(_type)
  }

  def getType(compilation: Compilation, builder: ConstraintBuilder, _type: NodeLike, parentScope: Scope): Type = {
    hasTypes(compilation, _type.shape).getType(compilation, builder, _type, parentScope)
  }

  def toStackType(_type: Node, language: Language) : Node = {
    byteCodeInstances(language, _type.shape).getStackType(_type, language)
  }

  def getTypeSize(_type: Node, language: Language): Int = hasStackSize(language, _type.shape)

  def getByteCodeString(language: Language)(_type: Node): String = {
      val grammar = LanguageGrammars.grammars.get(language).find(TypeSkeleton.ByteCodeTypeGrammar)
      BiGrammarToPrinter.toDocument(_type, grammar).renderString()
  }

  val hasTypes = new ShapeProperty[HasType]
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

  def getAllSuperTypes(state: Language)(_type: Node): LazyList[Set[Node]] = {
    var returnedTypes = Set.empty[Node]
    LazyList.iterate(Set(_type))(previousDepthTypes => {
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

  val byteCodeInstances = new ShapeProperty[ByteCodeTypeInstance]()

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