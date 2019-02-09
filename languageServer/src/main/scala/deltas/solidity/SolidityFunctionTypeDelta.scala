package deltas.solidity

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.node._
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.{FunctionType, PrimitiveType, Type, TypeApplication}
import deltas.bytecode.types.{TypeInstance, TypeSkeleton, VoidTypeDelta}
import deltas.javac.methods.MethodParameters
import deltas.javac.types.MethodTypeDelta.ReturnType

object SolidityFunctionTypeDelta extends DeltaWithGrammar with TypeInstance {

  object Shape extends NodeShape
  object Parameters extends NodeField
  object ReturnTypes extends NodeField

  object ParameterShape extends NodeShape
  object ParameterStorageLocation extends NodeField

  implicit class TypeParameter[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def _type: T = node(MethodParameters.Type).asInstanceOf[T]
    def _type_=(value: T): Unit = node(MethodParameters.Type) = value
  }

  implicit class SolidityFunctionType[T <: NodeLike](node: T) {
    def returnParameters: Seq[T] = node(ReturnType).asInstanceOf[Seq[T]]
    def returnParameters_=(value: Seq[T]): Unit = node(ReturnType) = value

    def parameterTypes: Seq[T] = node(Parameters).asInstanceOf[Seq[T]]
    def parameterTypes_=(value: Seq[T]): Unit = node(Parameters) = value
  }

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val typeGrammar = find(TypeSkeleton.JavaTypeGrammar)
    val storageLocation = find(StorageLocationDelta.StorageLocation)
    val parameter = typeGrammar.as(MethodParameters.Type) ~ storageLocation.as(ParameterStorageLocation) asNode ParameterShape
    val parameterList = parameter.toParameterList
    val modifiers = (find(StateMutabilityDelta.Grammar) | "internal" | "external").many.as(SolidityFunctionDelta.Modifiers)
    val grammar = "function" ~~ parameterList.as(Parameters) ~~ modifiers ~~ "returns" ~~ parameterList.as(ReturnTypes) asNode Shape
    typeGrammar.addAlternative(grammar)
  }

  override def description = "Add Solidity function types"

  override def dependencies = Set(TypeSkeleton)

  override def getSuperTypes(_type: Node) = Seq.empty

  override def getJavaGrammar(grammars: LanguageGrammars) = ???

  val returnList = PrimitiveType("returnList")
  override def getType(compilation: Compilation, builder: ConstraintBuilder, _type: NodeLike, parentScope: Scope): Type = {
    val parameters = _type.parameterTypes
    val returnParameters = _type.returnParameters
    createType(compilation, builder, parentScope, parameters, returnParameters)
  }

  def createType(compilation: Compilation, builder: ConstraintBuilder, parentScope: Scope,
                 parameters: Seq[NodeLike], returnParameters: Seq[NodeLike]): Type = {
    val parameterTypes = parameters.map(parameter => TypeSkeleton.getType(compilation, builder, parameter, parentScope))
    val returnTypes = returnParameters.map(returnParameter => TypeSkeleton.getType(compilation, builder, returnParameter, parentScope))
    createType(parameterTypes, returnTypes)
  }

  def createType(parameterTypes: Seq[Type], returnTypes: Seq[Type]) = {
    val returnAggregate = returnTypes.length match {
      case 0 => VoidTypeDelta.constraintType
      case 1 => returnTypes.head
      case _ => TypeApplication(returnList, returnTypes, "eariugblrig")
    }
    FunctionType.curry(parameterTypes, returnAggregate)
  }

  override def shape = Shape
}
