package deltas.solidity

import core.bigrammar.BiGrammar
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node.{Node, NodeField, NodeWrapper}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import deltas.ConstraintSkeleton
import deltas.HasNameDelta.Name
import deltas.bytecode.types.TypeSkeleton
import deltas.javac.classes.skeleton.HasConstraintsDelta
import deltas.javac.methods.MethodDelta.Method
import deltas.javac.methods.MethodParameters.MethodParameter
import deltas.javac.methods.call.CallDelta
import deltas.javac.methods.{MethodDelta, MethodParameters}
import deltas.statement.BlockDelta

object SolidityFunctionDelta extends DeltaWithGrammar with HasConstraintsDelta {

  object ReturnValues extends NodeField
  object Modifiers extends NodeField

  object ParameterStorageLocation extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val typeGrammar = find(TypeSkeleton.JavaTypeGrammar)
    val storageLocation: BiGrammar = find(StorageLocationDelta.StorageLocation)
    val parameter = typeGrammar.as(MethodParameters.Type) ~
      storageLocation ~~
      find(Name) asNode MethodParameters.Shape
    val parameterList = create(MethodDelta.Parameters, parameter.toParameterList)

    val returnParameter = typeGrammar.as(MethodParameters.Type) ~
      storageLocation ~
      identifier.spacedOption.as(Name) asNode MethodParameters.Shape
    val returnParameterList = returnParameter.toParameterList

    val name = (identifier | value("<default>")).as(Name)

    val modifierInvocation = find(Name) ~
      (find(CallDelta.CallArgumentsGrammar) | value(Seq.empty)).as(CallDelta.Arguments) asNode CallDelta.Shape

    val stateMutability = find(StateMutabilityDelta.Grammar)
    val modifiers = create(Modifiers, (printSpace ~> (modifierInvocation | stateMutability | "external" | "public" | "internal" | "private")).many.as(Modifiers))
    val returnValues = (printSpace ~ "returns" ~~> returnParameterList | value(Seq.empty)).as(ReturnValues)
    val blockGrammar: BiGrammar = find(BlockDelta.BlockGramar)
    val body = (";" ~> value(BlockDelta.neww(Seq.empty)) | blockGrammar).as(MethodDelta.Body)
    val grammar = "function" ~~ name ~ parameterList.as(MethodDelta.Parameters) ~ modifiers ~ returnValues ~~ body asLabelledNode MethodDelta.Shape
    find(SolidityContractDelta.Members).addAlternative(grammar)
  }

  override def description = "Adds solidity functions"

  override def dependencies = Set(TypeSkeleton, BlockDelta, StorageLocationDelta)

  override def shape = MethodDelta.Shape

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
    val method: Method[NodePath] = path

    val parameterTypes = method.parameters.map(p => p(MethodParameters.Type).asInstanceOf[NodePath])
    val returnParameters: Seq[MethodParameter[NodePath]] = NodeWrapper.wrapList(method(ReturnValues).asInstanceOf[Seq[NodePath]])
    val returnTypes: Seq[Node] = returnParameters.map(returnParameter => returnParameter._type)
    val methodType = SolidityFunctionTypeDelta.createType(compilation, builder, parentScope, parameterTypes, returnTypes)

    builder.declare(method.name, parentScope, path.getSourceElement(Name), Some(methodType))

    val bodyScope = builder.newScope(Some(parentScope))
    method.parameters.foreach(parameter => {
      MethodParameters.declare(compilation, builder, parameter, parentScope, bodyScope)
    })
    ConstraintSkeleton.constraints(compilation, builder, method.body, bodyScope)
  }
}


