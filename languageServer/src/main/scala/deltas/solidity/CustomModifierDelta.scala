package deltas.solidity

import core.bigrammar.BiGrammar
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node.NodeShape
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.FunctionType
import deltas.ConstraintSkeleton
import deltas.HasNameDelta.Name
import deltas.bytecode.types.{TypeSkeleton, VoidTypeDelta}
import deltas.javac.classes.skeleton.HasConstraintsDelta
import deltas.javac.methods.MethodDelta.Method
import deltas.javac.methods.MethodParameters.MethodParameter
import deltas.javac.methods.{MethodDelta, MethodParameters}
import deltas.statement.BlockDelta

object CustomModifierDelta extends DeltaWithGrammar with HasConstraintsDelta {

  object Shape extends NodeShape

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val parameterList = find(MethodDelta.Parameters)

    val blockGrammar: BiGrammar = find(BlockDelta.BlockGramar)
    val body = blockGrammar.as(MethodDelta.Body)
    val optionalParameters = (parameterList | value(Seq.empty)).as(MethodDelta.Parameters)
    val grammar = "modifier" ~~ find(Name) ~
      optionalParameters ~~ body asNode Shape
    find(SolidityContractDelta.Members).addAlternative(grammar)
  }

  override def description = "Adds solidity custom modifiers"

  override def dependencies = Set(SolidityFunctionDelta, BlockDelta)

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
    val method: Method[NodePath] = path
    val parameterTypes = method.parameters.map(parameter => {
      val methodParameter: MethodParameter[NodePath] = parameter
      TypeSkeleton.getType(compilation, builder, methodParameter._type, parentScope)
    })
    val methodType = FunctionType.curry(parameterTypes, VoidTypeDelta.constraintType)
    builder.declareSourceElement(path.getSourceElement(Name), parentScope, Some(methodType))

    val bodyScope = builder.newScope(Some(parentScope), "methodBody")
    method.parameters.foreach(parameter => {
      MethodParameters.declare(compilation, builder, parameter, parentScope, bodyScope)
    })
    ConstraintSkeleton.constraints(compilation, builder, method.body, bodyScope)
  }

  override def shape = Shape
}


