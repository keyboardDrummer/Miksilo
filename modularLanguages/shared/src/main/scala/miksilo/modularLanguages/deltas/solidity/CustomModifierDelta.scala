package miksilo.modularLanguages.deltas.solidity

import miksilo.modularLanguages.core.bigrammar.BiGrammar
import miksilo.modularLanguages.core.deltas.DeltaWithGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.modularLanguages.core.node.NodeShape
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.{FunctionType, Type}
import miksilo.modularLanguages.deltas.ConstraintSkeleton
import miksilo.modularLanguages.deltas.HasNameDelta.Name
import miksilo.modularLanguages.deltas.bytecode.types.{TypeSkeleton, VoidTypeDelta}
import miksilo.modularLanguages.deltas.expression.{ExpressionDelta, IsExpression}
import miksilo.modularLanguages.deltas.statement.{BlockDelta, LabelStatementDelta}
import miksilo.modularLanguages.core.deltas.path.ConstraintBuilderExtension._
import miksilo.modularLanguages.deltas.classes.ClassDelta
import miksilo.modularLanguages.deltas.classes.HasConstraintsDelta
import miksilo.modularLanguages.deltas.javac.methods.MethodParameters
import miksilo.modularLanguages.deltas.javac.methods.MethodParameters.MethodParameter
import miksilo.modularLanguages.deltas.method.MethodDelta
import miksilo.modularLanguages.deltas.method.MethodDelta.Method

object CustomModifierDelta extends DeltaWithGrammar with HasConstraintsDelta {

  object Shape extends NodeShape

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val parameterList = find(MethodDelta.Parameters)

    val blockGrammar: BiGrammar = find(BlockDelta.BlockGrammar)
    val body = blockGrammar.as(MethodDelta.Body)
    val optionalParameters = (parameterList | value(Seq.empty)).as(MethodDelta.Parameters)
    val grammar = "modifier" ~~ find(Name) ~
      optionalParameters ~~ body asNode Shape
    find(ClassDelta.Members).addAlternative(grammar)

    val underscoreGrammar = keywordGrammar("_") asNode UnderScoreShape
    find(ExpressionDelta.LastPrecedenceGrammar).addAlternative(underscoreGrammar)
  }


  override def inject(language: Language): Unit = {
    LabelStatementDelta.isLabelScope.add(language, Shape, ())
    ExpressionDelta.expressionInstances.add(language, UnderScoreShape, new IsExpression {
      override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = {
      }
    })
    super.inject(language)
  }

  object UnderScoreShape extends NodeShape

  override def description = "Adds solidity custom modifiers"

  override def dependencies = Set(SolidityFunctionDelta, BlockDelta)

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
    val method: Method[NodePath] = path
    val parameterTypes = method.parameters.map(parameter => {
      val methodParameter: MethodParameter[NodePath] = parameter
      TypeSkeleton.getType(compilation, builder, methodParameter._type, parentScope)
    })
    val methodType = FunctionType.curry(parameterTypes, VoidTypeDelta.constraintType)
    builder.declare(path.getField(Name), parentScope, methodType)

    val bodyScope = builder.newScope(parentScope, "methodBody")
    method.parameters.foreach(parameter => {
      MethodParameters.declare(compilation, builder, parameter, parentScope, bodyScope)
    })
    ConstraintSkeleton.constraints(compilation, builder, method.body, bodyScope)
  }

  override def shape = Shape
}


