package miksilo.modularLanguages.deltas.solidity

import miksilo.modularLanguages.core.deltas.DeltaWithGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.modularLanguages.core.node.{NodeGrammar, NodeShape}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.TypeApplication
import miksilo.languageServer.core.smarts.{ConstraintBuilder, ResolvesToType}
import miksilo.modularLanguages.deltas.expression.ExpressionDelta
import miksilo.modularLanguages.deltas.javac.classes.skeleton.HasConstraintsDelta
import miksilo.modularLanguages.deltas.javac.methods.call.CallDelta.Call
import miksilo.modularLanguages.deltas.javac.methods.call.{CallDelta, ReferenceExpressionSkeleton}
import miksilo.modularLanguages.deltas.statement.StatementDelta

object EmitStatementDelta extends DeltaWithGrammar with HasConstraintsDelta {

  object Shape extends NodeShape

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val call = find(CallDelta.Shape)
    val callGrammar = call.inner.asInstanceOf[NodeGrammar].inner
    val grammar = "emit" ~~ callGrammar ~ ";" asNode Shape
    find(StatementDelta.Grammar).addAlternative(grammar)
  }

  override def description = "Adds the emit statement"

  override def dependencies = Set(StatementDelta, CallDelta)

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
    val call: Call[NodePath] = path
    val methodReference = ReferenceExpressionSkeleton.getReference(compilation, builder, call.callee, parentScope)

    val callTypes = call.arguments.map(argument => ExpressionDelta.getType(compilation, builder, argument, parentScope))
    val functionType = TypeApplication(EventDelta.eventConstructor, callTypes, call)
    builder.add(new ResolvesToType(methodReference, builder.declarationVariable(), functionType))

    ExpressionDelta.constraintType(call.callee) = functionType
  }

  override def shape = Shape
}
