package deltas.solidity

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node.{NodeGrammar, NodeShape}
import core.language.{Compilation, Language}
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.TypeApplication
import core.smarts.{ConstraintBuilder, ResolvesToType}
import deltas.expression.ExpressionDelta
import deltas.javac.classes.skeleton.HasConstraintsDelta
import deltas.javac.methods.call.CallDelta.Call
import deltas.javac.methods.call.{CallDelta, ReferenceExpressionSkeleton}
import deltas.statement.StatementDelta

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
