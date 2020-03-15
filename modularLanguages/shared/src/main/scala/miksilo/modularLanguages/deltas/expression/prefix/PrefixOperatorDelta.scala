package miksilo.modularLanguages.deltas.expression.prefix

import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.modularLanguages.core.deltas.{Contract, DeltaWithGrammar}
import miksilo.modularLanguages.core.node.{NodeField, NodeShape}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.Type
import miksilo.modularLanguages.deltas.expression.{ExpressionDelta, ExpressionInstance}

trait PrefixOperatorDelta extends DeltaWithGrammar with ExpressionInstance {

  def keyword: String

  override val shape = Shape

  override def dependencies: Set[Contract] = Set(ExpressionDelta)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val expressionCore = find(ExpressionDelta.LastPrecedenceGrammar)
    val prefixOperator = keyword ~> expressionCore.as(Target) asNode Shape
    expressionCore.addAlternative(prefixOperator)
  }

  object Shape extends NodeShape
  object Target extends NodeField

  override def description: String = s"Adds the prefix $keyword operator."

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = {
    ExpressionDelta.addConstraints(compilation, builder, expression(Target).asInstanceOf[NodePath], _type, parentScope)
  }
}
