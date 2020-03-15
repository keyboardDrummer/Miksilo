package miksilo.modularLanguages.deltas.javac.expressions.literals

import miksilo.modularLanguages.core.deltas._
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.modularLanguages.core.node.{Node, NodeShape}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.Type
import miksilo.modularLanguages.deltas.expression.{ExpressionDelta, ExpressionInstance}

object NullDelta extends DeltaWithGrammar with ExpressionInstance {

  val _null = new Node(Shape)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val expressionGrammar = find(ExpressionDelta.FirstPrecedenceGrammar)
    val parseNull = "null" ~> value(_null)
    expressionGrammar.addAlternative(parseNull)
  }

  override def dependencies: Set[Contract] = Set(ExpressionDelta)

  object Shape extends NodeShape

  override val shape = Shape

  override def description: String = "Adds the usage of 'null'"

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = ???
}
