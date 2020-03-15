package miksilo.modularLanguages.deltas.expression

import miksilo.modularLanguages.core.bigrammar.grammars.StringLiteral
import miksilo.modularLanguages.core.deltas.{Contract, DeltaWithGrammar}
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.modularLanguages.core.node.{Node, NodeField, NodeShape}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.{PrimitiveType, Type}

object StringLiteralDelta extends DeltaWithGrammar with ExpressionInstance {

  override def description: String = "Adds the usage of string literals."

  val shape = Shape

  override def dependencies: Set[Contract] = Set(ExpressionDelta)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val inner = StringLiteral
    val grammar = inner.as(Value).asLabelledNode(Shape)
    find(ExpressionDelta.FirstPrecedenceGrammar).addAlternative(grammar)
  }

  def literal(value: String) = new Node(Shape, Value -> value)

  def getValue(literal: Node): String = literal(Value).asInstanceOf[String]

  object Shape extends NodeShape {
    override def toString = "String"
  }

  object Value extends NodeField {
    override def toString = "Value"
  }

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = {
    builder.typesAreEqual(_type, PrimitiveType("String"))
  }
}