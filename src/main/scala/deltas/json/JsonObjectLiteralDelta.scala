package deltas.json

import core.bigrammar.grammars.{Keyword, Parse, StringLiteral}
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node.{Node, NodeField, NodeShape}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.javac.expressions.{ExpressionInstance, ExpressionSkeleton}

object JsonObjectLiteralDelta extends ExpressionInstance {

  override def description: String = "Adds the JSON object literal to expressions"

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._

    val expressionGrammar = find(ExpressionSkeleton.ExpressionGrammar)
    val member = StringLiteral.as(MemberKey) ~ ":" ~~ expressionGrammar.as(MemberValue) asNode MemberShape
    val inner = "{" % (member.manySeparatedVertical(",").as(Members) ~ Parse(Keyword(",") | value(Unit))).indent() % "}"
    val grammar = inner.asLabelledNode(Shape)
    expressionGrammar.addAlternative(grammar)
  }

  override def getType(expression: NodePath, compilation: Compilation): Node = ???

  object MemberValue extends NodeField
  object MemberKey extends NodeField
  object MemberShape extends NodeShape

  object Members extends NodeField
  object Shape extends NodeShape
  override def shape: NodeShape = Shape

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = {

  }
}
