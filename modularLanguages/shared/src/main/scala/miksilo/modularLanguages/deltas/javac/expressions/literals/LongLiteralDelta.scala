package miksilo.modularLanguages.deltas.javac.expressions.literals

import miksilo.modularLanguages.core.bigrammar.BiGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.modularLanguages.core.deltas.{Contract, DeltaWithGrammar}
import miksilo.modularLanguages.core.node.{Node, NodeField, NodeShape}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.Type
import miksilo.modularLanguages.deltas.bytecode.types.LongTypeDelta
import miksilo.modularLanguages.deltas.expression.{ExpressionDelta, ExpressionInstance}

object LongLiteralDelta extends DeltaWithGrammar with ExpressionInstance {
  val shape = Shape

  override def dependencies: Set[Contract] = Set(ExpressionDelta)

  private def parseLong(number: String) = java.lang.Long.parseLong(number.dropRight(1))

  override def transformGrammars(_grammars: LanguageGrammars, state: Language): Unit = {
    val grammars = _grammars
    import grammars._
    val longGrammar: BiGrammar = grammars.regexGrammar("""-?\d+l""".r, "long literal").map[String, Long](
      number => parseLong(number), l => s"${l}l") as ValueKey asLabelledNode Shape
    val expressionGrammar = find(ExpressionDelta.FirstPrecedenceGrammar)
    expressionGrammar.addAlternative(longGrammar)
  }

  def literal(value: Long) = new Node(Shape, ValueKey -> value)

  def getValue(literal: Node): Long = literal(ValueKey).asInstanceOf[Long]

  object Shape extends NodeShape

  object ValueKey extends NodeField

  override def description: String = "Adds the usage of long literals by putting an l after the number."

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = {
    builder.typesAreEqual(_type, LongTypeDelta.constraintType)
  }
}
