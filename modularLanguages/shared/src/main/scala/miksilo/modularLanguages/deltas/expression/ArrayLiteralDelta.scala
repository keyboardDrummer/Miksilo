package miksilo.modularLanguages.deltas.expression

import miksilo.modularLanguages.core.bigrammar.grammars.Delimiter
import miksilo.modularLanguages.core.deltas.DeltaWithGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.modularLanguages.core.node.{NodeField, NodeShape, _}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.editorParser.parsers.editorParsers.History
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.Type
import miksilo.modularLanguages.deltas.json.JsonObjectLiteralDelta

object ArrayLiteralDelta extends DeltaWithGrammar with ExpressionInstance {

  override def description: String = "Adds the array literal to expressions"

  def create(elements: Seq[Node]) = Shape.create(Members -> elements)

  implicit class ArrayLiteral[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def members: Seq[T] = node(Members).asInstanceOf[Seq[T]]
  }

  override def transformGrammars(_grammars: LanguageGrammars, language: Language): Unit = {
    val grammars = _grammars
    import _grammars._

    val expression = find(ExpressionDelta.FirstPrecedenceGrammar)
    val array = Delimiter("[", History.missingInputPenalty * 2) ~ expression.manySeparated(",").as(Members) ~ "]"
    val grammar = array.asLabelledNode(Shape)
    expression.addAlternative(grammar)
  }

  object Members extends NodeField  {
    override def toString = "Members"
  }
  object Shape extends NodeShape {
    override def toString = "Array"
  }
  override def shape: NodeShape = Shape

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = {

  }
}
