package miksilo.modularLanguages.deltas.expression

import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.{PrimitiveType, Type}
import miksilo.modularLanguages.core.bigrammar.grammars.StringLiteralGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.modularLanguages.core.deltas.{Contract, DeltaWithGrammar}
import miksilo.modularLanguages.core.node._

object StringLiteralDelta extends DeltaWithGrammar with ExpressionInstance {

  override def description: String = "Adds the usage of string literals."

  val shape = Shape

  override def dependencies: Set[Contract] = Set(ExpressionDelta)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val inner = StringLiteralGrammar
    val grammar = inner.as(Value).asLabelledNode(Shape)
    find(ExpressionDelta.FirstPrecedenceGrammar).addAlternative(grammar)
  }

  object Shape extends TypedShape {
    type Typed[T <: NodeLike] = StringLiteral[T]

    def neww(value: String) = this.create(Value -> value)

    override def wrap[T <: NodeLike](value: T) = new StringLiteral(value)

    override def toString = "String"
  }

  implicit class StringLiteral[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def value: String = node(Value).asInstanceOf[String]
  }

  object Value extends NodeField {
    override def toString = "Value"
  }

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = {
    builder.typesAreEqual(_type, PrimitiveType("String"))
  }
}