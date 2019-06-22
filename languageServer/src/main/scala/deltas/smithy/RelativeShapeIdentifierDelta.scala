package deltas.smithy

import core.bigrammar.TokenTypes
import core.bigrammar.grammars.{Colorize, RegexGrammar}
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node.{GrammarKey, NodeField, NodeLike, NodeShape, NodeWrapper}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.objects.DeclarationVariable
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.{PrimitiveType, Type}
import deltas.javac.classes.skeleton.HasConstraintsDelta

object RelativeShapeIdentifierDelta extends DeltaWithGrammar with HasConstraintsDelta {

  object ShapeIdentifierGrammar extends GrammarKey
  object Shape extends NodeShape
  object Value extends NodeField
  object Accessor extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val myIdentifier = Colorize(RegexGrammar("[A-Za-z][A-Za-z0-9_]*".r, "identifier"), "variable")
    val relativeShapeId = myIdentifier.as(Value) ~ ("$" ~> myIdentifier.as(Accessor)).option asLabelledNode Shape
    create(ShapeIdentifierGrammar, relativeShapeId)
  }

  implicit class RelativeShape[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def value: String = node.getValue(Value).asInstanceOf[String]
    def accessor: Option[String]= node.getValue(Accessor).asInstanceOf[Option[String]]
  }

  val shapeType = PrimitiveType("shape")

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder,
                                  path: NodePath, parentScope: Scope): Unit = {
    getDeclaration(builder, path, parentScope, shapeType)
  }

  def getDeclaration(builder: ConstraintBuilder, path: NodePath, parentScope: Scope, _type: Type): DeclarationVariable = {
    val relativeShape: RelativeShape[NodePath] = path
    val outer = builder.resolveToType(path.getField(Value), parentScope, _type)
    relativeShape.get(Accessor) match {
      case None => outer
      case Some(_) =>
        val scope = builder.getDeclaredScope(outer)
        builder.resolveToType(path.getField(Accessor), scope, _type)
    }
  }

  override def dependencies = Set.empty

  override def shape = Shape

  override def description = "Adds the relative shape identifiers"
}
