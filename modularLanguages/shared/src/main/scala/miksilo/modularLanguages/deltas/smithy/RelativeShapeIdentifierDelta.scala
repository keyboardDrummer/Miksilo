package miksilo.modularLanguages.deltas.smithy

import miksilo.modularLanguages.core.bigrammar.grammars.{Colorize, RegexGrammar}
import miksilo.modularLanguages.core.deltas.DeltaWithGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.modularLanguages.core.node.{GrammarKey, NodeField, NodeLike, NodeShape, NodeWrapper}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.objects.DeclarationVariable
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.{PrimitiveType, Type}
import miksilo.modularLanguages.core.deltas.path.ConstraintBuilderExtension._
import miksilo.modularLanguages.deltas.classes.HasConstraintsDelta

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
