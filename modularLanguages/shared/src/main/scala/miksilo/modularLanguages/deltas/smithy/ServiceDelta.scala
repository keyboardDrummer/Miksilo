package miksilo.modularLanguages.deltas.smithy

import miksilo.modularLanguages.core.bigrammar.grammars.Keyword
import miksilo.modularLanguages.core.deltas.DeltaWithGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.modularLanguages.core.node.{NodeField, NodeLike, NodeShape, NodeWrapper}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.PrimitiveType
import miksilo.modularLanguages.deltas.expression.{ArrayLiteralDelta, StringLiteralDelta}
import miksilo.modularLanguages.deltas.expression.ArrayLiteralDelta.ArrayLiteral
import miksilo.modularLanguages.deltas.json.JsonObjectLiteralDelta.ObjectLiteral
import miksilo.modularLanguages.deltas.json.{JsonObjectLiteralDelta, JsonStringLiteralDelta}
import miksilo.modularLanguages.deltas.{FileWithMembersDelta, HasNameDelta}
import miksilo.modularLanguages.core.deltas.path.ConstraintBuilderExtension._
import miksilo.modularLanguages.deltas.classes.HasConstraintsDelta

object ServiceDelta extends DeltaWithGrammar with HasConstraintsDelta {

  object Shape extends NodeShape
  object Body extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val objectExpression = find(JsonObjectLiteralDelta.Shape)
    val grammar = Keyword("service", reserved = false) ~~
      identifier.as(HasNameDelta.Name) ~~ objectExpression.as(Body) asNode Shape
    val members = find(FileWithMembersDelta.Members)
    members.addAlternative(grammar)
  }

  val serviceType = PrimitiveType("service")

  implicit class Service[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def body: ObjectLiteral[T] = node(Body).asInstanceOf[T]
  }

  override def description = "Add the service statement"

  override def dependencies = Set(FileWithMembersDelta)

  override def shape = Shape

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
    val service: Service[NodePath] = path

    builder.declare(path.getField(HasNameDelta.Name), parentScope, serviceType)

    ResourceDelta.addResources(builder, parentScope, service.body)

    val operationsOption = service.body.get("operations")
    operationsOption.foreach(resources => {
      if (resources.shape == ArrayLiteralDelta.Shape) {
        val resourcesArray: ArrayLiteral[NodePath] = resources
        resourcesArray.members.foreach(resource => {
          if (resource.shape == StringLiteralDelta.Shape) {
            builder.resolveToType(resource.getField(StringLiteralDelta.Value), parentScope, OperationDelta.operationType)
          }
        })
      }
    })
  }
}
