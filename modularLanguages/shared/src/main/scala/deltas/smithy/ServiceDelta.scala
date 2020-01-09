package deltas.smithy

import core.bigrammar.grammars.Keyword
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node.{NodeField, NodeLike, NodeShape, NodeWrapper}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.PrimitiveType
import deltas.expression.{ArrayLiteralDelta, StringLiteralDelta}
import deltas.expression.ArrayLiteralDelta.ArrayLiteral
import deltas.json.JsonObjectLiteralDelta.ObjectLiteral
import deltas.json.{JsonObjectLiteralDelta, JsonStringLiteralDelta}
import deltas.{FileWithMembersDelta, HasNameDelta}
import core.deltas.path.ConstraintBuilderExtension._
import deltas.javac.classes.skeleton.HasConstraintsDelta

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
            builder.resolveToType(resource.getField(JsonStringLiteralDelta.Value), parentScope, OperationDelta.operationType)
          }
        })
      }
    })
  }
}
