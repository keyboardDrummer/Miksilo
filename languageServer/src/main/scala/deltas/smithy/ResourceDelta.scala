package deltas.smithy

import core.bigrammar.grammars.Keyword
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node.{NodeLike, NodeShape, NodeWrapper}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.PrimitiveType
import deltas.expression.{ArrayLiteralDelta, StringLiteralDelta}
import deltas.expression.ArrayLiteralDelta.ArrayLiteral
import deltas.javac.classes.skeleton.HasConstraintsDelta
import deltas.json.JsonObjectLiteralDelta.ObjectLiteral
import deltas.json.{JsonObjectLiteralDelta, JsonStringLiteralDelta}
import deltas.smithy.ServiceDelta.Body
import deltas.{FileWithMembersDelta, HasNameDelta}

object ResourceDelta extends DeltaWithGrammar with HasConstraintsDelta {
  object Shape extends NodeShape

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val objectExpression = find(JsonObjectLiteralDelta.Shape)
    val grammar = Keyword("resource", reserved = false) ~~
      find(HasNameDelta.Name) ~~ objectExpression.as(Body) asNode Shape
    val members = find(FileWithMembersDelta.Members)
    members.addAlternative(grammar)
  }

  override def shape = Shape

  implicit class Resource[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def body: ObjectLiteral[T] = node(Body).asInstanceOf[T]
  }

  val resourceType = PrimitiveType("resource")
  val lifeCycleOperations = Seq("create","read","update","delete","list")
  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
    val resource: Resource[NodePath] = path
    builder.declare(path.getField(HasNameDelta.Name), parentScope, resourceType)

    val body = resource.body
    addResources(builder, parentScope, body)

    val identifiersOption = resource.body.get("identifiers")
    identifiersOption.foreach(identifiers => {
      if (identifiers.shape == JsonObjectLiteralDelta.Shape) {
        val identifiersObject: ObjectLiteral[NodePath] = identifiers
        identifiersObject.members.foreach(identifierMember => {
          identifierMember.value
          val shapeReference = identifierMember.value
          if (shapeReference.shape == StringLiteralDelta.Shape) {
            builder.resolveToType(shapeReference.getField(JsonStringLiteralDelta.Value), parentScope,
              RelativeShapeIdentifierDelta.shapeType)
          }
        })
      }
    })

    lifeCycleOperations.foreach(lifeCycleOperation => {
      val operationOption = resource.body.get(lifeCycleOperation)
      operationOption.foreach(operation => {
        if (operation.shape == StringLiteralDelta.Shape) {
          builder.resolveToType(operation.getField(JsonStringLiteralDelta.Value), parentScope,
            OperationDelta.operationType)
        }
      })
    })
  }

  def addResources(builder: ConstraintBuilder, parentScope: Scope, body: ObjectLiteral[NodePath]): Unit = {
    val resourcesOption = body.get("resources")
    resourcesOption.foreach(resources => {
      if (resources.shape == ArrayLiteralDelta.Shape) {
        val resourcesArray: ArrayLiteral[NodePath] = resources
        resourcesArray.members.foreach(resource => {
          if (resource.shape == StringLiteralDelta.Shape) {
            builder.resolveToType(resource.getField(JsonStringLiteralDelta.Value), parentScope, resourceType)
          }
        })
      }
    })
  }

  override def description = "Add the resource statement"

  override def dependencies = Set(FileWithMembersDelta)
}
