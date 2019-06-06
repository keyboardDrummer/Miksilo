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
import deltas.expression.ArrayLiteralDelta
import deltas.expression.ArrayLiteralDelta.ArrayLiteral
import deltas.javac.classes.skeleton.HasConstraintsDelta
import deltas.json.JsonObjectLiteralDelta.ObjectLiteral
import deltas.json.{JsonObjectLiteralDelta, StringLiteralDelta}
import deltas.{FileWithMembersDelta, HasNameDelta}

object ServiceOrResourceDelta extends DeltaWithGrammar with HasConstraintsDelta {

  object Shape extends NodeShape
  object Body extends NodeField
  object Kind extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val objectExpression = find(JsonObjectLiteralDelta.Shape)
    val grammar = (Keyword("service", reserved = false) | Keyword("resource", reserved = false)).as(Kind) ~~
      identifier.as(HasNameDelta.Name) ~~ objectExpression.as(Body) asNode Shape
    val members = find(FileWithMembersDelta.Members)
    members.addAlternative(grammar)
  }

  val resourceType = PrimitiveType("resource")
  val serviceType = PrimitiveType("service")

  implicit class ServiceOrResource[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def kind: String = node.getValue(Kind).asInstanceOf[String]
    def body: ObjectLiteral[T] = node(Body).asInstanceOf[T]
  }

  override def description = "Adds the namespace service statement"

  override def dependencies = Set(FileWithMembersDelta)

  override def shape = Shape

  val lifeCycleOperations = Seq("create","read","update","delete","list")
  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
    val serviceOrResource: ServiceOrResource[NodePath] = path

    val myType = if (serviceOrResource.kind == "service")
      serviceType
    else
      resourceType
    builder.declareSourceElement(path.getSourceElement(HasNameDelta.Name), parentScope, Some(myType))

    val resourcesOption = serviceOrResource.body.get("resources")
    resourcesOption.foreach(resources => {
      if (resources.shape == ArrayLiteralDelta.Shape) {
        val resourcesArray: ArrayLiteral[NodePath] = resources
        resourcesArray.members.foreach(resource => {
          if (resource.shape == StringLiteralDelta.Shape) {
            builder.resolveToType(resource.getSourceElement(StringLiteralDelta.Value), parentScope, resourceType)
          }
        })
      }
    })

    val identifiersOption = serviceOrResource.body.get("identifiers")
    identifiersOption.foreach(identifiers => {
      if (identifiers.shape == JsonObjectLiteralDelta.Shape) {
        val identifiersObject: ObjectLiteral[NodePath] = identifiers
        identifiersObject.members.foreach(identifierMember => {
          identifierMember.value
          val shapeReference = identifierMember.value
          if (shapeReference.shape == StringLiteralDelta.Shape) {
            builder.resolveToType(shapeReference.getSourceElement(StringLiteralDelta.Value), parentScope,
              RelativeShapeIdentifierDelta.shapeType)
          }
        })
      }
    })

    lifeCycleOperations.foreach(lifeCycleOperation => {
      val operationOption = serviceOrResource.body.get(lifeCycleOperation)
      operationOption.foreach(operation => {
        if (operation.shape == StringLiteralDelta.Shape) {
          builder.resolveToType(operation.getSourceElement(StringLiteralDelta.Value), parentScope,
            OperationDelta.operationType)
        }
      })
    })
  }
}
