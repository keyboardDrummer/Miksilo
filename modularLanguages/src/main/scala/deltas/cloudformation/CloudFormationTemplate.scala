package deltas.cloudformation

import core.SolveConstraintsDelta
import core.deltas.path.{NodePath, PathRoot}
import java.io.InputStream
import com.typesafe.scalalogging.LazyLogging
import core.deltas.path.NodePath
import core.deltas.{Contract, Delta}
import core.language.Language
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.ConcreteScope
import core.smarts.types.objects.PrimitiveType
import deltas.expression.StringLiteralDelta
import deltas.json.JsonObjectLiteralDelta.{MemberKey, MemberShape, ObjectLiteral, ObjectLiteralMember}
import deltas.json.{JsonObjectLiteralDelta, JsonStringLiteralDelta}
import play.api.libs.json.{JsObject, Json}
import util.{JavaSourceUtils, SourceUtils}
import core.deltas.path.ConstraintBuilderExtension._

class CloudFormationTemplate(resourceSpecificationOption: Option[InputStream]) extends Delta with LazyLogging {

  val resourceTypes = resourceSpecificationOption.fold(JsObject.empty)(resourceSpecification => {
    val resourceTypeSpecification = SourceUtils.streamToString(resourceSpecification)
    val parsedFile = Json.parse(resourceTypeSpecification).as[JsObject]
    parsedFile.value("ResourceTypes").as[JsObject]
  })

  override def description: String = "Add cloudformation template semantics"

  val propertyType = PrimitiveType("PropertyKey")
  val valueType = PrimitiveType("Value")
  override def inject(language: Language): Unit = {
    super.inject(language)

      SolveConstraintsDelta.constraintCollector.add(language, (compilation, builder) => {
      val rootScope = builder.newScope(debugName = "rootScope")

      addResourceTypesFromSchema(resourceTypes, builder, rootScope)

      addPseudoParameters(builder, rootScope)
      val root = compilation.program.asInstanceOf[PathRoot]
      if (root.shape == JsonObjectLiteralDelta.Shape) {
        val program: ObjectLiteral[NodePath] = root

        addParameters(builder, rootScope, program)
        handleResources(builder, rootScope, program)
        resolveRefs(builder, rootScope, program)
      }
    })
  }

  private def handleResources(builder: ConstraintBuilder, rootScope: ConcreteScope, program: ObjectLiteral[NodePath]): Unit = {
    val resources: Option[ObjectLiteral[NodePath]] = program.get("Resources").
      filter(v => v.shape == JsonObjectLiteralDelta.Shape).map(v => ObjectLiteral(v))
    val members = resources.fold(Seq.empty[ObjectLiteralMember[NodePath]])(o => o.members)
    for (resource <- members) {
      builder.declare(resource.getField(MemberKey), rootScope, valueType)

      if (resource.value.shape == JsonObjectLiteralDelta.Shape) {
        val resourceMembers: ObjectLiteral[NodePath] = resource.value
        val typeString = resourceMembers.getValue("Type")
        val resourceType = JsonStringLiteralDelta.getValue(typeString)
        val typeDeclaration = builder.resolve(resourceType, rootScope, typeString.getField(JsonStringLiteralDelta.Value))
        val typeScope = builder.getDeclaredScope(typeDeclaration)
        resourceMembers.get("Properties").foreach(_properties => {
          if (_properties.shape == JsonObjectLiteralDelta.Shape) {
            val properties: ObjectLiteral[NodePath] = _properties
            for (property <- properties.members) {
              if (property.key.nonEmpty)
                builder.resolveToType(property.key, property.node.getField(MemberKey), typeScope, propertyType)
            }
          }
        })
      }

    }
  }

  private def resolveRefs(builder: ConstraintBuilder, rootScope: ConcreteScope, program: ObjectLiteral[NodePath]): Unit = {
    program.visitShape(MemberShape, (_member: NodePath) => {
      val member: JsonObjectLiteralDelta.ObjectLiteralMember[NodePath] = _member
      if (member.key == "Ref" && member.value.shape == StringLiteralDelta.Shape) {
        val value = JsonStringLiteralDelta.getValue(member.value)
        val refLocation = member.value.getField(JsonStringLiteralDelta.Value)
        builder.resolveToType(value, refLocation, rootScope, valueType)
      }
    })
  }

  private def addPseudoParameters(builder: ConstraintBuilder, rootScope: ConcreteScope): Unit = {
    val pseudoParameters = Seq("AWS::AccountId", "AWS::NotificationARNs", "AWS::NoValue", "AWS::Partition", "AWS::Region", "AWS::StackId", "AWS::StackName", "AWS::URLSuffix")
    for (pseudoParameter <- pseudoParameters)
      builder.declare(pseudoParameter, rootScope, null, Some(valueType))
  }

  private def addParameters(builder: ConstraintBuilder, rootScope: ConcreteScope,
                            program: ObjectLiteral[NodePath]): Unit = {
    program.getObject("Parameters") match {
      case Some(_parameters) =>
        val parameters: ObjectLiteral[NodePath] = _parameters
        for (parameter <- parameters.members) {
          builder.declare(parameter.node.getField(MemberKey), rootScope, valueType)
        }
      case _ =>
    }
  }

  private def addResourceTypesFromSchema(resourceTypes: JsObject, builder: ConstraintBuilder, universe: ConcreteScope): Unit = {
    for (resourceType <- resourceTypes.value) {
      val typeDeclaration = builder.declare(resourceType._1, universe)
      val typeScope = builder.declareScope(typeDeclaration)

      val typeObject = resourceType._2.as[JsObject]
      val properties = typeObject.value("Properties").as[JsObject]
      for (property <- properties.value) {
        builder.declare(property._1, typeScope, null, Some(propertyType))
      }
    }
  }

  override def dependencies: Set[Contract] = Set.empty
}
