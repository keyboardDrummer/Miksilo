package deltas.cloudformation

import core.deltas.path.NodePath
import core.deltas.{Contract, Delta}
import core.language.Language
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.ConcreteScope
import core.smarts.types.objects.PrimitiveType
import deltas.json.JsonObjectLiteralDelta.{MemberKey, MemberShape, ObjectLiteral}
import deltas.json.{JsonObjectLiteralDelta, JsonStringLiteralDelta}
import play.api.libs.json.{JsObject, Json}
import util.SourceUtils

object CloudFormationTemplate extends Delta {
  override def description: String = "Add cloudformation template semantics"

  val propertyType = PrimitiveType("PropertyKey")
  val valueType = PrimitiveType("Value")
  override def inject(language: Language): Unit = {
    super.inject(language)

    val file = SourceUtils.getTestFileContents("CloudFormationResourceSpecification.json")
    val parsedFile = Json.parse(file).as[JsObject]
    val resourceTypes = parsedFile.value("ResourceTypes").as[JsObject]

    language.collectConstraints = (compilation, builder) => {
      val rootScope = builder.newScope(debugName = "universe")

      addResourceTypesFromSchema(resourceTypes, builder, rootScope)

      val program: ObjectLiteral[NodePath] = compilation.root

      addPseudoParameters(builder, rootScope)
      addParameters(builder, rootScope, program)

      val resources: ObjectLiteral[NodePath] = program.getValue("Resources")
      for(resource <- resources.members) {
        builder.declare(resource.key, rootScope, resource.getLocation(MemberKey), Some(valueType))

        val resourceMembers: ObjectLiteral[NodePath] = resource.value
        val typeString = resourceMembers.getValue("Type")
        val resourceType = JsonStringLiteralDelta.getValue(typeString)
        val typeDeclaration = builder.resolve(resourceType, typeString.getLocation(JsonStringLiteralDelta.Value), rootScope)
        val typeScope = builder.getDeclaredScope(typeDeclaration)

        val properties: ObjectLiteral[NodePath] = resourceMembers.getValue("Properties")
        for(property <- properties.members) {
          builder.resolveToType(property.key, property.node.getLocation(MemberKey), typeScope, propertyType)
        }
      }

      program.visitShape(MemberShape, (_member: NodePath) => {
        val member: JsonObjectLiteralDelta.ObjectLiteralMember[NodePath] = _member
        if (member.key == "Ref") {
          val value = JsonStringLiteralDelta.getValue(member.value)
          val refLocation = member.value.getLocation(JsonStringLiteralDelta.Value)
          builder.resolveToType(value, refLocation, rootScope, valueType)
        }
      })
    }
  }

  private def addPseudoParameters(builder: ConstraintBuilder, rootScope: ConcreteScope): Unit = {
    val pseudoParameters = Seq("AWS::AccountId", "AWS::NotificationARNs", "AWS::NoValue", "AWS::Partition", "AWS::Region", "AWS::StackId", "AWS::StackName", "AWS::URLSuffix")
    for (pseudoParameter <- pseudoParameters)
      builder.declare(pseudoParameter, rootScope, null, Some(valueType))
  }

  private def addParameters(builder: ConstraintBuilder, universe: ConcreteScope, program: ObjectLiteral[NodePath]): Unit = {
    val parameters: ObjectLiteral[NodePath] = program.getValue("Parameters")
    for (parameter <- parameters.members) {
      builder.declare(parameter.key, universe, parameter.node.getLocation(MemberKey), Some(valueType))
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

  override def dependencies: Set[Contract] = Set(JsonObjectLiteralDelta)
}
