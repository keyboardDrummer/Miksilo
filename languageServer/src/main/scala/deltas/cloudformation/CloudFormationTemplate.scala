package deltas.cloudformation

import core.deltas.Delta
import core.deltas.path.PathRoot
import core.language.Language
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.ConcreteScope
import core.smarts.types.objects.PrimitiveType
import deltas.expression.StringLiteralDelta
import deltas.json.JsonObjectLiteralDelta
import deltas.json.JsonObjectLiteralDelta.{MemberKey, MemberShape, MemberValue, ObjectLiteral}
import play.api.libs.json.{JsObject, Json}
import util.SourceUtils

object CloudFormationTemplate extends Delta {
  override def description: String = "Add cloudformation template semantics"

  def inQuotes(value: String) = "\"" + value + "\""

  val propertyType = PrimitiveType("PropertyKey")
  val valueType = PrimitiveType("Value")
  override def inject(language: Language): Unit = {

    val file = SourceUtils.getTestFileContents("CloudFormationResourceSpecification.json")
    val parsedFile = Json.parse(file).as[JsObject]
    val resourceTypes = parsedFile.value("ResourceTypes").as[JsObject]

    language.collectConstraints = (compilation, builder) => {
      val rootScope = builder.newScope(debugName = "universe")

      addResouceTypesFromSchema(resourceTypes, builder, rootScope)

      val program: ObjectLiteral = compilation.program

      addPseudoParameters(builder, rootScope)
      addParameters(builder, rootScope, program)

      val resources: ObjectLiteral = program.getValue("Resources")
      for(resource <- resources.members) {
        builder.declare(resource.keyWithQuotes, rootScope, resource.node.getLocation(MemberKey), Some(valueType))

        val resourceMembers: ObjectLiteral = resource.value
        val typeString = resourceMembers.getValue("Type")
        val resourceType = StringLiteralDelta.getValue(typeString)
        val typeDeclaration = builder.resolve(resourceType, typeString.getLocation(StringLiteralDelta.Value), rootScope)
        val typeScope = builder.getDeclaredScope(typeDeclaration)

        val properties: ObjectLiteral = resourceMembers.getValue("Properties")
        for(property <- properties.members) {
          builder.resolveToType(property.keyWithQuotes, property.node.getLocation(MemberKey), typeScope, propertyType)
        }
      }

      PathRoot(program).visitShape(MemberShape, _member => {
        val member: JsonObjectLiteralDelta.ObjectLiteralMember = _member.current
        if (member.key == "Ref") {
          val value = StringLiteralDelta.getValue(member.value)
          builder.resolveToType(inQuotes(value), _member.getLocation(MemberValue), rootScope, valueType)
        }
      })

    }
    super.inject(language)
  }

  private def addPseudoParameters(builder: ConstraintBuilder, rootScope: ConcreteScope) = {
    val pseudoParameters = Seq("AWS::AccountId", "AWS::NotificationARNs", "AWS::NoValue", "AWS::Partition", "AWS::Region", "AWS::StackId", "AWS::StackName", "AWS::URLSuffix")
    for (pseudoParameter <- pseudoParameters)
      builder.declare(inQuotes(pseudoParameter), rootScope, null, Some(valueType))
  }

  private def addParameters(builder: ConstraintBuilder, universe: ConcreteScope, program: ObjectLiteral) = {
    val parameters: ObjectLiteral = program.getValue("Parameters")
    for (parameter <- parameters.members) {
      builder.declare(parameter.keyWithQuotes, universe, parameter.node.getLocation(MemberKey), Some(valueType))
    }
  }

  private def addResouceTypesFromSchema(resourceTypes: JsObject, builder: ConstraintBuilder, universe: ConcreteScope) = {
    for (resourceType <- resourceTypes.value) {
      val typeDeclaration = builder.declare(resourceType._1, universe)
      val typeScope = builder.declareScope(typeDeclaration)

      val typeObject = resourceType._2.as[JsObject]
      val properties = typeObject.value("Properties").as[JsObject]
      for (property <- properties.value) {
        builder.declare(inQuotes(property._1), typeScope, null, Some(propertyType))
      }
    }
  }
}
