package deltas.cloudformation

import core.deltas.Delta
import core.deltas.path.PathRoot
import core.language.Language
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
      val universe = builder.newScope(debugName = "universe")

      for(resourceType <- resourceTypes.value) {
        val typeDeclaration = builder.declare(resourceType._1, universe)
        val typeScope = builder.declareScope(typeDeclaration)

        val typeObject = resourceType._2.as[JsObject]
        val properties = typeObject.value("Properties").as[JsObject]
        for(property <- properties.value) {
          builder.declare(inQuotes(property._1), typeScope, null, Some(propertyType))
        }
      }

      val program: ObjectLiteral = compilation.program

      val parameters: ObjectLiteral = program.getValue("Parameters")
      for(parameter <- parameters.members) {
        builder.declare(parameter.keyWithQuotes, universe, parameter.node.getLocation(MemberKey), Some(valueType))
      }

      val resources: ObjectLiteral = program.getValue("Resources")
      for(resource <- resources.members) {
        builder.declare(resource.key, universe, resource.node.getLocation(MemberKey))

        val resourceMembers: ObjectLiteral = resource.value
        val typeString = resourceMembers.getValue("Type")
        val resourceType = StringLiteralDelta.getValue(typeString)
        val typeDeclaration = builder.resolve(resourceType, typeString.getLocation(StringLiteralDelta.Value), universe)
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
          builder.resolveToType(inQuotes(value), _member.getLocation(MemberValue), universe, valueType)
        }
      })

    }
    super.inject(language)
  }
}
