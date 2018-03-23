package deltas.cloudformation

import core.deltas.Delta
import core.language.node.SourceRange
import core.language.{Language, SourceElement}
import deltas.expression.StringLiteralDelta
import deltas.json.JsonObjectLiteralDelta
import deltas.json.JsonObjectLiteralDelta.{MemberKey, MemberShape, MemberValue, ObjectLiteral}
import langserver.types.Position
import play.api.libs.json.{JsObject, Json}
import util.SourceUtils

object CloudFormationTemplate extends Delta {
  override def description: String = "Add cloudformation template semantics"

  override def inject(language: Language): Unit = {

    val file = SourceUtils.getTestFileContents("CloudFormationResourceSpecification.json")
    val parsedFile = Json.parse(file).as[JsObject]
    val resourceTypes = parsedFile.value("ResourceTypes").as[JsObject]

    language.collectConstraints = (compilation, builder) => {
      val universe = builder.newScope()

      for(resourceType <- resourceTypes.value) {
        val typeDeclaration = builder.declare(resourceType._1, universe)
        val typeScope = builder.declareScope(typeDeclaration)

        val typeObject = resourceType._2.as[JsObject]
        val properties = typeObject.value("Properties").as[JsObject]
        for(property <- properties.value) {
          builder.declare(property._1, typeScope)
        }
      }

      val program: ObjectLiteral = compilation.program

      val parameters: ObjectLiteral = program.getValue("Parameters")
      for(parameter <- parameters.members) {
        builder.declare(parameter.key, universe, parameter.node.getLocation(MemberKey))
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
          builder.resolve(property.key, KeyLocation(property.node.getLocation(MemberKey)), typeScope)
        }
      }

      program.visitShape(MemberShape, _member => {
        val member: JsonObjectLiteralDelta.ObjectLiteralMember = _member
        if (member.key == "Ref") {
          val value = StringLiteralDelta.getValue(member.value)
          builder.resolve(value, _member.getLocation(MemberValue), universe)
        }
      })

    }
    super.inject(language)
  }

  case class KeyLocation(original: SourceElement) extends SourceElement {
    override def current: Any = original.current

    override def position: Option[SourceRange] = original.position.map(range => SourceRange(Position(range.start.line, range.start.character + 1), Position(range.end.line, range.end.character - 1)))
  }
}
