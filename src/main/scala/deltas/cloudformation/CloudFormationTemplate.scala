package deltas.cloudformation

import core.deltas.Delta
import core.language.Language
import deltas.expression.StringLiteralDelta
import deltas.json.JsonObjectLiteralDelta
import deltas.json.JsonObjectLiteralDelta.{MemberKey, MemberShape, MemberValue, ObjectLiteral}

object CloudFormationTemplate extends Delta {
  override def description: String = "Add cloudformation template semantics"

  override def inject(language: Language): Unit = {
    language.collectConstraints = (compilation, builder) => {
      val universe = builder.newScope()

      val program: ObjectLiteral = compilation.program

      val parameters: ObjectLiteral = program.getValue("Parameters")
      for(parameter <- parameters.members) {
        builder.declare(parameter.key, universe, parameter.node.getLocation(MemberKey))
      }

      val resources: ObjectLiteral = program.getValue("Resources")
      for(resource <- resources.members) {
        builder.declare(resource.key, universe, resource.node.getLocation(MemberKey))
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
}
