package miksilo.modularLanguages.core.deltas.path

import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.objects.{DeclarationVariable, NamedDeclaration, Reference}
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.Type

object ConstraintBuilderExtension {
  implicit class ExtendedConstraintBuilder(builder: ConstraintBuilder) {

    def resolveToType(origin: AnyPath, scope: Scope, _type: Type) : DeclarationVariable = {
      builder.resolveToType(origin.current.asInstanceOf[String], origin, scope, _type)
    }

    def referSourceElement(hasName: AnyPath, scope: Scope): Reference = {
      builder.refer(hasName.current.asInstanceOf[String], scope, Some(hasName))
    }

    def declare(hasName: AnyPath, container: Scope, _type: Type): NamedDeclaration = {
      builder.declare(hasName.current.asInstanceOf[String], container, hasName, Some(_type))
    }

    def declare(name: AnyPath, container: Scope): NamedDeclaration = {
      builder.declare(name.current.asInstanceOf[String], container, name, None)
    }
  }
}
