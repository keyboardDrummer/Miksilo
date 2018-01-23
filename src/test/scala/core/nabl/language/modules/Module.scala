package core.nabl.language.modules

import core.nabl.constraints.ConstraintBuilder
import core.nabl.constraints.scopes.objects.Scope
import core.nabl.language.structs.TypeDefinition

case class Module(name: String, bindings: Seq[Binding], structs: Seq[TypeDefinition] = Seq.empty, imports: Seq[ModuleImport] = Seq.empty)
{
  def constraints(builder: ConstraintBuilder, parentScope: Scope): Unit = {
    val moduleDeclaration = builder.declaration(name, this, parentScope)
    val scope = builder.declaredNewScope(moduleDeclaration, Some(parentScope))
    structs.foreach(struct => struct.constraints(builder, scope))
    bindings.foreach(binding => binding.constraints(builder, scope))
    imports.foreach(_import => _import.constraints(builder, scope)) //TODO moet bovenaan
  }
}
