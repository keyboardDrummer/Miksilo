package core.smarts.language.modules

import core.language.SourceElement
import core.language.node.{FileRange, SourceRange}
import core.smarts.ConstraintBuilder
import core.smarts.language.structs.TypeDefinition
import core.smarts.scopes.objects.Scope

trait FakeSourceElement extends SourceElement {
  override def current: Any = ???

  override def range: Option[SourceRange] = None

  override def fileRange: Option[FileRange] = None
}

case class Module(name: String, bindings: Seq[Binding], structs: Seq[TypeDefinition] = Seq.empty,
                  imports: Seq[ModuleImport] = Seq.empty) extends FakeSourceElement
{
  def constraints(builder: ConstraintBuilder, parentScope: Scope): Unit = {
    val moduleDeclaration = builder.declare(name, parentScope, this)
    val scope = builder.declareScope(moduleDeclaration, Some(parentScope))
    structs.foreach(struct => struct.constraints(builder, scope))
    bindings.foreach(binding => binding.constraints(builder, scope))
    imports.foreach(_import => _import.constraints(builder, scope)) //TODO moet bovenaan
  }
}
