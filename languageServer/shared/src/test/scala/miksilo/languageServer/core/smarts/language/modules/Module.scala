package miksilo.languageServer.core.smarts.language.modules

import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.language.structs.TypeDefinition
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import languageServer.SourcePath

trait FakeSourcePath extends SourcePath {

  override def uriOption: Option[String] = None

  override def rangeOption = None
}

case class Module(name: String, bindings: Seq[Binding], structs: Seq[TypeDefinition] = Seq.empty,
                  imports: Seq[ModuleImport] = Seq.empty) extends FakeSourcePath
{
  def constraints(builder: ConstraintBuilder, parentScope: Scope): Unit = {
    val moduleDeclaration = builder.declare(name, parentScope, this)
    val scope = builder.declareScope(moduleDeclaration, parentScope)
    structs.foreach(struct => struct.constraints(builder, scope))
    bindings.foreach(binding => binding.constraints(builder, scope))
    imports.foreach(_import => _import.constraints(builder, scope)) //TODO moet bovenaan
  }
}
