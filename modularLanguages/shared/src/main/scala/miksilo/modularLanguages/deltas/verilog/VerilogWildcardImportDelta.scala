package miksilo.modularLanguages.deltas.verilog

import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.modularLanguages.core.deltas.{Contract, DeltaWithGrammar}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.core.node.{NodeField, NodeShape}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.modularLanguages.deltas.javac.classes.skeleton.HasConstraintsDelta

object VerilogWildcardImportDelta extends DeltaWithGrammar with HasConstraintsDelta {

  object Shape extends NodeShape
  object Package extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val member = find(VerilogModuleDelta.MemberShape)
    val _import = "import" ~~> (identifier.as(Package) ~ "::" ~ "*" asNode Shape) ~< ";"
    member.addAlternative(_import)
  }

  override def description: String = "Adds Verilog wildcard import to the language"

  override def dependencies: Set[Contract] = Set(VerilogModuleDelta)

  override def shape: NodeShape = Shape

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
    val packageDeclaration = builder.resolve(path.getValue(Package).asInstanceOf[String], parentScope, path.getField(Package))
    val importedScope = builder.getDeclaredScope(packageDeclaration)
    builder.importScope(parentScope, importedScope)
  }
}
