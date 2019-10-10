package deltas.verilog

import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.deltas.{Contract, DeltaWithGrammar}
import core.language.{Compilation, Language}
import core.language.node.{NodeField, NodeShape}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import deltas.javac.classes.skeleton.HasConstraintsDelta

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
    val packageDeclaration = builder.resolve(path.getValue(Package).asInstanceOf[String], path.getField(Package), parentScope)
    val importedScope = builder.getDeclaredScope(packageDeclaration)
    builder.importScope(parentScope, importedScope)
  }
}
