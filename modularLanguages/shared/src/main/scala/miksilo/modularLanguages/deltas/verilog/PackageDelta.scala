package miksilo.modularLanguages.deltas.verilog

import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.modularLanguages.core.deltas.{Contract, DeltaWithGrammar}
import miksilo.editorParser.document.BlankLine
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.core.node.{NodeField, NodeLike, NodeShape, NodeWrapper}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.modularLanguages.deltas.classes.HasConstraintsDelta
import miksilo.modularLanguages.deltas.{ConstraintSkeleton, FileWithMembersDelta}

object PackageDelta extends DeltaWithGrammar with HasConstraintsDelta {

  import miksilo.modularLanguages.deltas.HasNameDelta.Name

  object Shape extends NodeShape
  object Members extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val fileMember = find(FileWithMembersDelta.Members)

    val packageMember = create(Members)
    val packageGrammar = "package" ~~ find(Name) ~ ";" %
      packageMember.manySeparatedVertical(BlankLine).as(Members) %
      "endpackage" ~~ (":" ~~ identifier).option
    fileMember.addAlternative(packageGrammar asNode Shape)

    val clazz = find(VerilogClassDelta.Shape)
    packageMember.addAlternative(clazz)
  }

  implicit class Package[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def name: String = node.getValue(Name).asInstanceOf[String]
    def members: Seq[T] = node(Members).asInstanceOf[Seq[T]]
  }

  override def description: String = "Adds packages to Verilog"

  override def dependencies: Set[Contract] = Set(FileWithMembersDelta, VerilogClassDelta)

  override def shape: NodeShape = Shape

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
    val _package: Package[NodePath] = path
    val packageDeclaration = builder.declare(_package.name, parentScope, path.getField(Name))
    val packageScope = builder.declareScope(packageDeclaration, debugName = "package")

    for(member <- _package.members) {
      ConstraintSkeleton.constraints(compilation, builder, member, packageScope)
    }
  }
}
