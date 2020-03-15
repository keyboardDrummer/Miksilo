package miksilo.modularLanguages.deltas.verilog

import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.modularLanguages.core.deltas.{Contract, DeltaWithGrammar}
import miksilo.modularLanguages.core.node.{NodeLike, NodeShape, NodeWrapper}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.modularLanguages.deltas.HasNameDelta.HasName
import miksilo.modularLanguages.deltas.javac.classes.skeleton.HasConstraintsDelta

object VerilogClassDelta extends DeltaWithGrammar with HasConstraintsDelta {
  override def description: String = "Adds Verilog classes"

  import miksilo.modularLanguages.deltas.HasNameDelta.Name
  object Shape extends NodeShape

  implicit class Class[T <: NodeLike](val node: T) extends NodeWrapper[T] with HasName[T] {
  }

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._

    val clazz = ("class" ~~ find(Name) ~ ";" % "endclass" ~~ (":" ~~ identifier).option) asNode Shape
    create(Shape, clazz)
  }


  override def dependencies: Set[Contract] = Set.empty

  override def shape: NodeShape = Shape

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
    val _clazz: Class[NodePath] = path
    builder.declare(_clazz.name, parentScope, _clazz.getField(Name))
  }
}
