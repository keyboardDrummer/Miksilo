package deltas.verilog

import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.deltas.{Contract, DeltaWithGrammar}
import core.language.{Compilation, Language}
import core.language.node.{NodeField, NodeShape}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import deltas.javac.classes.skeleton.HasConstraintsDelta

object VerilogClassDelta extends DeltaWithGrammar with HasConstraintsDelta {
  override def description: String = "Adds Verilog classes"

  object Shape extends NodeShape
  object Name extends NodeField
  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._

    val clazz = ("class" ~~ identifier.as(Name) ~ ";" % "endclass" ~~ (":" ~~ identifier).option) asNode Shape
    create(Shape, clazz)
  }


  override def dependencies: Set[Contract] = Set.empty

  override def shape: NodeShape = Shape

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {

  }
}
